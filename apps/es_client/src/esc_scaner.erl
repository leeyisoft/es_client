%%%-------------------------------------------------------------------
%% @doc es_client public API
%% @end
%%%-------------------------------------------------------------------

-module (esc_scaner).

-behaviour(gen_server).

-export([start_link/1]).

%% gen_server 回调函数
-export([init/1
    , handle_call/3
    , handle_cast/2
    , handle_info/2
    , terminate/2
    , code_change/3
]).

% for test
-compile(export_all).


%%====================================================================
%% API
%%====================================================================

start_link({Name, StartArgs}) ->
    gen_server:start_link({local, Name}, ?MODULE, StartArgs, []);

start_link({FileMd5, _File, _Separator, _Multiline, _Keys, _Index} = StartArgs) ->
    io:format("我是esc sup: ~p 参数 ~p~n", [self(), StartArgs]),
    gen_server:start_link({local, FileMd5}, ?MODULE, StartArgs, []).

init(InitArgs) ->
    % 注意，如果想让 terminate/2 在应用程序停止时被调用，
    % 就必须设置 trap_exit = true
    % process_flag(trap_exit, true),

    Pid = self(),
    io:format("esc_scaner ~p init InitArgs ~p ~n", [Pid, InitArgs]),

    gen_server:cast(Pid, InitArgs),
    {ok, InitArgs}.

handle_call(stop, _From, _State) ->
    % io:format("我是 scaner ~p handle_call stop _From ~p State~p~n", [self(), _From, _State]),
    {stop, normal, _State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast({check_list, List}, State) when length(List)>0 ->
    % io:format("check_list pid ~p, list ~p, State ~p~n", [self(), List, State]),
    check_scaner(List),
    {noreply, State};
handle_cast(Msg, State) when is_tuple(Msg) ->
    Pid = self(),
    % io:format("handle_cast pid ~p, Msg ~p ~n", [Pid, Msg]),
    % io:format("我是子拥程~p State ~p ~n", [Pid, State]),
    Res = scan_file(Msg),
    % io:format("我是子拥程~p scan_file Res ~p ~n", [Pid, Res]),
    {noreply, State, Res};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    Pid = self(),
    io:format("我是scaner ~p handle_info/2 _Info ~p, State ~p ~n", [Pid, _Info, State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("~p stopping Pid ~p ~n", [?MODULE, self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

check_scaner(List) when is_list(List) ->
    % io:format("check_scaner/1 list pid ~p, Num ~p , List ~p~n", [self(), Num, List]),
    [check_scaner(FileMd5) || FileMd5 <- List],

    % 休眠3s
    timer:sleep(3000),
    check_scaner(List);

check_scaner(Name) when is_atom(Name) ->
    Pid = whereis(Name),
    {status, Status} = process_info(Pid,status),
    % io:format("pid ~p, status ~p ~n", [Pid, Status]),
    if
        Status =:= waiting ->
            % io:format("Status =:= waiting pid ~p, status ~p ~n", [Pid, Status]),
            % 唤醒 hibernate 的进程
            Item = sys:get_state(Name, infinity),
            gen_server:cast(Pid, Item);
        true ->
            ok
    end.


%% 该函数功能是判断文件是否被重置 Nginx日志会被定期切割，被切割了之后，应该重新读取文件
%% 修改日志内容多余400字符 应该被重置
%% 重中间修改少于400字符的内容，为报异常
%% 修改日志内容少于余400字符（在文件末尾修改）会重定位到文件末尾
%% 修改文件内容多余400字符会重新读取文件
new_position(Fd, Position, FSize) ->
    if
        Position =< FSize ->
            % 把文件指针移到上次读取后的位置
            file:position(Fd, {bof, Position}),
            Position;
        (Position - FSize) =< 400  ->
            % 把文件指针移到文件末尾
            file:position(Fd, {eof, 0}),
            FSize;
        true ->
            % 把文件指针移到文件开始的文章
            file:position(Fd, {bof, 0}),
            0
    end.

scan_file({FileMd5, File, Separator, Multiline, Keys, Index}) ->
    case file:open(File, read) of
        {ok, Fd} ->
            % 从数据库读取
            Position = esc_db:get_last_position(FileMd5),
            % 获取文件大小
            FSize = filelib:file_size(File),

            NewPosition = new_position(Fd, Position, FSize),
            % io:format("Pid ~p scan_file Position ~p ~n", [self(), Position]),

            Res = if
                Multiline==false -> % 循环读取文件行
                    loop_read_file_line(Fd, Separator, Keys, Index, File);
                true ->
                    Param = {File, Separator, Multiline, Keys, Index, Fd, FSize},
                    loop_read_file_multiline(NewPosition, 400, Param)
            end,
            file:close(Fd),
            case Res of
                {eof, Position2} ->
                    esc_db:save_logfile(FileMd5, Position2),
                    hibernate;
                _ ->
                    io:format("scan_file end Res ~p ~n", [Res]),
                {ok, Res}
            end;
        {error, enoent} -> % enoent 表示文件不存在了，文件被重命名的时候会这样
            % 下次需要重头开始读取文件
            esc_db:save_logfile(FileMd5, 0),
            hibernate;
        {error, Why} ->
            io:format("file:open/2 error: ~p ~n", [Why]),
            {error, Why}
    end.

%%
%% 循环的追行读取文件
%%
loop_read_file_line(Fd, Separator, Keys, Index, File) ->
    Line = file:read_line(Fd),
    % 相对当前位置的偏移量；0 表示当前指针位置
    % 放在 file:read_line/1 后面，获取当前指针位置
    {ok, Position} = file:position(Fd, {cur, 0}),

    case Line of
        {ok, "\n"} ->
            loop_read_file_line(Fd, Separator, Keys, Index, File);
        {ok, Line2} ->
            RowId = esc_func:md5(Line2),
            % io:format("Position ~p RowId ~p ~n", [Position, RowId]),

            % {status, Status} = process_info(self(),status),
            % io:format("loop_read_file_line/5 pid ~p, status ~p ~n", [self(), Status]),
            % io:format("Position ~p RowId ~p : ~p~n", [Position, RowId, Line2]),

            MsgData = str_to_json(Line2, Separator, Keys),
            sent_to_msg(Index, RowId, MsgData, File),

            loop_read_file_line(Fd, Separator, Keys, Index, File);
        eof ->
            % io:format("Position ~p: ~p~n", [Position, Line]),
            {eof, Position};
        {error, Reason} ->
            {error, Reason, Position}
    end.

loop_read_file_multiline(StartPoistion, ReadLength, Param) ->
    {File, Separator, Re, Keys, Index, Fd, FSize} = Param,
    case file:pread(Fd, StartPoistion, ReadLength) of
        {ok, Binary} ->
            {ok,MP} = re:compile(Re),

            case re:run(Binary, MP, [{capture,all,index},global]) of
                {match, [_Head|[_Head|Tail]]} when length(Tail)>20 ->
                    loop_read_file_multiline(StartPoistion, ReadLength - ReadLength div 2, Param);
                {match, List} when length(List)>1 -> % 至少两个记录
                    % io:format("~nBinary ~p: ~p~n", [StartPoistion, Binary]),
                    % io:format("~n StartPoistion : ~p, Len: ~p, List ~p,~n", [StartPoistion, length(List), List]),

                    % List2 = [0, 1, 3, 5, 7]
                    % ListA = [0, 1, 3, 5] % 每个记录的相对其实位置
                    % ListB = [1, 3, 5, 7]
                    % List5 = ListB - ListA % 每个记录的偏移量
                    % lists:zip(ListA, List5)

                    % List2 匹配结果的每个记录的其实位置
                    List2 = [Offset || [{Offset, _}] <- List],

                    % 去除最后一行记录，因为它很有可能不是一个完整的记录
                    List3 = lists:reverse(List2),
                    [_|List4] = List3,

                    % 每个记录的相对其实位置
                    ListA = lists:reverse(List4),

                    [_|ListB] = List2,

                    % 获取记录偏移量
                    List5 = [ B - A || {B, A} <- lists:zip(ListB, ListA) ],

                    % 计算每个记录的开始位置和长度, 根据每个记录的开始位置和长度读取记录
                    Rows = [esc_func:esc_read_line(Fd, StartPoistion + Start, Offset) || {Start,Offset} <- lists:zip(ListA, List5)],
                    % 把记录转换成 erlastic_json() 类型的数据
                    DataList = [{esc_func:md5(Str), str_to_json(Str, Separator, Keys)} || Str <- Rows],
                    % 发送数据到 es
                    [sent_to_msg(Index, RowId, MsgData, File) || {RowId, MsgData} <- DataList],

                    % 从新的位置继续读取
                    NewStartPoistion = lists:max(List2) + StartPoistion,
                    loop_read_file_multiline(NewStartPoistion, ReadLength, Param);
                {match, _} when FSize > (StartPoistion + ReadLength) -> % 只匹配一行记录
                    % io:format("~n not end StartPoistion ~p, 1 ReadLength ~p ~n", [StartPoistion, ReadLength]),
                    loop_read_file_multiline(StartPoistion, ReadLength * 3, Param);
                {match, _} -> % 最后一行了
                    MsgData = str_to_json(Binary, Separator, Keys),
                    % save to mnesia
                    RowId = esc_func:md5(Binary),
                    sent_to_msg(Index, RowId, MsgData, File),
                    % msg_sender:sent_to_es(Index, RowId, MsgData),

                    Len = length(Binary),
                    % io:format("~n end StartPoistion ~p, 1 ReadLength ~p 1 Len ~p Binary ~p ~n", [StartPoistion, ReadLength, Len, Binary]),

                    {eof, StartPoistion + Len};
                nomatch ->
                    loop_read_file_multiline(StartPoistion, ReadLength + ReadLength div 2, Param)
            end;
        eof ->
            % io:format("StartPoistion ~p: ~p~n", [StartPoistion, Line]),
            {eof, StartPoistion};
        {error, Reason} ->
            {error, Reason, StartPoistion}
    end.

%%
str_to_json(Str, Separator, Keys) ->
    try

        if
            Separator==[] ->
                jsx:decode(list_to_binary(Str));
            true ->
                Vals = esc_func:esc_split(Str, Separator, all),
                kv_to_erlastic_json(Keys, Vals)
        end
    catch
        Exception:Reason ->
            io:format("~n catch str_to_json Separator: ~p, Key, ~p, string: ~p~n", [Separator, Keys, Str]),
            io:format("Exception: ~p , Reason: ~p, ~n", [Exception, Reason]),
            % 可以把这里的错误也发送到 es 里面去
            {caught, Exception, Reason}
    end.

kv_to_erlastic_json(Keys, Vals) when is_list(Keys), is_list(Vals), length(Keys)>0, length(Vals)>0 ->
    KVList = esc_func:esc_zip(Keys, Vals),
    % io:format("kv_to_erlastic_json Vals: ~p~n", [Vals]),
    % io:format("kv_to_erlastic_json KVList: ~p~n", [KVList]),
    Items = lists:flatten([format_k_v(Key, Val) || {Key, Val} <- KVList]),
    % io:format("kv_to_erlastic_json Items: ~p~n", [Items]),
    [{list_to_binary(X),list_to_binary(Y)} || {X,Y} <- [filter_k_v(Item) || Item <- Items]].

%% 过滤无效字符串 "\" :\n\\"
filter_k_v(Item) ->
    FilterStr = "\" :\n\\",
    case Item of
        {Key1, Val1} ->
            {string:trim(Key1, both, FilterStr), string:trim(Val1, both, FilterStr)};
        [_H|_Tail] ->
            [filter_k_v(Item2) || Item2 <- lists:flatten(Item)];
        _ ->
            % 应该要把 Item 打印到日志文件里面，看看是什么东西
            esc_loger:log("format_k_v/1 Item ~p~n", [Item]),
            Item
    end.

%% 格式化数据，按照 keys 配置的元组转换数据类型
%% 返回的可能是 {K, V} 元组、空列表 []，也可能是 list [{K1,V1}, ...]
format_k_v(Key, Val) when is_tuple(Key) ->
    case Key of
        {name, Name, ip} ->
            Re = "\\d{1,3}.\\d{1,3}.\\d{1,3}.\\d{1,3}",
            {ok,MP} = re:compile(Re),
            {match,[[Ip]]} = re:run(Val, MP, [{capture,all, list},global]),
            {Name, Ip};
        {name, Name, datetime} ->
            Re = "\\d{4}[-|/]?\\d{2}[-|/]?\\d{2} \\d{2}:\\d{2}:\\d{2}",
            {ok,MP} = re:compile(Re),
            {match,[[DateTime]]} = re:run(Val, MP, [{capture,all, list},global]),
            {Name, DateTime};
        {name, Name, integer} ->
            {Name, list_to_integer(Val)};
        {name, Name, float} ->
            {Name, list_to_float(Val)};
        {name, Name, string} ->
            {Name, Val};

        {split, _Separator, KLi} when is_list(KLi), Val==[] ->
            [];
        {split, Separator, KLi} when is_list(KLi) ->
            VLi = esc_func:esc_split(Val, Separator, all),
            List = esc_func:esc_zip(KLi, VLi),
            lists:flatten([format_k_v(Key2, Val2) || {Key2, Val2} <- List]);
        {split, Separator, _} ->
            [_Key|Name] = string:split(Val, Separator),
            if
                Name==[] ->
                    [];
                true ->
                    format_k_v_split(Key, Val)
            end
    end.

format_k_v_split(Key, Val) ->
    case Key of
        {split, Separator, {split, Separator2, KeyList}} when is_list(KeyList) ->

            [KeyName|Val2] = string:split(Val, Separator),

            ValList = string:split(string:trim(Val2, both, Separator2), Separator2, all),
            List = lists:sublist(ValList, length(KeyList)),

            List3 = if
                List==[] ->
                    [];
                true ->
                    List2 = esc_func:esc_zip(KeyList, List),
                    [format_k_v(Key3, Val3) || {Key3, Val3} <- List2]
            end,

            [{KeyName, Val2} | List3];

        {split, _Separator, List} when is_list(List), Val==[]  ->
            [];
        {split, Separator, List} when is_list(List) ->
            List2 = fstring:split(Val, Separator),
            [format_k_v(Key2, Val2) || {Key2, Val2} <- List2];
        {split, Separator, ip} ->
            [KeyName|_ValLi] = string:split(Val, Separator),

            Re = "\\d{1,3}.\\d{1,3}.\\d{1,3}.\\d{1,3}",
            {ok,MP} = re:compile(Re),
            {match,[[Ip]]} = re:run(Val, MP, [{capture,all, list},global]),
            Val2 = if
                Ip==[] ->
                    _ValLi;
                true ->
                    Ip
            end,
            {KeyName, Val2};
        {split, Separator, datetime} ->
            [KeyName|_ValLi] = string:split(Val, Separator),

            Re = "\\d{4}[-|/]?\\d{2}[-|/]?\\d{2} \\d{2}:\\d{2}:\\d{2}",
            {ok,MP} = re:compile(Re),
            {match,[[DateTime]]} = re:run(Val, MP, [{capture,all, list},global]),

            {KeyName, DateTime};
        {split, Separator, string} ->
            [KeyName|Val2] = string:split(Val, Separator),
            {KeyName, Val2}
    end.


sent_to_msg(Index, MsgMd5, Msg, File) ->
    Index2 = filter_index_name(Index),
    Pid = self(),
    try
        Msg2 = case lists:keytake(<<"@timestamp">>, 1, Msg) of
            {value, {Key, Timestamp}, List} ->
                [{list_to_binary("file"), list_to_binary(File)}, {Key, list_to_binary(esc_func:timestamp_format(Timestamp))} | List];
            false ->
                Msg
        end,
        % io:format("try Pid ~p sent_to_msg/4 index: ~p, md5: ~p, Msg2: ~p ~n",
            % [Pid, Index2, MsgMd5, Msg2])
        io:format("try Pid ~p sent_to_msg/4 index: ~p, md5: ~p, Msg2: ~p ~n",
            [Pid, Index2, MsgMd5, Msg2]),
        erlastic_search:index_doc_with_id(list_to_binary(Index2), <<"log">>, MsgMd5, Msg2)
    catch
        Exception:Reason ->
            esc_loger:log("Exception: ~p , Reason: ~p, ~n", [Exception, Reason]),
            esc_loger:log("catch Pid ~p sent_to_msg/4 index: ~p, md5: ~p , msg: ~p~n", [Pid, Index2, MsgMd5, Msg]),
            {caught, Exception, Reason}
    end.

filter_index_name(Index) when is_list(Index) ->
    Re = "\\{[\\w|\\-|/]+\\}",
    case re:run(Index, Re, [global, {capture, all, list}]) of
        nomatch ->
            Index;
        {match, List} ->
            filter_index_name(Index, List)
    end.

filter_index_name(Index, List) when length(List)==1 ->
    [[Item|_]|_] = List,
    {{Year, Month, Day}, _} = calendar:local_time(),
    Ymd = case Item of
        "{Y-m}" ->
            lists:flatten(io_lib:format("~4..0w-~2..0w", [Year, Month]));
        "{Y-m-d}" ->
            lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0w", [Year, Month, Day]));
        "{Ym}" ->
            lists:flatten(io_lib:format("~4..0w~2..0w", [Year, Month]));
        "{Ymd}" ->
            lists:flatten(io_lib:format("~4..0w~2..0w~2..0w", [Year, Month, Day]));
        _ ->
            Item
    end,
    re:replace(Index, Item, Ymd, [global, {return, list}]);

filter_index_name(Index, [Item|Tail]=List) when length(List)>1 ->
    Index2 = filter_index_name(Index, [Item]),
    filter_index_name(Index2, Tail).

