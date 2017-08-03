%%%-------------------------------------------------------------------
%% @doc es_client public API
%% @end
%%%-------------------------------------------------------------------

-module (file_scaner).

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

start_link(StartArgs) ->
    io:format("我是~p的子拥程 参数 ~p~n", [self(), StartArgs]),
    {FileMd5, _Position, _File, _Separator, _Multiline, _Keys, _Index} = StartArgs,
    gen_server:start_link({local, list_to_atom(FileMd5)}, ?MODULE, StartArgs, []).

init(InitArgs) ->
    % 注意，如果想让 terminate/2 在应用程序停止时被调用，
    % 就必须设置 trap_exit = true
    % process_flag(trap_exit, true),

    Pid = self(),
    io:format("我是子拥程~p init InitArgs ~p ~n", [Pid, InitArgs]),

    gen_server:cast(Pid, InitArgs),
    {ok, InitArgs}.

handle_call(stop, _From, _State) ->
    io:format("我是~p的子拥程 handle_call stop _From ~p State~p~n", [self(), _From, _State]),
    {stop, normal, _State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    Pid = self(),
    % io:format("我是子拥程~p Msg ~p ~n", [Pid, Msg]),
    % io:format("我是子拥程~p State ~p ~n", [Pid, State]),

    Res = scan_file(Msg),
    io:format("我是子拥程~p scan_file Res ~p ~n", [Pid, Res]),
    {noreply, State, Res}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("~p stopping Pid ~p ~n", [?MODULE, self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

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

scan_file(Item) ->
    {FileMd5, _Position, File, Separator, Multiline, Keys, Index} = Item,
    case file:open(File, read) of
        {ok, Fd} ->
            % 从数据库读取
            Position = esc_db:get_last_position(FileMd5),
            % 获取文件大小
            FSize = filelib:file_size(File),

            NewPosition = new_position(Fd, Position, FSize),
            % io:format("我是子拥程~p scan_file Position ~p ~n", [self(), Position]),

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
                    % 休眠5s
                    timer:sleep(5000),
                    % eof 的情况下，再次循环文件
                    Item2 = {FileMd5, Position2, File, Separator, Multiline, Keys, Index},
                    scan_file(Item2);
                _ ->
                    io:format("scan_file end Res ~p ~n", [Res])
            end,
            {ok, Res};
        {error, enoent} -> % 文件被重命名的时候会这样
            % 休眠11s
            timer:sleep(11000),
            scan_file(Item);
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
            RowId = func:md5(Line2),
            io:format("Position ~p RowId ~p ~n", [Position, RowId]),
            % io:format("Position ~p RowId ~p : ~p~n", [Position, RowId, Line2]),

            MsgData = str_to_json(Line2, Separator, Keys, File),
            sent_to_msg(Index, RowId, MsgData),

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
                    Rows = [func:esc_read_line(Fd, StartPoistion + Start, Offset) || {Start,Offset} <- lists:zip(ListA, List5)],
                    % 把记录转换成 erlastic_json() 类型的数据
                    DataList = [{func:md5(Str), str_to_json(Str, Separator, Keys, File)} || Str <- Rows],
                    % 发送数据到 es
                    [sent_to_msg(Index, RowId, MsgData) || {RowId, MsgData} <- DataList],

                    % 从新的位置继续读取
                    NewStartPoistion = lists:max(List2) + StartPoistion,
                    loop_read_file_multiline(NewStartPoistion, ReadLength, Param);
                {match, _} when FSize > (StartPoistion + ReadLength) -> % 只匹配一行记录
                    % io:format("~n not end StartPoistion ~p, 1 ReadLength ~p ~n", [StartPoistion, ReadLength]),
                    loop_read_file_multiline(StartPoistion, ReadLength * 3, Param);
                {match, _} -> % 最后一行了
                    MsgData = str_to_json(Binary, Separator, Keys, File),
                    % save to mnesia
                    RowId = func:md5(Binary),
                    sent_to_msg(Index, RowId, MsgData),
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
str_to_json(Str, Separator, Keys, File) ->
    try

        if
            Separator==[] ->
                jsx:decode(list_to_binary(Str));
            true ->
                Vals = func:esc_split(Str, Separator, all),
                kv_to_erlastic_json(Keys, Vals, File)
        end
    catch
        Exception:Reason ->
            io:format("~n catch str_to_json Separator: ~p, Keys: ~p, string: ~p~n~n~n", [Separator, Keys, Str]),
            io:format("Exception: ~p , Reason: ~p, ~n", [Exception, Reason]),
            % 可以把这里的错误也发送到 es 里面去
            {caught, Exception, Reason}
    end.

kv_to_erlastic_json(Keys, Vals, File) when is_list(Keys), is_list(Vals), length(Keys)>0, length(Vals)>0 ->
    KVList = func:esc_zip(Keys, Vals),
    % io:format("kv_to_erlastic_json Vals: ~p~n", [Vals]),
    % io:format("kv_to_erlastic_json KVList: ~p~n", [KVList]),
    Items = lists:flatten([format_k_v(Key, Val) || {Key, Val} <- KVList]),
    Items2 = [{"file", File} | Items],
    % io:format("kv_to_erlastic_json Items: ~p~n", [Items]),
    [{list_to_binary(X),list_to_binary(Y)} || {X,Y} <- [filter_k_v(Item) || Item <- Items2]].

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
            io:format("format_k_v/1 Item ~p~n", [Item]),
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
            VLi = func:esc_split(Val, Separator, all),
            List = func:esc_zip(KLi, VLi),
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
                    List2 = func:esc_zip(KeyList, List),
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


sent_to_msg(Index, MsgMd5, Msg) ->
    try
        % io:format("try sent_to_msg/3 index: ~p, md5: ~p , msg: ~p~n", [Index, MsgMd5, Msg])
        % ok
        erlastic_search:index_doc_with_id(list_to_binary(Index), <<"doc">>, MsgMd5, Msg)
    catch
        Exception:Reason ->
            io:format("sent_to_msg/3 index: ~p, md5: ~p , msg: ~p~n", [Index, MsgMd5, Msg]),
            io:format("Exception: ~p , Reason: ~p, ~n", [Exception, Reason]),
            {caught, Exception, Reason}
    end.
