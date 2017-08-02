-module (file_scaner).

-behaviour(gen_server).

-export([start_link/1, scan_file/1]).

%% gen_server 回调函数
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% for test
-compile(export_all).

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

scan_file(Item) ->
    {FileMd5, _Position, File, Separator, Multiline, Keys, Index} = Item,
    case file:open(File, read) of
        {ok, Fd} ->
            % 从数据库读取
            Position = esc_db:get_last_position(FileMd5),
            % io:format("我是子拥程~p scan_file Position ~p ~n", [self(), Position]),
            % 把文件指针移到上次读取后的位置
            file:position(Fd, {bof, Position}),

            Res = if
                Multiline==false -> % 循环读取文件行
                    loop_read_file_line(Fd, Separator, Keys, Index);
                true ->
                    FSize = filelib:file_size(File),
                    loop_read_file_multiline(Separator, Multiline, Keys, Index, Fd, Position, FSize, 400)
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
        {error, Why} ->
            {error, Why}
    end.

%% private

%%
%% 循环的追行读取文件
%%
loop_read_file_line(Fd, Separator, Keys, Index) ->
    Line = file:read_line(Fd),
    % 相对当前位置的偏移量；0 表示当前指针位置
    % 放在 file:read_line/1 后面，获取当前指针位置
    {ok, Position} = file:position(Fd, {cur, 0}),

    case Line of
        {ok, "\n"} ->
            loop_read_file_line(Fd, Separator, Keys, Index);
        {ok, Line2} ->
            RowId = func:md5(Line2),
            io:format("Position ~p RowId ~p : ~p~n", [Position, RowId, Line2]),

            MsgData = str_to_json(Line2, Separator, Keys),
            sent_to_msg_sender(Index, RowId, MsgData),

            loop_read_file_line(Fd, Separator, Keys, Index);
        eof ->
            % io:format("Position ~p: ~p~n", [Position, Line]),
            {eof, Position};
        {error, Reason} ->
            {error, Reason, Position}
    end.

loop_read_file_multiline(Separator, Re, Keys, Index, Fd, StartPoistion, FSize, ReadLength) ->
    case file:pread(Fd, StartPoistion, ReadLength) of
        {ok, Binary} ->
            {ok,MP} = re:compile(Re),

            case re:run(Binary, MP, [{capture,all,index},global]) of
                {match, [_Head|[_Head|Tail]]} when length(Tail)>20 ->
                    loop_read_file_multiline(Separator, Re, Keys, Index, Fd, StartPoistion, FSize, ReadLength - ReadLength div 2);
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
                    Rows = [read_line_for_lrfm(Fd, StartPoistion + Start, Offset) || {Start,Offset} <- lists:zip(ListA, List5)],
                    % 把记录转换成 erlastic_json() 类型的数据
                    DataList = [{func:md5(Str), str_to_json(Str, Separator, Keys)} || Str <- Rows],
                    % 发送数据到 msg_sender，以便于推送到 es
                    [sent_to_msg_sender(Index, RowId, MsgData) || {RowId, MsgData} <- DataList],

                    % 从新的位置继续读取
                    NewStartPoistion = lists:max(List2) + StartPoistion,
                    loop_read_file_multiline(Separator, Re, Keys, Index, Fd, NewStartPoistion, FSize, ReadLength);
                {match, _} when FSize > (StartPoistion + ReadLength) -> % 只匹配一行记录
                    % io:format("~n not end StartPoistion ~p, 1 ReadLength ~p ~n", [StartPoistion, ReadLength]),
                    NewLen = ReadLength * 3,
                    loop_read_file_multiline(Separator, Re, Keys, Index, Fd, StartPoistion, FSize, NewLen);
                {match, _} -> % 最后一行了
                    MsgData = str_to_json(Binary, Separator, Keys),
                    % save to mnesia
                    RowId = func:md5(Binary),
                    sent_to_msg_sender(Index, RowId, MsgData),
                    % msg_sender:sent_to_es(Index, RowId, MsgData),

                    Len = length(Binary),
                    % io:format("~n end StartPoistion ~p, 1 ReadLength ~p 1 Len ~p Binary ~p ~n", [StartPoistion, ReadLength, Len, Binary]),

                    {eof, StartPoistion + Len};
                nomatch ->
                    loop_read_file_multiline(Separator, Re, Keys, Index, Fd, StartPoistion, FSize, ReadLength + ReadLength div 2)
            end;
        eof ->
            % io:format("StartPoistion ~p: ~p~n", [StartPoistion, Line]),
            {eof, StartPoistion};
        {error, Reason} ->
            {error, Reason, StartPoistion}
    end.
% read_line_for_lrfm/3 的 List至少有一个记录
read_line_for_lrfm(Fd, StartPoistion, Len)->
    {ok, Row} = file:pread(Fd, StartPoistion, Len),
    Row.

%%
str_to_json(Str, Separator, Keys) ->
    try
        CheckSeparator = string:find(Separator, "["),
        if
            Separator==[] ->
                jsx:decode(list_to_binary(Str));
            CheckSeparator==nomatch ->
                Vals = string:split(Str, Separator, all),
                SubList = lists:sublist(Vals, length(Keys)),
                Items = [format_k_v(Key, Val) || {Key, Val} <- lists:zip(Keys, SubList)],
                [{list_to_binary(X),list_to_binary(Y)} || {X,Y} <- lists:flatten(Items)];
            true ->
                Vals = re:split(Str, Separator, [{return, list}]),
                SubList = lists:sublist(Vals, length(Keys)),
                Items = [format_k_v(Key, Val) || {Key, Val} <- lists:zip(Keys, SubList)],
                [{list_to_binary(X),list_to_binary(Y)} || {X,Y} <- lists:flatten(Items)]
        end
    catch
        Exception:Reason ->
            io:format("~n catch str_to_json Separator: ~p, Keys: ~p, string: ~p~n~n~n", [Separator, Keys, Str]),
            io:format("Exception: ~p , Reason: ~p, ~n", [Exception, Reason]),
            {caught, Exception, Reason}
    end.

%% 格式化数据，按照 keys 配置的元组转换数据类型
%% 返回的可能是 {K, V} 元组，也可能是 list [{K1,V1}, ...]
format_k_v(Key, Val) when is_tuple(Key) ->
    Item = case Key of
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

        {split, Separator, {split, Separator2, KeyList}} when is_list(KeyList) ->

            [KeyName|Val2] = string:split(Val, Separator),

            ValList = string:split(string:trim(Val2, both, Separator2), Separator2, all),
            List = lists:sublist(ValList, length(KeyList)),
            List2 = lists:zip(KeyList, List),

            [{KeyName, Val2} | List2];

        {split, Separator, ip} ->
            [KeyName|_ValLi] = string:split(Val, Separator),

            Re = "\\d{1,3}.\\d{1,3}.\\d{1,3}.\\d{1,3}",
            {ok,MP} = re:compile(Re),
            {match,[[Ip]]} = re:run(Val, MP, [{capture,all, list},global]),

            {KeyName, Ip};
        {split, Separator, datetime} ->
            [KeyName|_ValLi] = string:split(Val, Separator),

            Re = "\\d{4}[-|/]?\\d{2}[-|/]?\\d{2} \\d{2}:\\d{2}:\\d{2}",
            {ok,MP} = re:compile(Re),
            {match,[[DateTime]]} = re:run(Val, MP, [{capture,all, list},global]),

            {KeyName, DateTime};
        {split, Separator, string} ->
            [KeyName|Val2] = string:split(Val, Separator),
            {KeyName, Val2}
    end,
    format_k_v(Item).

%% 过滤无效字符串 \" :
format_k_v(Item) ->
    FilterStr = "\" :",
    case Item of
        {Key1, Val1} when is_list(Key1), is_list(Val1) ->
            {string:trim(Key1), string:trim(Val1, both, FilterStr)};
        {Key1, Val1} when is_list(Key1) ->

            {string:trim(Key1), Val1};
        {Key1, Val1} when is_list(Val1) ->
            {Key1, string:trim(Val1, both, FilterStr)};
        [_H|_Tail] ->
            [format_k_v(Item2) || Item2 <- Item];
        _ ->
            Item
    end.

sent_to_msg_sender(Index, MsgMd5, Msg) ->
    io:format("sent_to_msg_sender/ index: ~p, md5: ~p , msg: ~p~n", [Index, MsgMd5, Msg]),
    msg_sender:sent_to_es(Index, MsgMd5, Msg),
    ok.
