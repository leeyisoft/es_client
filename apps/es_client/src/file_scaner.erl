-module (file_scaner).

-behaviour(gen_server).

-export([start_link/1, scan_file/1]).
-export([stop/1]).

%% gen_server 回调函数
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("es_client.hrl").

stop(StopArgs)->
    io:format("我是子拥程~p stop StopArgs ~p ~n", [self(), StopArgs]),
    % [FileMd5|_] = StopArgs,
    Name2 = if
        is_list(StopArgs) ->
            list_to_atom(StopArgs);
        is_atom(StopArgs) ->
            StopArgs
    end,
    gen_server:call(Name2 , stop).

start_link(StartArgs) ->
    io:format("我是~p的子拥程 参数 ~p~n", [self(), StartArgs]),
    {?LogFileTable, FileMd5, _Position, _File, _Separator, _Multiline, _Keys, _Index} = StartArgs,
    gen_server:start_link({local, list_to_atom(FileMd5)}, ?MODULE, StartArgs, []).

init(InitArgs) ->
    % 注意，如果想让 terminate/2 在应用程序停止时被调用，
    % 就必须设置 trap_exit = true
    process_flag(trap_exit, true),

    Pid = self(),
    io:format("我是子拥程~p init InitArgs ~p ~n", [Pid, InitArgs]),

    gen_server:cast(Pid, InitArgs),
    {ok, InitArgs}.

handle_call(stop, _From, _State) ->
    io:format("我是~p的子拥程 handle_call stop _From ~p State~p~n", [self(), _From, _State]),
    {stop, normal};
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
    {?LogFileTable, FileMd5, _Position, File, Separator, Multiline, Keys, Index} = Item,
    case file:open(File, read) of
        {ok, Fd} ->
            % 从数据库读取
            Position = func:get_last_position(FileMd5),
            % io:format("我是子拥程~p scan_file Position ~p ~n", [self(), Position]),
            % 把文件指针移到上次读取后的位置
            file:position(Fd, {bof, Position}),

            % 循环读取文件
            Res = loop_read_file_line(Fd),
            file:close(Fd),
            case Res of
                {eof, Position2} ->
                    Data = #?LogFileTable{
                        name_md5=FileMd5,
                        last_position=Position2,
                        multiline=Multiline, % Multiline 为 false 的表示单行匹配; 为 list 正则表达式
                        separator=Separator, % Separator 为 "" 的标示为json格式数据
                        keys=Keys,
                        index=Index,
                        file=File
                    },
                    func:save_logfile(Data),
                    % 休眠5s
                    timer:sleep(5000),
                    % eof 的情况下，再次循环文件
                    scan_file(Data)
            end,
            {ok, Res};
        {error, Why} ->
            {error, Why}
    end.

%% private

%%
%% 循环的追行读取文件
%%
loop_read_file_line(Fd) ->
    Line = file:read_line(Fd),
    % 相对当前位置的偏移量；0 表示当前指针位置
    % 放在 file:read_line/1 后面，获取当前指针位置
    {ok, Position} = file:position(Fd, {cur, 0}),

    case Line of
        {ok, Line2} ->
            Md5 = func:md5(Line2),
            io:format("Position ~p md5 ~p : ~p~n", [Position, Md5, Line2]),

            loop_read_file_line(Fd);
        "\n" ->
            loop_read_file_line(Fd);
        eof ->
            % io:format("Position ~p: ~p~n", [Position, Line]),
            {eof, Position};
        {error, Reason} ->
            {error, Reason, Position}
    end.


    % Sig = erlang:md5(Line),

    % RowId = iolist_to_binary([io_lib:format("~2.16.0b", [S2]) || S2 <- binary_to_list(Sig)]),

    % erlastic_search:index_doc_with_id(<<"lee_index">>, <<"doc">>, RowId, jsx:decode(list_to_binary(Line)))
