-module (file_scaner).

-behaviour(gen_server).

-export([start_link/1]).
-export([stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

stop(Name)->
    Name2 = if
        is_list(Name) ->
            list_to_atom(Name);
        is_atom(Name) ->
            Name
    end,
    gen_server:call(Name2 , {stop}).

start_link(Name) when is_list(Name) ->
    io:format("我是~p的子拥程~n", [self()]),
    gen_server:start_link( {local, list_to_atom(Name)}, ?MODULE, Name, []).

init(_Args) ->
    Pid = self(),
    io:format("我是子拥程~p init _Args ~p ~n", [Pid, _Args]),
    {ok, _Args}.

handle_call( {stop } , _From, State) ->
    { stop , normal , ok , State };
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    % Pid = self(),
    % io:format("我是子拥程~p _Msg ~p ~n", [Pid, _Msg]),
    % io:format("我是子拥程~p State ~p ~n", [Pid, State]),
    [File, Multiline, Separator] = _Msg,
    handle_file_json(File, Multiline, Separator),
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% private

handle_file_json(File, Multiline, Separator) ->
    case file:open(File, read) of
        {ok, S} ->
            % 从数据库读取 LineNum
            LineNum = 1,
            % 循环读取文件
            LastLineNum = loop_read_file_json(S, LineNum),
            file:close(S),
            % 保存 LastLineNum
            % todo
            {ok, LastLineNum};
        {error, Why} ->
            {error, Why}
    end.

%%
loop_read_file_json(S, LineNum) ->
    Line = io:get_line(S, ''),

    case Line of
        eof ->
            % io:format("num ~p: ~p~n", [LineNum, Line]),
            LineNum;
        "\n" ->
            loop_read_file_json(S, LineNum);
        {error, _Reason} ->
            {error, _Reason};
        _ ->
            Md5 = func:md5(Line),
            io:format("num ~p ~p : ~p~n", [LineNum, Md5, Line]),

            loop_read_file_json(S, LineNum+1)
    end.


    % Sig = erlang:md5(Line),

    % RowId = iolist_to_binary([io_lib:format("~2.16.0b", [S2]) || S2 <- binary_to_list(Sig)]),

    % erlastic_search:index_doc_with_id(<<"lee_index">>, <<"doc">>, RowId, jsx:decode(list_to_binary(Line)))
