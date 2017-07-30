-module (msg_sender).

-behaviour(gen_server).

-export([start_link/1]).
-export([stop/1]).

%% gen_server 回调函数
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("es_client.hrl").

stop(StopArgs)->
    io:format("我是子拥程~p stop StopArgs ~p ~n", [self(), StopArgs]),
    % [FileMd5|_] = StopArgs,
    Name = if
        is_list(StopArgs) ->
            list_to_atom(StopArgs);
        is_atom(StopArgs) ->
            StopArgs
    end,
    gen_server:call({local, Name} , stop).

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
    {stop, normal, _State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(Msg, State) ->
    Pid = self(),
    io:format("我是子拥程~p Msg ~p ~n", [Pid, Msg]),
    io:format("我是子拥程~p State ~p ~n", [Pid, State]),
    {noreply, State, Msg}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("~p stopping Pid ~p ~n", [?MODULE, self()]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private

sent_to_es(Index, RowId, Data) ->
    erlastic_search:index_doc_with_id(list_to_binary(Index), <<"doc">>, RowId, Data).
