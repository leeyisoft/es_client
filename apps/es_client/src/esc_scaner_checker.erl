-module (esc_scaner_checker).

-export([start_link/0]).

-export([init_it/1]).


%% Internals.

start_link() ->
    proc_lib:start_link(?MODULE
        , init_it
        , [self()]
    ).

init_it(Parent) ->
    try
        init(Parent)
    catch
        _:normal -> exit(normal);
        _:shutdown -> exit(shutdown);
        _:Reason = {shutdown, _} -> exit(Reason);
        _:Reason -> exit({Reason, erlang:get_stacktrace()})
    end.

init(Parent) ->
    CurPid = self(),
    register(?MODULE, CurPid),
    % io:format("esc_scaner_checker/init/2 list pid ~p , Parent ~p~n", [CurPid, Parent]),
    ok = proc_lib:init_ack(Parent, {ok, CurPid}),
    receive
        {check_scaner, List} ->
            check_scaner(List);
        Other ->
            io:format("Unsupported messages for esc_scaner_checker msg: ~p.~n", [Other])
    end.


%%====================================================================
%% Internal functions
%%====================================================================

check_scaner(List) when is_list(List) ->
    % io:format("check_scaner/1 list pid ~p , List ~p~n", [self(), List]),
    [check_scaner(FileMd5) || FileMd5 <- List],

    % 休眠3s
    timer:sleep(2000),
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
