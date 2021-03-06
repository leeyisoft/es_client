%%%-------------------------------------------------------------------
%% @doc es_client top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(es_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(_Args) ->
    % simple_one_for_one 表明我们必须手动添加（通过 start_child）该进程的子进程。
    % 100s内重启10次
    % permanent 子进程总会被重启。
    % io:format("es_client_sup/init/1 _Args ~p~n", [_Args]),
    SpecChecker = {
        esc_scaner_checker
        , {esc_scaner_checker, start_link, []}
        , permanent
        , 2000
        , worker
        , [esc_scaner_checker]
    },
    SpecScaners = {
        esc_scaner_sup
        , {esc_scaner_sup, start_link, _Args}
        , permanent
        , 2000
        , supervisor
        , [esc_scaner_sup]
    },
    io:format("es_client_sup/init/1", []),
    {ok, {{one_for_one, 10, 100}, [
        SpecChecker,
        SpecScaners
    ]}}.


%%====================================================================
%% Internal functions
%%====================================================================

