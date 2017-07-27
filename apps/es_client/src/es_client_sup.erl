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
    Spec = {file_scaner, {file_scaner, start_link, _Args}, permanent, 2000, worker, [file_scaner]},
    {ok, {{simple_one_for_one, 10, 100}, [Spec]}}.


%%====================================================================
%% Internal functions
%%====================================================================

