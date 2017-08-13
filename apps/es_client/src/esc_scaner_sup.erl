%%%-------------------------------------------------------------------
%% @doc esc_scaner 2th level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(esc_scaner_sup).

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
    io:format("esc_scaner_sup/init/1", []),
    Spec = {esc_scaner, {esc_scaner, start_link, _Args}, permanent, 2000, worker, [esc_scaner]},
    {ok, {{simple_one_for_one, 10, 100}, [Spec]}}.


%%====================================================================
%% Internal functions
%%====================================================================

