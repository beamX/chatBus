%%%-------------------------------------------------------------------
%% @doc chatbus top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('chatbus_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


-define(CHILD(Id, Mod, Args, Restart, Type), {Id, {Mod, start_link, Args},
                                              Restart, 60000, Type, [Mod]}).

-define(SIMPLE_CHILD(WorkerMod), ?CHILD(WorkerMod, WorkerMod, [], transient,
                                        worker)).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 10, 60}, [?SIMPLE_CHILD(bus_manager)]} }.

%%====================================================================
%% Internal functions
%%====================================================================
