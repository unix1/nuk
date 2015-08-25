%%%-------------------------------------------------------------------
%% @doc nuk user store supervisor
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user_store_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(Id, Module, Args, Type), {Id, {Module, start_link, Args},
        permanent, 5000, Type, [Module]}).

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
    {ok, { {one_for_one, 0, 1}, children()} }.

%%====================================================================
%% Internal functions
%%====================================================================

children() ->
    UserStore = ?CHILD(nuk_user_store_server, nuk_user_store_server, [], worker),
    [UserStore].