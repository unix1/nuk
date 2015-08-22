%%%-------------------------------------------------------------------
%% @doc nuk top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(nuk_sup).

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
    %% TODO remove nuk_lobby_sup - replaced by user
    LobbySup = ?CHILD(nuk_lobby_sup, nuk_lobby_sup, [], supervisor),
    UserSup = ?CHILD(nuk_user_sup, nuk_user_sup, [], supervisor),
    UserStoreSup = ?CHILD(nuk_user_store_sup, nuk_user_store_sup, [], supervisor),
    GameSup = ?CHILD(nuk_game_sup, nuk_game_sup, [], supervisor),
    [UserSup, LobbySup, UserStoreSup, GameSup].
