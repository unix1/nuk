%%%-------------------------------------------------------------------
%% @doc nuk game supervisor
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/0]).

%% Behavior callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Behavior callbacks
%% ===================================================================

init([]) ->
    MaxRestart = 1,
    MaxTime = 3600,
    ChildSpec = {nuk_game_server,
                 {nuk_game_server, start_link, []},
                 permanent,
                 5000, % shutdown time
                 worker,
                 [nuk_game_server]},
    {ok, {{simple_one_for_one, MaxRestart, MaxTime}, [ChildSpec]}}.
