-module(nuk_lobby_sup).

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
    ChildSpec = {nuk_lobby_server,
                 {nuk_lobby_server, start_link, []},
                 permanent,
                 5000, % shutdown time
                 worker,
                 [nuk_lobby_server]},
    {ok, {{simple_one_for_one, MaxRestart, MaxTime}, [ChildSpec]}}.
