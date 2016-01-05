%%%-------------------------------------------------------------------
%% @doc `nuk_game_sup' module
%%
%% This supervisor is started by {@link nuk_sup} top level supervisor.
%%
%% Whenever a new game session is created, nuk spawns a new
%% {@link nuk_game_server}. This module is a `simple_one_for_one' supervisor
%% that supervises those servers.
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_sup).

-behaviour(supervisor).

%% Supervision
-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% Supervision
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    MaxRestart = 1,
    MaxTime = 3600,
    ChildSpec = {nuk_game_server,
                 {nuk_game_server, start_link, []},
                 temporary,
                 5000, % shutdown time
                 worker,
                 [nuk_game_server]},
    {ok, {{simple_one_for_one, MaxRestart, MaxTime}, [ChildSpec]}}.
