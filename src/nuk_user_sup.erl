%%%-------------------------------------------------------------------
%% @doc `nuk_user_sup' module
%%
%% This supervisor is started by {@link nuk_sup} top level supervisor.
%%
%% Whenever a user logs in, nuk spawns a new {@link nuk_user_server}. This
%% module is for a `simple_one_for_one' supervisor that supervises those
%% servers.
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user_sup).

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
    ChildSpec = {nuk_user_server,
                 {nuk_user_server, start_link, []},
                 temporary,
                 5000, % shutdown time
                 worker,
                 [nuk_user_server]},
    {ok, {{simple_one_for_one, MaxRestart, MaxTime}, [ChildSpec]}}.
