%%%-------------------------------------------------------------------
%% @doc nuk user supervisor
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
