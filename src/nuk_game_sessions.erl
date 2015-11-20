%%%-------------------------------------------------------------------
%% @doc nuk game sessions
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_sessions).

%% API
-export([get_pid/1, put/1]).

-spec get_pid(SessionId :: string()) ->
    {ok, Pid :: pid()} |
    {error, game_session_not_found, Extra :: string()}.
get_pid(SessionId) ->
    nuk_game_session_store_server:get_pid(SessionId).

-spec put(Pid :: pid()) -> SessionId :: string().
put(Pid) when is_pid(Pid) ->
    nuk_game_session_store_server:put(Pid).
