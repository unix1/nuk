%%%-------------------------------------------------------------------
%% @doc nuk sessions
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user_sessions).

%% API
-export([get/1, delete/1, list/0]).

-spec get(SessionId :: string()) ->
    {ok, nuk_user_session:session()}.
get(SessionId) ->
    nuk_user_session_store_server:get(SessionId).

-spec delete(SessionId :: string()) -> ok.
delete(SessionId) ->
    nuk_user_session_store_server:delete(SessionId).

-spec list() -> [nuk_user_session:session()].
list() ->
    nuk_user_session_store_server:list().
