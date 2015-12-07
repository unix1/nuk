%%%-------------------------------------------------------------------
%% @doc nuk games
%% @end
%%%-------------------------------------------------------------------

-module(nuk_games).

%% API

% Game registration
-export([register/1]).
-export([unregister/1]).
-export([get/1, list/0]).

% Game flow
-export([create/2]).
-export([create/3]).
-export([join/2]).
-export([start/2]).
-export([get_game_session/1]).
-export([turn/3]).

%%====================================================================
%% Game registration
%%====================================================================

-spec register(Game :: nuk_game:game()) -> ok.
register(Game) ->
    ok = nuk_game_store_server:put(Game).

-spec unregister(GameName :: string()) -> ok.
unregister(GameName) ->
    ok = nuk_game_store_server:delete(GameName).

-spec get(GameName :: string()) ->
    {ok, nuk_game:game()} |
    {error, game_not_found, string()}.
get(GameName) ->
    nuk_game_store_server:get(GameName).

-spec list() -> [nuk_game:game()].
list() ->
    nuk_game_store_server:list().

%%====================================================================
%% Game flow
%%====================================================================

-spec create(UserSessionId :: string(), GameName :: string()) ->
    {ok, GameSessionId :: string()} |
    {error, invalid_user_session, Extra :: string()} |
    {error, invalid_game_name, Extra :: string()}.
create(UserSessionId, GameName) ->
    create(UserSessionId, GameName, []).

-spec create(UserSessionId :: string(),
             GameName :: string(),
             Options :: list(tuple())) ->
    {ok, GameSessionId :: string()} |
    {error, invalid_user_session, Extra :: string()} |
    {error, invalid_game_name, Extra :: string()}.
create(UserSessionId, GameName, Options) ->
    case get_user(UserSessionId) of
        {error, user_session_not_found, Reason} ->
            {error, user_session_not_found, Reason};
        {ok, User} ->
            nuk_game_server:create(User, GameName, Options)
    end.

-spec join(GameSessionId :: string(), UserSessionId :: string()) ->
    ok |
    {error, game_session_not_found, Extra :: string()} |
    {error, user_session_not_found, Extra :: string()} |
    {error, user_already_joined, Extra :: string()} |
    {error, max_users_reached, Extra :: string()}.
join(GameSessionId, UserSessionId) ->
    case get_user_game_pid(UserSessionId, GameSessionId) of
        {error, ErrorCode, Reason} ->
            {error, ErrorCode, Reason};
        {ok, User, GamePid} ->
            nuk_game_server:join(GamePid, User)
    end.

-spec start(GameSessionId :: string(), UserSessionId :: string()) ->
    ok |
    {error, game_session_not_found, Extra :: string()} |
    {error, user_session_not_found, Extra :: string()} |
    {error, min_users_not_met, Extra :: string()} |
    {error, user_not_in_game, Extra :: string()}.
start(GameSessionId, UserSessionId) ->
    case get_user_game_pid(UserSessionId, GameSessionId) of
        {error, ErrorCode, Reason} ->
            {error, ErrorCode, Reason};
        {ok, User, GamePid} ->
            nuk_game_server:start(GamePid, User)
    end.

-spec get_game_session(GameSessionId :: string()) ->
    {ok, nuk_game_session:session()} |
    {error, game_session_not_found, Extra :: string()}.
get_game_session(GameSessionId) ->
    case nuk_game_sessions:get_pid(GameSessionId) of
        {error, game_session_not_found, Reason} ->
            {error, game_session_not_found, Reason};
        {ok, GamePid} ->
            {ok, nuk_game_server:get_session(GamePid)}
    end.

-spec turn(GameSessionId :: string(), UserSessionId :: string(), Turn :: term()) ->
    ok |
    {error, game_session_not_found, Extra :: string()} |
    {error, user_session_not_found, Extra :: string()} |
    {error, bad_turn_order, Extra :: string()} |
    {error, invalid_turn, Extra :: string()}.
turn(GameSessionId, UserSessionId, Turn) ->
    case get_user_game_pid(UserSessionId, GameSessionId) of
        {error, ErrorCode, Reason} ->
            {error, ErrorCode, Reason};
        {ok, User, GamePid} ->
            nuk_game_server:turn(GamePid, User, Turn)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

-spec get_user(UserSessionId :: string()) ->
    {ok, User :: nuk_user:user()} |
    {error, user_session_not_found, Extra :: string()}.
get_user(UserSessionId) ->
    nuk_user_sessions:get_user(UserSessionId).

-spec get_user_game_pid(UserSessionId :: string(), GameSessionId :: string()) ->
    {ok, User :: nuk_user:user(), GamePid :: pid()} |
    {error, user_session_not_found, Extra :: string()} |
    {error, game_session_not_found, Extra :: string()}.
get_user_game_pid(UserSessionId, GameSessionId) ->
    case nuk_user_sessions:get_user(UserSessionId) of
        {error, user_session_not_found, Reason} ->
            {error, user_session_not_found, Reason};
        {ok, User} ->
            case nuk_game_sessions:get_pid(GameSessionId) of
                {error, game_session_not_found, Reason} ->
                    {error, game_session_not_found, Reason};
                {ok, GamePid} ->
                    {ok, User, GamePid}
            end
    end.
