%%%-------------------------------------------------------------------
%% @doc `nuk_games' module
%%
%% This module should be used as an API to interacting with all game related
%% actions. There are 2 types of actions: (1) game registration, and (2) game
%% flow.
%%
%% The game registration functions are:{@link register/2},
%% {@link unregister/1}, {@link get/1}, {@link list/0}.
%%
%% The game flow functions are: {@link create/2}, {@link create/3},
%% {@link join/2}, {@link leave/2}, {@link start/2},
%% {@link get_game_session/1}, {@link turn/3}.
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
-export([leave/2]).
-export([start/2]).
-export([get_game_session/1]).
-export([turn/3]).

%%====================================================================
%% Game registration
%%====================================================================

%% @doc Register a game engine
%%
%% This registers a game engine with nuk. After creating a new game engine
%% by implementing the {@link nuk_game_engine} behavior, create a
%% {@link nuk_game:game()} data type and use it here to register that game.
%% A game engine must be registered before it can be played.
%% @end
-spec register(Game :: nuk_game:game()) -> ok.
register(Game) ->
    ok = nuk_game_store_server:put(Game).

%% @doc Unregister a game engine
%%
%% Unregistering has the opposite effect of registering via {@link register/1}.
%% It makes nuk forget about the registered game.
%% @end
-spec unregister(GameName :: string()) -> ok.
unregister(GameName) ->
    ok = nuk_game_store_server:delete(GameName).

%% @doc Get a game by its name
%%
%% This can be used to look up any registered game metadata for a specific game
%% engine. Given a game name, get a {@link nuk_game:game()} data type. Then use
%% {@link nuk_game} module functions to extract needed information.
%% @end
-spec get(GameName :: string()) ->
    {ok, nuk_game:game()} |
    {error, game_not_found, Extra :: string()}.
get(GameName) ->
    nuk_game_store_server:get(GameName).

%% @doc List all registered games
%%
%% Produces a list of all registered games in the system. The return is a list
%% of {@link nuk_game:game()} data types. Use {@link nuk_game} module
%% functions to extract needed information from each game element.
%% @end
-spec list() -> [nuk_game:game()].
list() ->
    nuk_game_store_server:list().

%%====================================================================
%% Game flow
%%====================================================================

%% @doc Create a new game session with default options
%% @equiv create(UserSessionId, GameName, [])
%% @end
-spec create(UserSessionId :: string(), GameName :: string()) ->
    {ok, GameSessionId :: string()} |
    {error, invalid_user_session, Extra :: string()} |
    {error, invalid_game_name, Extra :: string()}.
create(UserSessionId, GameName) ->
    create(UserSessionId, GameName, []).

%% @doc Create a new game with options
%%
%% Using a logged in user session, create a new game by supplying its
%% registered name. This does not start a game, but merely creates a new
%% session allowing other players to join. Game session must be created before
%% it can be started.
%%
%% Calling this function triggers the {@link nuk_game_engine:initialize/2}
%% callback.
%% @end
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

%% @doc Join a player to a game session
%%
%% Joins a given logged in user session to an existing game session. Game
%% session must be created first, see {@link create/2} and {@link create/3}.
%%
%% Calling this function triggers the {@link nuk_game_engine:player_join/3}
%% callback.
%% @end
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

%% @doc Remove a player from a game session
%%
%% This does the opposite of {@link join/2} - it allows a player to leave an
%% existing game session that the player has already joined.
%%
%% Calling this function triggers the {@link nuk_game_engine:player_leave/3}
%% callback.
%% @end
-spec leave(GameSessionId :: string(), UserSessionId :: string()) ->
    ok |
    {error, game_session_not_found, Extra :: string()} |
    {error, user_session_not_found, Extra :: string()} |
    {error, user_not_in_game, Extra :: string()} |
    {error, game_already_started, Extra :: string()}.
leave(GameSessionId, UserSessionId) ->
    case get_user_game_pid(UserSessionId, GameSessionId) of
        {error, ErrorCode, Reason} ->
            {error, ErrorCode, Reason};
        {ok, User, GamePid} ->
            nuk_game_server:leave(GamePid, User)
    end.

%% @doc Start a game
%%
%% This starts an existing game session. In general, at this point all players
%% wishing to participate should have already joined the game via
%% {@link join/2}.
%%
%% Calling this function triggers the {@link nuk_game_engine:start/2} callback.
%% @end
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

%% @doc Get game session containing nuk and game engine states
%%
%% Returns the current snapshot of the game session state. The
%% {@link nuk_game_session} module functions should be used to extract needed
%% data from the returned {@link nuk_game_session:session()} data type. This is
%% useful for players to get a new game session state during the game.
%%
%% Note that {@link nuk_game_session:get_game_state/1} can be used to extract
%% arbitrary game state set by the specific {@link nuk_game_engine}.
%% @end
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

%% @doc Make a player turn
%%
%% This function should be used when it's time for a specific player to make a
%% turn. The `Turn' argument is an arbitrary term that is expected by the game
%% engine. It is not validated by nuk and is passed to the game engine
%% directly.
%%
%% Calling this function triggers the {@link nuk_game_engine:turn/4} callback.
%% @end
-spec turn(GameSessionId :: string(), UserSessionId :: string(), Turn :: term()) ->
    ok |
    {error, game_session_not_found, Extra :: string()} |
    {error, user_session_not_found, Extra :: string()} |
    {error, user_not_in_game, Extra :: string()} |
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

%% @doc Get user from user session ID
%% @private
%%
%% This is a convenience function to get a user {@link nuk_user:user()} data
%% type given the user session ID.
%% @end
-spec get_user(UserSessionId :: string()) ->
    {ok, User :: nuk_user:user()} |
    {error, user_session_not_found, Extra :: string()}.
get_user(UserSessionId) ->
    nuk_user_sessions:get_user(UserSessionId).

%% @doc Get both user and game process ID based on respective session IDs
%% @private
%%
%% This is a convenience function get both a user {@link nuk_user:user()} data
%% type and the game session process `pid()' given the user session ID and the
%% game session ID. It attempts to get the user session first. If that's
%% successful then it attempts to get the game session process ID.
%% end
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
