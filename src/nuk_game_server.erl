%%%-------------------------------------------------------------------
%% @doc nuk game server
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/1, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([create/2, start/2, join/2, leave/2, get_game_state/1, finish/1]).

%%====================================================================
%% Supervision
%%====================================================================

start_link(GameName) ->
    gen_server:start_link(?MODULE, [GameName], []).

init([GameName]) ->
    {ok, Game} = nuk_games:get(GameName),
    {ok, #{session => nuk_game_session:new(Game)}}.

%%====================================================================
%% API
%%====================================================================

%% create a new game session
%% TODO move to init/1 ?
%% TODO at least game module lookup, so it's stored in state
-spec create(UserSessionId :: string(), GameName :: string()) ->
    {ok, GameSessionId :: string()} |
    {error, user_session_not_found, Extra :: string()} |
    {error, invalid_game_name, Extra :: string()}.
create(UserSessionId, GameName) ->
    {ok, Pid} = supervisor:start_child(nuk_game_sup, [GameName]),
    gen_server:call(Pid, {initialize, UserSessionId}).

-spec join(Pid :: pid(), UserSessionId :: string()) ->
    ok |
    {error, user_already_joined, Extra :: string()} |
    {error, max_users_reached, Extra :: string()}.
join(Pid, UserSessionId) ->
    gen_server:call(Pid, {player_join, UserSessionId}).

-spec leave(Pid :: pid(), UserSessionId :: string()) ->
    ok |
    {error, unknown_user, Extra :: string()}.
leave(Pid, UserSessionId) ->
    gen_server:call(Pid, {player_leave, UserSessionId}).

%% start a game.
-spec start(Pid :: pid(), UserSessionId :: string()) ->
    ok |
    {error, game_session_not_found, Extra :: string()} |
    {error, user_session_not_found, Extra :: string()} |
    {error, game_engine_error, Extra :: string()}.
start(Pid, UserSessionId) ->
    gen_server:call(Pid, {start, UserSessionId}).

-spec get_game_state(Pid :: pid) -> term().
get_game_state(Pid) ->
    gen_server:call(Pid, {get_game_state}).

%% end a game
%% TODO figure out finish
-spec finish(Pid :: pid()) -> ok.
finish(Pid) ->
    ok = gen_server:call(Pid, {finish}).

%%====================================================================
%% Behavior callbacks
%%====================================================================

handle_call({initialize, UserSessionId}, _From,
            #{session := GameSession} = State) ->
    GameModule = get_game_engine_module(GameSession),
    case get_user(UserSessionId) of
        {error, user_session_not_found, Reason} ->
            {reply, {error, user_session_not_found, Reason}, State};
        {ok, User} ->
            %% TODO support options?
            {ok, GameState} = GameModule:initialize(User, []),
            GameSessionNew = nuk_game_session:set_game_state(GameSession, GameState),
            StateNew = State#{session := GameSessionNew},
            GameSessionId = nuk_game_sessions:put(self()),
            {reply, {ok, GameSessionId}, StateNew}
    end;
handle_call({player_join, UserSessionId}, _From, #{session := GameSession} = State) ->
    %% TODO invoke game engine
    case get_user(UserSessionId) of
        {error, user_session_not_found, Reason} ->
            {reply, {error, user_session_not_found, Reason}, State};
        {ok, User} ->
            GameModule = get_game_engine_module(GameSession),
            GameState = nuk_game_session:get_game_state(GameSession),
            case GameModule:player_join(User, GameState) of
                {error, ErrorCode, Reason} ->
                    {reply, {error, ErrorCode, Reason}, State};
                {ok, GameStateNew} ->
                    StateNew = nuk_game_session:set_game_state(GameSession, GameStateNew),
                    {reply, ok, StateNew}
            end
    end;
handle_call({player_leave, _UserSessionId}, _From, State) ->
    %% TODO invoke game engine
    {reply, ok, State};
handle_call({start, _UserSessionId}, _From, State) ->
    %% TODO invoke game engine
    {reply, ok, State};
handle_call({get_game_state}, _From, #{session := GameSession} = State) ->
    GameState = nuk_game_session:get_game_state(GameSession),
    {reply, GameState, State};
handle_call({finish}, _From, State) ->
    %% TODO invoke game engine
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

get_game_engine_module(GameSession) ->
    Game = nuk_game_session:get_game(GameSession),
    nuk_game:get_module(Game).

%% TODO does this belong in this module?
get_user(UserSessionId) ->
    case nuk_user_sessions:get(UserSessionId) of
        {error, user_session_not_found, Reason} ->
            {error, user_session_not_found, Reason};
        {ok, UserSession} ->
            {ok, nuk_user_session:get_user(UserSession)}
    end.
