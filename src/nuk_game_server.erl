%%%-------------------------------------------------------------------
%% @doc nuk game server
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/0, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([create/2, start/2, join/2, leave/2, finish/1]).

%%====================================================================
%% Supervision
%%====================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([GameName]) ->
    {ok, #{session => nuk_game_session:new(GameName)}}.

%%====================================================================
%% API
%%====================================================================

%% create a new game session
%% TODO move to init/1 ?
-spec create(UserSessionId :: string(), GameName :: string()) ->
    {ok, GameSessionId :: string()} |
    {error, invalid_user_session, Extra :: string()} |
    {error, invalid_game_name, Extra :: string()}.
create(UserSessionId, GameName) ->
    {ok, Pid} = supervisor:start_child(nuk_game_sup, [GameName]),
    gen_server:call(Pid, {initialize, UserSessionId}).

-spec join(Pid :: pid(), UserSessionId :: string()) ->
    ok |
    {error, already_joined, Extra :: string()} |
    {error, max_users_reached, Extra :: string()}.
join (Pid, UserSessionId) ->
    gen_server:call(Pid, {player_join, UserSessionId}).

-spec leave(Pid :: pid(), UserSessionId :: string()) ->
    ok |
    {error, unknown_user, Extra :: string()}.
leave (Pid, UserSessionId) ->
    gen_server:call(Pid, {player_leave, UserSessionId}).

%% start a game.
-spec start(Pid :: pid(), UserSessionId :: string()) ->
    ok |
    {error, invalid_game_session, Extra :: string()} |
    {error, invalid_user_session, Extra :: string()} |
    {error, game_engine_error, Extra :: string()}.
start(Pid, UserSessionId) ->
    gen_server:call(Pid, {start, UserSessionId}).

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
    User = get_user(UserSessionId),
    %% TODO support options?
    GameState = GameModule:initialize(User, []),
    GameSessionNew = nuk_game_session:set_game_state(GameSession, GameState),
    StateNew = State#{session := GameSessionNew},
    {reply, ok, StateNew};
handle_call({player_join, _UserSessionId}, _From, State) ->
    %% TODO invoke game engine
    {reply, ok, State};
handle_call({player_leave, _UserSessionId}, _From, State) ->
    %% TODO invoke game engine
    {reply, ok, State};
handle_call({start, _UserSessionId}, _From, State) ->
    %% TODO invoke game engine
    {reply, ok, State};
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
    UserSession = nuk_sessions:get_session(UserSessionId),
    nuk_sessions:get_user(UserSession).
