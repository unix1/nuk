%%%-------------------------------------------------------------------
%% @doc nuk game server
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_server).

-behaviour(gen_server).

%% API
-export([create/2]).
-export([start/2]).
-export([join/2]).
-export([leave/2]).
-export([get_session/1]).
-export([turn/3]).
-export([finish/1]).

%% Supervision
-export([start_link/1, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

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
-spec create(User :: nuk_user:user(), GameName :: string()) ->
    {ok, GameSessionId :: string()} |
    {error, invalid_game_name, Extra :: string()}.
create(User, GameName) ->
    {ok, Pid} = supervisor:start_child(nuk_game_sup, [GameName]),
    %% TODO move to init/1 ?
    %% TODO at least game module lookup, so it's stored in state
    gen_server:call(Pid, {initialize, User}).

-spec join(Pid :: pid(), User :: nuk_user:user()) ->
    ok |
    {error, user_already_joined, Extra :: string()} |
    {error, max_users_reached, Extra :: string()}.
join(Pid, User) ->
    gen_server:call(Pid, {player_join, User}).

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

%% get game session
-spec get_session(Pid :: pid) -> term().
get_session(Pid) ->
    gen_server:call(Pid, {get_session}).

%% process player's turn
-spec turn(Pid :: pid(), User :: nuk_user:user(), Turn :: term()) ->
    ok |
    {error, bad_turn_order, Extra :: string()} |
    {error, invalid_turn, Extra :: string()}.
turn(Pid, User, Turn) ->
    gen_server:call(Pid, {turn, User, Turn}).

%% end a game
%% TODO figure out finish
-spec finish(Pid :: pid()) -> ok.
finish(Pid) ->
    ok = gen_server:call(Pid, {finish}).

%%====================================================================
%% Behavior callbacks
%%====================================================================

handle_call({initialize, User}, _From,
            #{session := GameSession} = State) ->
    GameModule = get_game_engine_module(GameSession),
    %% TODO support options?
    %% TODO handle when this call fails
    {ok, GameState} = GameModule:initialize(User, []),
    GameSession1 = nuk_game_session:set_game_state(GameSession, GameState),
    GameSession2 = nuk_game_session:set_players(GameSession1, [User]),
    GameSession3 = nuk_game_session:set_status(GameSession2, initialized),
    StateNew = State#{session := GameSession3},
    GameSessionId = nuk_game_sessions:put(self()),
    {reply, {ok, GameSessionId}, StateNew};
handle_call({player_join, User}, _From, #{session := GameSession} = State) ->
    GameModule = get_game_engine_module(GameSession),
    GameState = nuk_game_session:get_game_state(GameSession),
    case check_user_can_join(GameSession, User) of
        {error, ErrorCode, Reason} ->
            {reply, {error, ErrorCode, Reason}, State};
        ok ->
            case GameModule:player_join(User, GameState) of
                {error, ErrorCode, Reason} ->
                    {reply, {error, ErrorCode, Reason}, State};
                {ok, GameStateNew} ->
                    GameSession1 = nuk_game_session:set_game_state(GameSession,
                                                                   GameStateNew),
                    GameSession2 = nuk_game_session:add_player(GameSession1, User),
                    StateNew = State#{session := GameSession2},
                    {reply, ok, StateNew}
            end
    end;
handle_call({player_leave, _User}, _From, State) ->
    %% TODO invoke game engine
    {reply, ok, State};
handle_call({start, _User}, _From, State) ->
    %% TODO invoke game engine
    {reply, ok, State};
handle_call({get_session}, _From, #{session := GameSession} = State) ->
    {reply, GameSession, State};
handle_call({turn, User, Turn}, _From, #{session := GameSession} = State) ->
    GameModule = get_game_engine_module(GameSession),
    GameState = nuk_game_session:get_game_state(GameSession),
    case GameModule:turn(User, Turn, GameState) of
        {error, ErrorCode, Reason} ->
            {error, ErrorCode, Reason};
        {ok, await_turn, NextTurnPlayers, GameState} ->
            GameSession1 = nuk_game_session:set_game_state(GameSession, GameState),
            GameSession2 = nuk_game_session:set_status(GameSession1, await_turn),
            GameSession3 = nuk_game_session:set_players_turn(GameSession2,
                                                             NextTurnPlayers),
            StateNew = State#{session := GameSession3},
            {reply, ok, StateNew};
        {ok, complete, _Winners, _Losers, _Extra} ->
            %% TODO set winners, losers, extra
            GameSession1 = nuk_game_session:set_status(GameSession, complete),
            StateNew = State#{session := GameSession1},
            {reply, ok, StateNew}
    end;
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

-spec get_game_engine_module(GameSession :: nuk_game_session:session()) -> atom().
get_game_engine_module(GameSession) ->
    Game = nuk_game_session:get_game(GameSession),
    nuk_game:get_module(Game).

-spec get_max_players(GameSession :: nuk_game_session:session()) -> integer().
get_max_players(GameSession) ->
    Game = nuk_game_session:get_game(GameSession),
    nuk_game:get_max_players(Game).

-spec check_user_can_join(GameSession :: nuk_game_session:session(),
                          User :: nuk_user:user()) ->
    ok |
    {error, user_already_joined, Extra :: string()} |
    {error, max_users_reached, Extra :: string()}.
check_user_can_join(GameSession, User) ->
    case nuk_game_session:has_player(GameSession, User) of
        true ->
            {error, user_already_joined, "User already joined the game"};
        false ->
            MaxPlayers = get_max_players(GameSession),
            CurrentPlayersCcount = nuk_game_session:get_players_count(GameSession),
            if
                CurrentPlayersCcount < MaxPlayers ->
                    ok;
                true ->
                    {error, max_users_reached, "Maximum number of users reached"}
            end
    end.
