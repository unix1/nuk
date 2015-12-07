%%%-------------------------------------------------------------------
%% @doc nuk game server
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_server).

-behaviour(gen_server).

%% API
-export([create/3]).
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

%% create a new game session with options
-spec create(User :: nuk_user:user(), GameName :: string(), Options :: list()) ->
    {ok, GameSessionId :: string()} |
    {error, invalid_game_name, Extra :: string()}.
create(User, GameName, Options) ->
    {ok, Pid} = supervisor:start_child(nuk_game_sup, [GameName]),
    %% NOTE we make another gen_server call here so that we can return
    %% responses during failures - i.e. if we did everything in init
    %% we wouldn't be able to return the error details from server
    gen_server:call(Pid, {initialize, User, Options}).

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

%% start a game
-spec start(Pid :: pid(), User :: nuk_user:user()) ->
    ok |
    {error, user_not_in_game, Extra :: string()}.
start(Pid, User) ->
    gen_server:call(Pid, {start, User}).

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

handle_call({initialize, User, Options}, _From,
            #{session := GameSession} = State) ->
    %% NOTE further minor optimization is possible by setting game module in state
    GameModule = get_game_engine_module(GameSession),
    case GameModule:initialize(User, Options) of
        {error, invalid_options, Reason} ->
            {stop, normal, {error, invalid_options, Reason}, State};
        {ok, GameState} ->
            GameSession1 = nuk_game_session:set_game_state(GameSession, GameState),
            GameSession2 = nuk_game_session:set_players(GameSession1, [User]),
            GameSession3 = nuk_game_session:set_status(GameSession2, initialized),
            StateNew = State#{session := GameSession3},
            GameSessionId = nuk_game_sessions:put(self()),
            {reply, {ok, GameSessionId}, StateNew}
    end;
handle_call({player_join, User}, _From, #{session := GameSession} = State) ->
    case check_user_can_join(GameSession, User) of
        {error, ErrorCode, Reason} ->
            {reply, {error, ErrorCode, Reason}, State};
        ok ->
            GameModule = get_game_engine_module(GameSession),
            GameState = nuk_game_session:get_game_state(GameSession),
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
handle_call({start, User}, _From, #{session := GameSession} = State) ->
    case check_user_can_start(GameSession, User) of
        {error, user_not_in_game, Reason} ->
            {reply, {error, user_not_in_game, Reason}, State};
        ok ->
            GameModule = get_game_engine_module(GameSession),
            GameState = nuk_game_session:get_game_state(GameSession),
            {ok, await_turn, NextTurnPlayers, GameStateNew} = GameModule:start(GameState),
            GameSession1 = nuk_game_session:set_game_state(GameSession, GameStateNew),
            GameSession2 = nuk_game_session:set_status(GameSession1, await_turn),
            GameSession3 = nuk_game_session:set_players_turn(GameSession2,
                                                             NextTurnPlayers),
            StateNew = State#{session := GameSession3},
            {reply, ok, StateNew}
    end;
handle_call({get_session}, _From, #{session := GameSession} = State) ->
    {reply, GameSession, State};
handle_call({turn, User, Turn}, _From, #{session := GameSession} = State) ->
    case check_user_can_turn(GameSession, User) of
        {error, ErrorCode, Reason} ->
            {reply, {error, ErrorCode, Reason}, State};
        ok ->
            GameModule = get_game_engine_module(GameSession),
            GameState = nuk_game_session:get_game_state(GameSession),
            case GameModule:turn(User, Turn, GameState) of
                {error, ErrorCode, Reason} ->
                    {reply, {error, ErrorCode, Reason}, State};
                {ok, await_turn, NextTurnPlayers, GameStateNew} ->
                    GameSession1 = nuk_game_session:set_game_state(GameSession,
                                                                   GameStateNew),
                    GameSession2 = nuk_game_session:set_status(GameSession1,
                                                               await_turn),
                    GameSession3 = nuk_game_session:set_players_turn(GameSession2,
                                                                     NextTurnPlayers),
                    GameSession4 = nuk_game_session:increment_turn_number(GameSession3),
                    StateNew = State#{session := GameSession4},
                    {reply, ok, StateNew};
                {ok, complete, Winners, Losers, GameStateNew} ->
                    GameSession1 = nuk_game_session:set_game_state(GameSession,
                                                                   GameStateNew),
                    GameSession2 = nuk_game_session:set_status(GameSession1, complete),
                    GameSession3 = nuk_game_session:set_winners_losers(GameSession2,
                                                                       Winners,
                                                                       Losers),
                    GameSession4 = nuk_game_session:increment_turn_number(GameSession3),
                    StateNew = State#{session := GameSession4},
                    {reply, ok, StateNew}
            end
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
            CurrentPlayersCount = nuk_game_session:get_players_count(GameSession),
            if
                CurrentPlayersCount < MaxPlayers ->
                    ok;
                true ->
                    {error, max_users_reached, "Maximum number of users reached"}
            end
    end.

-spec check_user_can_start(GameSession :: nuk_game_session:session(),
                           User :: nuk_user:user()) ->
    ok |
    {error, user_not_in_game, Extra :: string()}.
check_user_can_start(GameSession, User) ->
    check_user_can_act(GameSession, User).

-spec check_user_can_turn(GameSession :: nuk_game_session:session(),
                          User :: nuk_user:user()) ->
    ok |
    {error, user_not_in_game, Extra :: string()} |
    {error, bad_turn_order, Extra :: string()}.
check_user_can_turn(GameSession, User) ->
    case check_user_can_act(GameSession, User) of
        {error, user_not_in_game, Reason} ->
            {error, user_not_in_game, Reason};
        ok ->
            case nuk_game_session:is_players_turn(GameSession, User) of
                false ->
                    {error, bad_turn_order, "It is not your turn"};
                true ->
                    ok
            end
    end.

-spec check_user_can_act(GameSession :: nuk_game_session:session(),
                         User :: nuk_user:user()) ->
    ok |
    {error, user_not_in_game, Extra :: string()}.
check_user_can_act(GameSession, User) ->
    case nuk_game_session:has_player(GameSession, User) of
        false ->
            {error, user_not_in_game, "User has not joined this game"};
        true ->
            ok
    end.
