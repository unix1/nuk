%%%-------------------------------------------------------------------
%% @doc nuk game coin flip
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_coinflip).

-behaviour(nuk_game_engine).

%% Behavior callbacks
-export([initialize/2]).
-export([player_join/3]).
-export([player_leave/3]).
-export([start/2]).
-export([turn/4]).
-export([finish/2]).

%%====================================================================
%% Behavior callbacks
%%====================================================================

initialize(User, OptionsOverride) ->
    OptionsDefault = #{max_turns => 3},
    try lists:foldl(
            fun({Name, Value}, Acc) -> Acc#{Name := Value} end,
            OptionsDefault,
            OptionsOverride
        ) of
        Options ->
            StatePrivate = #{coin_position => nil},
            Username = nuk_user:get_username(User),
            StatePlayers = #{Username => #{wins => 0, losses => 0}},
            StatePublic = Options,
            State = nuk_game_engine_state:new(StatePrivate,
                                              StatePublic,
                                              StatePlayers),
            {ok, State}
    catch
        error:{badkey, OptionName} ->
            {error, invalid_options, OptionName}
    end.

player_join(_User, State, _NukState) -> {ok, State}.

player_leave(User, State, NukState) ->
    case nuk_game_state:get_status(NukState) of
        await_turn ->
            % player is leaving while game is in progress, player loses
            {ok, complete, [], [User], State};
        _ ->
            % game is not in progress, player hasn't lost
            {ok, complete, [], [], State}
    end.

start(State, NukState) ->
    [User] = nuk_game_state:get_players(NukState),
    StatePrivate = #{coin_position => flip_coin()},
    StateNew = nuk_game_engine_state:set_private(State, StatePrivate),
    {ok, await_turn, [User], StateNew}.

turn(User, Turn, State, NukState) when Turn =:= heads; Turn =:= tails ->
    [StatePrivate, StatePublic, StatePlayers] = nuk_game_engine_state:get_all(State),
    Username = nuk_user:get_username(User),
    #{wins := Wins, losses := Losses} = nuk_game_engine_state:get_player(State, Username),
    MaxTurns = maps:get(max_turns, StatePublic),
    [Win, Loss] = process_turn(Turn, StatePrivate),
    NewWins = Wins + Win,
    NewLosses = Losses + Loss,
    StatePlayersNew = StatePlayers#{Username := #{wins => NewWins,
                                                  losses => NewLosses}},
    StatePrivateNew = #{coin_position => flip_coin()},
    StateNew = nuk_game_engine_state:set_all(State,
                                             StatePrivateNew,
                                             StatePublic,
                                             StatePlayersNew),
    case nuk_game_state:get_turn_number(NukState) of
        MaxTurns ->
            if
                NewWins > MaxTurns / 2 ->
                    {ok, complete, [User], [], StateNew};
                true ->
                    {ok, complete, [], [User], StateNew}
            end;
        _ ->
            {ok, await_turn, [User], StateNew}
    end;
turn(_User, _Turn, _State, _NukState) ->
    {error, invalid_turn, "Invalid turn"}.

finish(_State, _NukState) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc Flips a virtual coin
%% @private
%%
%% Generates `heads' or `tails' randomly and returns result.
%% @end
flip_coin() ->
    case rand:uniform(2) of
        1 -> heads;
        2 -> tails
    end.

%% @doc Processes a player turn
%% @private
%%
%% Compares the player turn to the coin positimon and returns whether player
%% won or lost in a form of a list. The list contains number of wins and losses
%% as a result of this turn - i.e. either `[1, 0]' or `[0, 1]'.
process_turn(Turn, #{coin_position := CoinPosition})
        when Turn =:= heads; Turn =:= tails ->
    case Turn of
        CoinPosition ->
            [1, 0];
        _ ->
            [0, 1]
    end.
