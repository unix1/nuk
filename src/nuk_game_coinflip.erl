%%%-------------------------------------------------------------------
%% @doc nuk game coin flip
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_coinflip).

-behaviour(nuk_game_engine).

-export([initialize/2, player_join/3, player_leave/3, start/2, turn/4, finish/2]).

initialize(User, OptionsOverride) ->
    OptionsDefault = #{
        turn_number => 0,
        wins => 0,
        losses => 0,
        max_turns => 3,
        user => User
    },
    try lists:foldl(
            fun({Name, Value}, Acc) -> Acc#{Name := Value} end,
            OptionsDefault,
            OptionsOverride
        ) of
        Options ->
            {ok, Options}
    catch
        error:{badkey, OptionName} ->
            {error, invalid_options, OptionName}
    end.

player_join(_User, State, _NukState) -> {ok, State}.

player_leave(User, #{user := User, turn_number := 0} = State, _NukState) ->
    % no turns made yet, player hasn't lost
    {ok, complete, [], [], State};
player_leave(User, #{user := User} = State, _NukState) ->
    % player is leaving the game in progress, player loses
    {ok, complete, [], [User], State}.

start(#{user := User} = State, _NukState) ->
    {ok, await_turn, [User], State}.

turn(User, Turn, #{turn_number := TurnNumber,
                   wins := Wins,
                   losses := Losses,
                   max_turns := MaxTurns,
                   user := User} = State, _NukState)
        when Turn =:= heads; Turn =:= tails ->
    NewTurnNumber = TurnNumber + 1,
    [Win, Loss] = process_turn(Turn),
    NewWins = Wins + Win,
    NewLosses = Losses + Loss,
    NewState = State#{turn_number := NewTurnNumber,
                      wins := NewWins,
                      losses := NewLosses},
    case NewTurnNumber of
        MaxTurns ->
            if
                NewWins > MaxTurns / 2 ->
                    {ok, complete, [User], [], NewState};
                true ->
                    {ok, complete, [], [User], NewState}
            end;
        _ ->
            {ok, await_turn, [User], NewState}
    end;
turn(_User, _Turn, _State, _NukState) ->
    {error, invalid_turn, "Invalid turn"}.

finish(_State, _NukState) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

process_turn(Turn) when Turn =:= heads; Turn =:= tails ->
    Toss = rand:uniform(2),
    case turn_to_number(Turn) of
        Toss ->
            [1, 0];
        _ ->
            [0, 1]
    end.

turn_to_number(Turn) when Turn =:= heads -> 1;
turn_to_number(Turn) when Turn =:= tails -> 2.
