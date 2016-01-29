%%%-------------------------------------------------------------------
%% @doc `nuk_game_coinflip_multi' module
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_coinflip).

-behaviour(nuk_game_engine).

-export([initialize/2, player_join/3, player_leave/3, start/2, turn/4, finish/2]).

-spec initialize(User :: nuk_user:user(), OptionsOverride :: list()) ->
    {error, invalid_options, string()} |
    {ok, nuk_game_engine_state:state()}.
initialize(User, OptionsOverride) when is_list(OptionsOverride) ->
    % parse and override default options
    OptionsDefault = #{max_turns => 3},
    try lists:foldl(
            fun({Name, Value}, Acc) -> Acc#{Name := Value} end,
            OptionsDefault,
            OptionsOverride
        ) of
        Options ->
            Username = nuk_user:get_username(User),
            StatePrivate = #{coin_position => nil},
            StatePublic = Options#{turn_number => 0},
            StatePlayers = #{Username => #{wins => 0, losses => 0}},
            State = nuk_game_engine_state:new(StatePrivate,
                                              StatePublic,
                                              StatePlayers),
            {ok, State}
    catch
        error:{badkey, OptionName} ->
            {error, invalid_options, OptionName}
    end.

player_join(User, State, _NukState) ->
    Username = nuk_user:get_username(User),
    StatePlayer = #{wins => 0, losses => 0},
    StateNew = nuk_game_engine_state:put_player(State, Username, StatePlayer),
    {ok, StateNew}.

player_leave(User, State, NukState) ->
    case nuk_game_state:get_status(NukState) of
        initialized -> % game hasn't started
            case length(nuk_game_state:get_players(NukState)) of
                1 -> % last player leaving, game is finished
                    % NOTE we don't remove the last player so that player can
                    % still get the game session for testing
                    %StateNew = nuk_game_engine_state:set_players(State, #{}),
                    {ok, complete, [], [], State};
                _ -> % there are more players, let this one go
                    Username = nuk_user:get_username(User),
                    StateNew = nuk_game_engine_state:remove_player(State,
                                                                   Username),
                    {ok, initialized, StateNew}
            end;
        _ -> % game has started, not allowed to leave
            {error, game_already_started, "Game has already started."}
    end.

start(State, NukState) ->
    % flip a coin, increment a public turn number
    #{max_turns := MaxTurns, turn_number := TurnNumber} =
        nuk_game_engine_state:get_public(State),
    StatePrivate = #{coin_position => flip_coin()},
    StatePublic = #{max_turns => MaxTurns, turn_number => TurnNumber + 1},
    StateNew1 = nuk_game_engine_state:set_private(State, StatePrivate),
    StateNew2 = nuk_game_engine_state:set_public(StateNew1, StatePublic),
    % it's a coin flip game, it's everybody's turn!
    {ok, await_turn, nuk_game_state:get_players(NukState), StateNew2}.

turn(User, Turn, State, NukState) when Turn =:= heads; Turn =:= tails ->
    % add wins/losses to user state
    Username = nuk_user:get_username(User),
    StatePrivate = nuk_game_engine_state:get_private(State),
    StatePlayers = nuk_game_engine_state:get_players(State),
    StatePlayer = nuk_game_engine_state:get_player(State, Username),
    #{wins := Wins, losses := Losses} = StatePlayer,
    [Win, Loss] = get_turn_result(Turn, StatePrivate),
    StatePlayerNew = StatePlayer#{wins := Wins + Win, losses := Losses + Loss},
    StatePlayersNew = StatePlayers#{Username := StatePlayerNew},
    % remove user from list of turn users, update state
    NextTurnPlayers = nuk_game_state:get_players_turn(NukState),
    NextTurnPlayersNew = lists:delete(User, NextTurnPlayers),
    % return result
    case NextTurnPlayersNew of
        [] ->
            % next turn players list is empty
            StatePublic = nuk_game_engine_state:get_public(State),
            #{turn_number := TurnNumber, max_turns := MaxTurns} = StatePublic,
            NewTurnNumber = TurnNumber + 1,
            StatePublicNew = StatePublic#{turn_number := NewTurnNumber},
            if
                TurnNumber =:= MaxTurns ->
                    % last turn, calc winners/losers and end
                    Players = nuk_game_state:get_players(NukState),
                    [Winners, Losers] = get_winners_losers(StatePlayersNew, Players),
                    StateNew = nuk_game_engine_state:set_all(State,
                                                             StatePrivate,
                                                             StatePublicNew,
                                                             StatePlayersNew),
                    {ok, complete, Winners, Losers, StateNew};
                true ->
                    % add all players back to turn users
                    StatePrivateNew = StatePrivate#{coin_position := flip_coin()},
                    StateNew = nuk_game_engine_state:set_all(State,
                                                             StatePrivateNew,
                                                             StatePublicNew,
                                                             StatePlayersNew),
                    {ok, await_turn, nuk_game_state:get_players(NukState), StateNew}
            end;
        _ ->
            % next turn players is not empty
            StateNew = nuk_game_engine_state:set_players(State, StatePlayersNew),
            {ok, await_turn, NextTurnPlayersNew, StateNew}
    end;
turn(_User, _Turn, _State, _NukState) ->
    {error, invalid_turn, "Turn must be 'heads' or 'tails'"}.

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

%% @doc Return result of player turn
%% @private
%%
%% Compares the player guess to the coin position and returns a list containing
%% number of wins and losses as a result.
%% @end
-spec get_turn_result(Turn :: atom(), State :: map()) -> [integer()].
get_turn_result(Turn, #{coin_position := Turn}) when is_atom(Turn) -> [1, 0];
get_turn_result(Turn, _State) when is_atom(Turn) -> [0, 1].

%% @doc Return a list of winners and losers
%% @private
%%
%% Given player states returns a list of 2 lists, each containing
%% `nuk_user:user()' data types; the first list is of winners and the second is
%% of losers.
%% @end
-spec get_winners_losers(StatePlayers :: map(), Players :: [nuk_user:user()])
    -> [[]].
get_winners_losers(StatePlayers, Players) ->
    #{winners := Winners, losers := Losers} = lists:foldl(
        fun(User,
            #{win_ratio := WinRatio, winners := Winners, losers := Losers} = Acc) ->
            Username = nuk_user:get_username(User),
            #{wins := Wins, losses := Losses} = maps:get(Username, StatePlayers),
            if
                Wins / Losses > WinRatio ->
                    Acc#{win_ratio := WinRatio,
                         winners := [User],
                         losers := Losers ++ Winners};
                Wins / Losses == WinRatio ->
                    Acc#{winners := [User | Winners]};
                true ->
                    Acc#{losers := [User | Losers]}
            end
        end,
        #{win_ratio => 0, winners => [], losers => []},
        Players
    ),
    [Winners, Losers].
