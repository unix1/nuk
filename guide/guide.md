Guide
=====

nuk is a bare bones turn based game server implementation in Erlang. It is meant to provide the following interfaces and functionality:

- interface for clients/players to interact with the game server
- interface for arbitrary turn-based game implementations
- functionality for common turn-based game servers

This is a simple guide to demonstrate how to register a game and use the player inteface with the game server.

Registering a Game
------------------

nuk provides a behavior for game writers to implement arbitrary turn based games. This is done by implementing the `nuk_game_engine` behavior. That topic is covered separately in [Implementing a game](implementing-a-game.md). Once you have a specific game implementation, you must first register that game with nuk before it can be used:

For example, if we have implemented a Coin Flip game, we can register it like this:

```erlang
% create a game data type passing name, module, min and max players
Game = nuk_game:new("Coin Flip", nuk_game_coinflip, 1, 2),

% register the game
ok = nuk_games:register(Game).
```

That's it! The game is registered and can be referenced with the unique name "Coin Flip".

Users Interface
---------------

Next we must have valid users who can play a game.

To create users:

```erlang
ok = nuk_users:put(nuk_user:new("User1", "Pass1")).
ok = nuk_users:put(nuk_user:new("User2", "Pass2")).
```

To log in:

```erlang
{ok, UserSessionId1} = nuk_users:login("User1", "Pass1").
{ok, UserSessionId2} = nuk_users:login("User2", "Pass2").
```

Player Interface
----------------

Having registered a game and created users, we can start a new game.

First, we must create a new game session, allowing any valid number of users to join it:

```erlang
{ok, GameSessionId} = nuk_games:create(UserSessionId1, "Coin Flip").
```

Now we can have other logged in user sessions join the newly created game:

```erlang
ok = nuk_games:join(GameSessionId, UserSessionId2).
```

After we are satisifed with the players who have joined the game then it can be started:

```erlang
ok = nuk_games:start(GameSessionId, UserSessionId1).
```

After the game has started players can request to get the data in the game session. The game session data consists of few components:

- game registration data
- general nuk game state, containing information about
    - players in the game
    - turn information: turn number and whose turn it is next
- game engine specific game state, consisting of
    - private game state which will always be empty for every player
    - public game state which is shared among all players
    - player specific state which is shared only with the requesting player

Here's how to get these states and extract the data:

```erlang
% get game session data
{ok, GameSession} = nuk_games:get_game_session(GameSessionId, UserSessionId1).

% extract general nuk info

% players in the game
Players = nuk_game_session:get_players(GameSession).

% status, just started games will most likely be in await_turn
await_turn = nuk_game_session:get_status(GameSession).

% whose turn is it next?
NextTurnPlayers = nuk_game_session:get_players_turn(GameSession).

% game specific state provided by the game engine, e.g. coin flip game above
GameState = nuk_game_session:get_game_state(GameSession).

% game public state
PublicState = nuk_game_engine_state:get_public(GameState).

% game player specific state
PlayerState = nuk_game_engine_state:get_player(GameState, "User1").
```

Here's how to make a turn:

```erlang
ok = nuk_games:turn(GameSessionId, UserSessionId1, heads).
```

Note that in the above example, to make a turn we supply the game and user session IDs and the turn data. In our simple Coin Flip game example, the turn is simply atom `heads` but it is a term entirely dependent on the game being played. It must the the data structure that the game engine expects.

After the turn is made, the players can proceed to get the game session and state data as shown above and keep making turns until the game ends. Once the game ends, the status will be `complete` and winners and losers can then be obtained:

```erlang
complete = nuk_game_session:get_status(GameSession).
{Winners, Losers} = nuk_game_session:get_winners_losers(GameSession).
```

This is it for a simple guide of how users and games work in nuk!

How to Play
-----------

```erlang

% register a game
nuk_games:register(nuk_game:new("Coin Flip", nuk_game_coinflip, 1, 1)).

% create and login players
nuk_users:put(nuk_user:new("User1", "Pass1")).
{ok, UserSessionId1} = nuk_users:login("User1", "Pass1").

% create a new game session
{ok, GameSessionId1} = nuk_games:create(UserSessionId1, "Coin Flip").

```
