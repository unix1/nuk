Developer Overview
==================

Organization
------------

nuk is an OTP application that is supervised by `nuk_sup` top level supervisor; which, in turn, starts the following supervisors:

- `nuk_user_sup`: supervises `simple_one_for_one` workers `nuk_user_server` for each logged in user
- `nuk_user_store_sup`: supervises `one_for_one` worker `nuk_user_store_server` (see below for more info)
- `nuk_game_sup`: supervises `simple_one_for_one` workers `nuk_game_server` for each game session
- `nuk_game_store_sup`: supervises `one_for_one` worker `nuk_game_store_server` which handles registration of game engines

Users and Sessions
------------------

Public interfaces:

- `nuk_users` for all user related actions: `get`, `put`, `delete`, `list`, `login`
- `nuk_user_sessions` for all user session related actions: `get`, `delete`, `list`
- `nuk_games` for all game related actions: registering, starting, joining, turns, etc.
- `nuk_game_sessions` for all game session related actions

nuk provides behaviors for implementing callbacks for user, game and session storage:

- `nuk_user_storage`
- `nuk_user_session_storage`
- `nuk_game_storage`
- `nuk_game_session_storage`

The provided implementations of these behaviors are:

- `nuk_user_store_server`: stores users in local server state and provides behavior callbacks to operate on users
- `nuk_user_session_store_server`: provides behavior callbacks to operate on user sessions; the default session "storage" is the `nuk_user_sup` and its children active sessions
- `nuk_game_store_server`: stores registered games in local server state and provides behavior callbacks to operate on game registration
- `nuk_game_session_store_server`: provides behavior callbacks to operate on game sessions; the main function of this behavior is to translate a session token to a `nuk_game_server` process ID

These implementations are only there for proof on concept and testing purposes. i.e. you might want to hook up a more robust user and session storage than the default implementations provided with nuk. This is possible by implementing above behaviors and then setting them in nuk application environment values:

```erlang
application:set_env(nuk, users, custom_user_store_service).
application:set_env(nuk, user_sessions, custom_user_session_store_service).
application:set_env(nuk, games, custom_game_store_service).
application:set_env(nuk, game_sessions, custom_game_session_store_service).
```

Replace the `custom_*` module names above with the actual module names.

Note that you may pick which behaviors to implement. For example, you could implement a custom user, user session and game session storage, but leave the existing nuk game storage.

Games
-----

nuk starts a new `nuk_game_server` worker whenever a game session is started. `nuk_game_server` does the following:

- handles interaction with the players
- keeps the state and general data about the specific game session and its players
- invokes game engine callbacks for specific events during the game
- stores game engine state and passes it with every callback

nuk provides the `nuk_game_engine` behavior for a general hookup for any turn based game engine. `nuk_game_engine` defines callbacks that `nuk_game_server` invokes during specific events, passing along both the internal state and general data that it keeps about the game session.

These callbacks are:

- `initialize` - when a player first creates an instance of a new game
- `player_join` - when a player joins a game
- `player_leave` - when a player leaves a game
- `start` - when a player attempts to start a game
- `turn` - when a player makes a turn
- `finish` - when a game is completed

`nuk_game_server` already handles general validation and bookkeeping of these actions and game implementations (although they could) need not concern themselves with them. For example, nuk's game state will keep track of which users have joined a specific game, whose turn it is next, game's status, etc.

For further information about how to implement a game engine, refer to [Implementing a game](implementing-a-game.md).
