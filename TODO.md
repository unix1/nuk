TODO
====

Users and sessions
------------------
- separate user storage into application/behavior
- separate session storage into application/behavior
- user sessions storage
- wrapper functions performed by simple_one_for_one workers (user login, sessions)

Game
----
- game functions
- game engine behavior


Desired client interface
------------------------
```erlang

%% login/logout
Session = nuk_users:login(Username, Password).
ok = nuk_users:logout(Session).

%% returns list of all active users
[User1|OtherUsers] = nuk_users:list_users().

%% returns list of available games (game engines)
[Game1|OtherGames] = nuk_games:list().

%% returns list of active games
[GameSession2|OtherGameSessions] = nuk_games:running().

%% start a new game
GameSession1 = nuk_games:start(Session, Game1).

%% join an existing game
nuk_games:join(Session, GameSession2).

nuk_games:get_state(Session, GameSession).

nuk_games:move(Session, GameSession, Move).

nuk_games:leave(Session, GameSession).

nuk_games:end(Session, GameSession).
```

Desired game engine interface
-----------------------------
```
nuk_games:initialize ------> nuk_game_engine:initialize ------> {ok, GameState}

nuk_games:join       ------> nuk_game_engine:join       ------> {ok, GameState}

nuk_games:start      ------> nuk_game_engine:start      ------> {ok, GameState}

nuk_games:move       ------> nuk_game_engine:move       ------> {ok, GameState}

```

Game actions and external status
--------------------------------
- initialize: initialized
- start: active

Game state
----------
```erlang
#{
    status => Status, % see above
    players => [User1, User2, User3, User4],
    waiting_turn => [User2, User4],
    state => #{
        %% other internal state specific to game engine
    }
}
```
