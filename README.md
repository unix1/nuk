nuk
=====

nuk is a generic turn based game server written in Erlang.

Features
--------

- provides framework for registering games, users and controlling their sessions
- allows you to write and plug in your own turn based game engine using behavior callbacks
- allows to use custom external storage using simple behaviors

Additional info
---------------

- [developer overview](guide/developer-overview.md)
- [guide](guide/guide.md)
- [implementing a game](guide/implementing-a-game.md) (coming soon)
- [reference documentation](doc/README.md)

what nuk is *not*:
------------------

- client or frontend (it's a server)
- web server: it does not provide one, although I might consider adding this in the future
