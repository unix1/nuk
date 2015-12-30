

# Module nuk_game_storage #
* [Description](#description)

`nuk_game_storage` module.

<a name="description"></a>

## Description ##
This behavior allows to extend the storage service for registered games.
When a new game engine is registered with nuk via
[`nuk_games:register/1`](nuk_games.md#register-1) it is stored internally by the system.
Implementing this behavior allows a custom storage backend to be defined.
The default simple implementation is provided with
[`nuk_game_store_server`](nuk_game_store_server.md).