

# Module nuk_user_storage #
* [Description](#description)

`nuk_user_storage` module.

<a name="description"></a>

## Description ##
This behavior allows to extend the storage service for registered users.
When a new user is created engine with nuk via
[`nuk_users:put/1`](nuk_users.md#put-1) it is stored internally by the system.
Implementing this behavior allows a custom storage backend to be defined.
The default simple implementation is provided with
[`nuk_user_store_server`](nuk_user_store_server.md).