

# Module nuk_user_sup #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

`nuk_user_sup` module.

__Behaviours:__ [`supervisor`](supervisor.md).

<a name="description"></a>

## Description ##

This supervisor is started by [`nuk_sup`](nuk_sup.md) top level supervisor.

Whenever a user logs in, nuk spawns a new [`nuk_user_server`](nuk_user_server.md). This
module is for a `simple_one_for_one` supervisor that supervises those
servers.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

