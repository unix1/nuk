

# Module nuk_sup #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

`nuk_sup` module.

__Behaviours:__ [`supervisor`](supervisor.md).

<a name="description"></a>

## Description ##
This is a top level `one_for_one` nuk supervisor started by the nuk
application. It, in turn, starts the following supervisors under it:
- [`nuk_user_sup`](nuk_user_sup.md)
- [`nuk_user_store_sup`](nuk_user_store_sup.md)
- [`nuk_game_sup`](nuk_game_sup.md)
- [`nuk_game_store_sup`](nuk_game_store_sup.md)<a name="index"></a>

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

