

# Module nuk_user_session #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

`nuk_user_session` module.

<a name="description"></a>

## Description ##

This module is used to operate on [`nuk_user_session:session()`](nuk_user_session.md#type-session) data
type. This data type is used when dealing with user sessions in nuk, for
example, when calling [`nuk_user_sessions:get/1`](nuk_user_sessions.md#get-1).

For public API the [`nuk_user_sessions`](nuk_user_sessions.md) module should be used which
already provides convenience functions for extracting information from
user sessions.
<a name="types"></a>

## Data Types ##




### <a name="type-session">session()</a> ###


__abstract datatype__: `session()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_user-1">get_user/1</a></td><td>Get user.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Create a new <a href="nuk_user_session.md#type-session"><code>nuk_user_session:session()</code></a> data type.</td></tr><tr><td valign="top"><a href="#set_user-2">set_user/2</a></td><td>Set user.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_user-1"></a>

### get_user/1 ###

<pre><code>
get_user(X1::<a href="#type-session">session()</a>) -&gt; <a href="nuk_user.md#type-user">nuk_user:user()</a>
</code></pre>
<br />

Get user

Extracts a [`nuk_user:user()`](nuk_user.md#type-user) stored in the session.

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

Create a new [`nuk_user_session:session()`](nuk_user_session.md#type-session) data type

Creates a new data type with default values.

<a name="set_user-2"></a>

### set_user/2 ###

<pre><code>
set_user(Session::<a href="#type-session">session()</a>, User::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

Set user

Set a specified [`nuk_user:user()`](nuk_user.md#type-user) in the session.

