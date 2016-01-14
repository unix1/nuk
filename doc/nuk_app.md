

# Module nuk_app #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

`nuk_app` module.

__Behaviours:__ [`application`](application.md).

<a name="description"></a>

## Description ##
This starts the nuk application.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_storage_module-1">get_storage_module/1</a></td><td>Get storage module.</td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_storage_module-1"></a>

### get_storage_module/1 ###

<pre><code>
get_storage_module(Type::atom()) -&gt; atom()
</code></pre>
<br />

Get storage module

Returns the storage module associated with the specified storage type. If
none specified, nuk default is returned.

<a name="start-2"></a>

### start/2 ###

`start(StartType, StartArgs) -> any()`

<a name="stop-1"></a>

### stop/1 ###

`stop(State) -> any()`

