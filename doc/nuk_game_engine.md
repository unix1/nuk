

# Module nuk_game_engine #
* [Description](#description)

`nuk_game_engine` module.

<a name="description"></a>

## Description ##
This is a behavior that all game engines must implement. It is also the only
logic that game engines need implement. All callback function returns allow
engines to: (1) set the arbitrary state that's relevant to the game being
implemented, (2) get callbacks for important game events with the arbitrary
state previously set.