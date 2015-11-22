Usage
=====

At the moment, the only way to start the program involves providing your username and OAuth code (which you can get from [here](https://twitchapps.com/tmi/)), with an optional list of channels to join at the end of the argument list:
```
./twirc test_username oauth:12345 channel1 channel2...
```

Commands
========

After you login, there are several commands available for use:

Name     | Arguments        | Description
-------- | ---------------- | -----------
join     | channel(s)       | Joins the specified channels.
leave    | channel(s)       | Leaves the specified channels.
send     | channel, message | Sends a message to the specified channel.
mods     | channel          | Prints a list of moderators connected to the specified channel.
channels | none             | Prints a list of all currently connected channels.
leaveall | none             | Leaves all currently connected channels.
