Usage
=====

You can launch the program in several ways:

With your username and oauth code:
```
Twirc.exe test oauth:abc12345
```

With channels to join after logging in:
```
Twirc.exe test oauth:abc12345 channel1 channel2...
```

Or, if you have created a settings.cfg file, you can launch it with no arguments or the channels you wish to join after logging in.

A typical settings.cfg file looks like this:
```
username:<user>
password:<oauth code>
```

Once logged in, you can use several commands:

Command  | Arguments        | Description
-------  | ---------------- | -----------
join     | channel(s)       | Joins the specified channels.
leave    | channel(s)       | Leaves the specified channels.
send     | channel, message | Sends a message to the specified channel.
mods     | channel          | Prints the moderators of the specified channel.
channels | \<none>          | Prints the list of currently connected channels.
leaveall | \<none>          | Leaves all connected channels.
