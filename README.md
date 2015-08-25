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

Settings
========
If you want to change the colors of the interface, join certain channels every time you launch the program, or enable auto-login, you'll have to create a file in the root directory called _settings.cfg_.

A typical settings file looks like this:
```
username:<Twitch username>
password:<Twitch oauth code>
channels:<channels to join after login>
```
Note that the channels setting is optional.

If you don't like the default colors of the interface, you can change them. To replace a certain color, add this to your settings file:
```
color<color to replace>:<new color>
```

For example:
```
colorDarkRed:Magenta
```

For a full list of colors, take a look [here.](https://msdn.microsoft.com/en-us/library/system.consolecolor(v=vs.110).aspx)

Commands
========
Once logged in, you can use several commands:

Command  | Arguments        | Description
-------  | ---------------- | -----------
join     | channel(s)       | Joins the specified channels.
leave    | channel(s)       | Leaves the specified channels.
send     | channel, message | Sends a message to the specified channel.
mods     | channel          | Prints the moderators of the specified channel.
channels | \<none>          | Prints the list of currently connected channels.
leaveall | \<none>          | Leaves all connected channels.
commands | \<none>          | Prints all commands with their usage.
