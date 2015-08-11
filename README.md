Usage
=====
Simply launch the executable with your username and oauth code:
```
Twirc.exe test oauth:abc12345
```

Or with channels to join after logging in:
```
Twirc.exe test oauth:abc12345 channel1 channel2...
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
