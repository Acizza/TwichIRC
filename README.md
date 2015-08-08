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

Command | Arguments
------- | ---------
join    | channel(s)
leave   | channel(s)
send    | channel, message
mods    | channel
