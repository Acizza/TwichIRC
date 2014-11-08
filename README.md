TwichIRC
========

Note: Currently not being worked on. Will likely continue after finishing work on other projects.

Should be stable enough to use without too many issues.
Currently only tested under Mono on Linux, but should run on other platforms fine.

You can get a list of current commands after launching by typing "commands" and getting their description by typing "help [command]". Alternatively, you can type the command and the usage will show up accordingly.

Example usage:
```
./TwitchIRC.exe -file=config.cfg
```
config.cfg:
```
-username = user
-oauth = oauth:12345
```

Using config files is not required, and launching the executable without any parameters will just require you to login manually (login [username] [oauth]).
