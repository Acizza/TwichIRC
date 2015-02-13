Twirc
========

A simple CLI-based IRC client for Twitch, primarily for Linux.
A GUI is not planned at the moment, but one may come in the future.

Usage
=====

Connecting:
```
./Twirc.CLI.exe -username=test123 -password=oauth:1234
```

Connecting & joining multiple channels:
```
./Twirc.CLI.exe -username=test123 -password=oauth:1234 channel1 channel2 channel3
```

After starting the application, you can type a different set of commands to do various things. To see all of the available commands, type `commands` in to the console. Typing `help <command>` will display its usage.

You can also put commands (like login information) in a file named `settings.cfg`, or one with a name of your choice (just run the application with `-settings=<path>`).

An example settings.cfg looks like this:
```
username = test123
password = oauth:1234
autologin = true
```

You can save your current login information once the application has started by typing `savesettings` in to the console.
