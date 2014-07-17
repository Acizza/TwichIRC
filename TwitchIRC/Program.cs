using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;

// TODO: Refactor.

namespace TwitchIRC
{
	public class Program
	{
		public static AClientHandler Handler { get; private set; }

		static void Main(string[] args)
		{
			string username = null;
			string oauth    = null;

			var client = new IrcClient(Handler = new TwitchHandler());
			var parser = new ArgParser();

			parser.Add("file",     v => parser.Parse(File.ReadAllLines(v)));
			parser.Add("username", v => username = v);
			parser.Add("oauth",    v => oauth = v);

			parser.Parse(args);

			if(username != null && oauth != null) // Temporary
				client.Connect("irc.twitch.tv", 6667, username, oauth);

			ReadInput();
		}

		static void ReadInput()
		{
			string line;

			while((line = Console.ReadLine()) != null)
			{
				var args = line.Split(' ');

				if(args.Length < 1)
					continue;

				var iter = Handler.CommandHandler.GetCommandIndex(args[0].ToLower());
				var command = iter.Key;

				if(command == null)
				{
					Log.Error("Unknown command: " + args[0]);
					continue;
				}

				var funcArgs = args.Skip(1).ToArray();

				if(funcArgs.Length < command.MinArgs)
				{
					Log.Error(command.MinArgs + " args expected. Got " + funcArgs.Length);
					continue;
				}

				iter.Value.Invoke(funcArgs);
			}
		}
	}
}