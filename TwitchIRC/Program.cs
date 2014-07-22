using System;
using System.IO;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace TwitchIRC
{
	public class Program
	{
		public static AClientHandler Handler { get; private set; }

		static void Main(string[] args)
		{
			string username = null;
			string oauth    = null;

			try
			{
				var client = new IrcClient(Handler = new TwitchHandler());
				var parser = new ArgParser();

				parser.Add("file",     v => parser.Parse(File.ReadAllLines(v)));
				parser.Add("username", v => username = v);
				parser.Add("oauth",    v => oauth = v);

				parser.Parse(args);

				if(username != null && oauth != null) // Temporary
					client.Connect("199.9.252.120", 6667, username, oauth);

				ReadInput();
			}
			catch(Exception e)
			{
				#if DEBUG
				Log.Error(e.ToString());
				#else
				Log.Error(e.Message + "\n" + e.StackTrace);
				#endif
			}
		}

		static void ReadInput()
		{
			string line;

			while((line = Console.ReadLine()) != null)
			{
				var args = line.Split(' ');

				if(args.Length < 1)
					continue;

				var iter = Handler.GetCommandIndex(args[0].ToLower());
				var command = iter.Key;

				if(command == null)
				{
					Log.Error("Unknown command: " + args[0]);
					continue;
				}

				var funcArgs = args.Skip(1).ToArray();

				if(funcArgs.Length < command.MinArgs)
				{
					var builder = new StringBuilder();

					builder.Append(command.MinArgs);
					builder.Append(" args expected. Got ");
					builder.Append(funcArgs.Length);

					if(!string.IsNullOrEmpty(command.ArgDesc))
					{
						builder.Append("\nUsage: ");
						builder.Append(command.ArgDesc);
					}

					Log.Error(builder.ToString());
					continue;
				}

				iter.Value.Invoke(funcArgs);
			}
		}
	}
}