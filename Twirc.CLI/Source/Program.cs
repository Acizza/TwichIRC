using System;
using System.Collections.Generic;
using System.Threading;
using Twirc.CLI.Util;
using Twirc.Lib;

namespace Twirc.CLI
{
	public static class Program
	{
		public static IRCClient Client { get; private set; }

		private static List<string> _joinChannels;

		private static void Main(string[] args)
		{
			var options = new OptionParser();

			string username = null;
			string password = null;
			_joinChannels   = new List<string>();

			options["u|user|username"] = new OptionDesc(
				v => username = v,
				"Username to use for login.");

			options["p|pass|oauth|o|password"] = new OptionDesc(
				v => password = v,
				"The password / oauth key to use for login.");

			options.OnOtherArgument += _joinChannels.Add;
			options.Parse(args);

			RunClient(username, password);
		}

		private static void RunClient(string username, string password)
		{
			// This address can also be used: 199.9.250.117:443
			using(Client = new IRCClient("irc.twitch.tv", 6667))
			{
				InitializeClient(Client);

				if(!String.IsNullOrEmpty(username) && !String.IsNullOrEmpty(password))
					Client.Login(username, password);

				var processThread = new Thread(_ =>
				{
					while(Client.Alive)
						Client.ProcessNextLine();
				});

				processThread.IsBackground = true;
				processThread.Start();

				while(true)
				{
					var result = CommandProcessor.Process(Console.ReadLine());

					if(!result.Item2)
						WriteLine(ConsoleColor.Red, "ERROR: " + result.Item1);
				}
			}
		}

		private static void InitializeClient(IRCClient client)
		{
			client.OnLogin += (response, username) =>
			{
				if(response.Success)
				{
					WriteFmt(ConsoleColor.DarkYellow, "[{0}] ", GetTime());
					Write(ConsoleColor.White, "Logged in as ");
					WriteLine(ConsoleColor.Red, username);

					foreach(var channel in _joinChannels)
						client.Join(channel);

					// No longer needed.
					_joinChannels = null;
				}
				else
				{
					WriteFmt(ConsoleColor.DarkYellow, "[{0}] ", GetTime());
					Write(ConsoleColor.White, "Failed to login [");
					Write(ConsoleColor.Red, response.Code);
					Write(ConsoleColor.White, "]: ");
					WriteLine(ConsoleColor.Red, response.Message);
				}
			};

			client.OnJoin += (channel, username) =>
			{
				WriteFmt(ConsoleColor.DarkYellow, "[{0}] ", GetTime());
				Write(ConsoleColor.Red, username);
				Write(ConsoleColor.White, " joined ");
				WriteLine(ConsoleColor.DarkYellow, channel.Name);
			};

			client.OnLeave += (channel, username) =>
			{
				WriteFmt(ConsoleColor.DarkYellow, "[{0}] ", GetTime());
				Write(ConsoleColor.Red, username);
				Write(ConsoleColor.White, " left ");
				WriteLine(ConsoleColor.DarkYellow, channel.Name);
			};

			client.OnMessage += (channel, user, message) =>
			{
				WriteFmt(ConsoleColor.DarkYellow, "[{0}] <{1}> ",
					GetTime(),
					channel.Name);

				Write(ConsoleColor.Red, user);
				WriteLine(ConsoleColor.White, ": " + message);
			};

			client.OnUserSubscribed += (channel, username) =>
			{
				WriteFmt(ConsoleColor.DarkYellow, "[{0}] ", GetTime());
				Write(ConsoleColor.White, "User ");
				Write(ConsoleColor.Red, username);
				Write(ConsoleColor.White, " subscribed to ");
				WriteLine(ConsoleColor.DarkYellow, channel.Name);
			};
		}

		public static void WriteFmt(ConsoleColor color, string format, params object[] message)
		{
			Console.ForegroundColor = color;
			Console.Write(format, message);
			Console.ResetColor();
		}

		public static void WriteLineFmt(ConsoleColor color, string format, params object[] message)
		{
			Console.ForegroundColor = color;
			Console.WriteLine(format, message);
			Console.ResetColor();
		}

		public static void Write(ConsoleColor color, string message)
		{
			Console.ForegroundColor = color;
			Console.Write(message);
			Console.ResetColor();
		}

		public static void WriteLine(ConsoleColor color, string message)
		{
			Console.ForegroundColor = color;
			Console.WriteLine(message);
			Console.ResetColor();
		}

		public static string GetTime()
		{
			return DateTime.Now.ToString("hh:mm:ss tt");
		}
	}
}