using System;
using System.Collections.Generic;
using Twirc.CLI.Util;
using Twirc.Lib;
using System.Threading;

namespace Twirc.CLI
{
	public sealed class Program
	{
		static void Main(string[] args)
		{
			var options = new OptionParser();

			string loginUsername = null;
			string loginPassword = null;
			var joinChannels     = new List<string>();

			options["u|user|username"] = new OptionDesc(
				v => loginUsername = v,
				"Username to use for login.");

			options["p|pass|oauth|o|password"] = new OptionDesc(
				v => loginPassword = v,
				"The password / oauth key to use for login.");

			options.OnOtherArgument += joinChannels.Add;
			options.Parse(args);

			if(String.IsNullOrEmpty(loginUsername) || String.IsNullOrEmpty(loginPassword))
			{
				Console.ForegroundColor = ConsoleColor.Yellow;
				Console.WriteLine("Login username / password cannot be empty.");

				Environment.Exit(2);
			}

			// This address can also be used: 199.9.250.117:443
			using(var client = new IRCClient("irc.twitch.tv", 6667))
			{
				InitializeClient(client);

				if(client.Login(loginUsername, loginPassword).Status != LoginStatus.Success)
					return;

				foreach(var channel in joinChannels)
					client.Join(channel);

				var processThread = new Thread(_ =>
				{
					while(client.Alive)
						client.ProcessNextLine();
				});

				processThread.IsBackground = true;
				processThread.Start();
			}
		}

		static void InitializeClient(IRCClient client)
		{
			client.OnLogin += (response, username, password) =>
			{
				if(response.Status == LoginStatus.Success)
				{
					WriteFmt(ConsoleColor.DarkYellow, "[{0}] ", GetTime());
					Write(ConsoleColor.White, "Logged in as ");
					WriteLine(ConsoleColor.Red, username);
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

		static void WriteFmt(ConsoleColor color, string format, params object[] message)
		{
			Console.ForegroundColor = color;
			Console.Write(format, message);
		}

		static void WriteLineFmt(ConsoleColor color, string format, params object[] message)
		{
			Console.ForegroundColor = color;
			Console.WriteLine(format, message);
		}

		static void Write(ConsoleColor color, string message)
		{
			Console.ForegroundColor = color;
			Console.Write(message);
		}

		static void WriteLine(ConsoleColor color, string message)
		{
			Console.ForegroundColor = color;
			Console.WriteLine(message);
		}

		static string GetTime()
		{
			return DateTime.Now.ToString("hh:mm:ss tt");
		}
	}
}