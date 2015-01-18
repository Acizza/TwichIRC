using System;
using Twirc.Lib;

namespace Twirc.CLI
{
	public sealed class Program
	{
		static void Main(string[] args)
		{
			// This IP can also be used
			//using(var irc = new IRCClient("199.9.250.117", 443))

			using(var irc = new IRCClient("irc.twitch.tv", 6667))
			{
				irc.OnLogin += (response, username, password) =>
				{
					if(response.Status == LoginStatus.Success)
						Console.WriteLine("Logged in as: " + username);
					else
						Console.WriteLine("Failed to login [{0}]: {1}", response.Code, response.Message);
				};

				irc.OnPing += () =>
				{
					Console.WriteLine("Received PING request");
				};

				irc.OnJoin += (channel, username) =>
				{
					WriteFmt(ConsoleColor.DarkYellow, "[{0}] ", GetTime());
					Write(ConsoleColor.Red, username);
					Write(ConsoleColor.White, " joined ");
					WriteLine(ConsoleColor.DarkYellow, channel.Name);
				};

				irc.OnLeave += (channel, username) =>
				{
					WriteFmt(ConsoleColor.DarkYellow, "[{0}] ", GetTime());
					Write(ConsoleColor.Red, username);
					Write(ConsoleColor.White, " left ");
					WriteLine(ConsoleColor.DarkYellow, channel.Name);
				};

				irc.OnMessage += (channel, user, message) =>
				{
					WriteFmt(ConsoleColor.DarkYellow, "[{0}] <{1}> ",
						GetTime(),
						channel.Name);

					Write(ConsoleColor.Red, user);
					WriteLine(ConsoleColor.White, ": " + message);
				};

				irc.OnUserSubscribed += (channel, username) =>
				{
					WriteFmt(ConsoleColor.DarkYellow, "[{0}] ", GetTime());
					Write(ConsoleColor.White, "User ");
					Write(ConsoleColor.Red, username);
					Write(ConsoleColor.White, " subscribed to ");
					WriteLine(ConsoleColor.DarkYellow, channel.Name);
				};

				irc.Login("<username>", "<oauth>");
				irc.Join("<channel>");

				if(!irc.LoggedIn)
					return;

				while(irc.Alive)
					irc.ProcessNextLine();
			}
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