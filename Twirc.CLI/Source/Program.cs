﻿using System;
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
			var config = new ConfigParser();

			string username = null;
			string password = null;
			bool autoLogin  = true;
			_joinChannels   = new List<string>();

			config.Add("u|user|username", v => username = v, "Username to use for login.");
			config.Add("p|pass|oauth|o|password", v => password = v, "The password / oauth key to use for login.");
			config.Add("f|file|parse", config.ParseFile, "Parses a configuration file.");
			config.Add("j|join", _joinChannels.Add, "Joins the specified channel on login.");
			config.Add("a|al|autologin|auto-login", v => autoLogin = bool.Parse(v), "Enables / disables auto-login.");
			config.Add("h|help", v =>
			{
				config.PrintHelp();
				Environment.Exit(0);
			}, "Prints information about all command-line parameters.");

			config.OnOtherArgument += _joinChannels.Add;

			config.ParseSettingsFile();
			config.Parse(args);

			RunClient(username, password, autoLogin);
		}

		/// <summary>
		/// Starts a new thread that will process every line from the IRC client until it is closed.
		/// </summary>
		public static void StartProcessing()
		{
			var processThread = new Thread(_ =>
			{
				while(Client.Alive)
					Client.ProcessNextLine();
			});

			processThread.IsBackground = true;
			processThread.Start();
		}

		private static void RunClient(string username, string password, bool autoLogin)
		{
			// This address can also be used: 199.9.250.117:443
			using(Client = new IRCClient("irc.twitch.tv", 6667))
			{
				InitializeClient(Client);

				if(autoLogin && !String.IsNullOrEmpty(username) && !String.IsNullOrEmpty(password))
				{
					Client.Login(username, password);
					StartProcessing();
				}
				else
				{
					Console.WriteLine("Tip: use the \"login\" command (help login) or \"commands\".");
				}

				while(true)
				{
					var result = CommandProcessor.Process(Console.ReadLine());

					if(!result.Item2)
						WriteLine(ConsoleColor.Red, result.Item1);
				}
			}
		}

		private static void InitializeClient(IRCClient client)
		{
			client.OnLogin += (response, username) =>
			{
				if(response.Success)
				{
					WriteTime();
					Write(ConsoleColor.White, "Logged in as ");
					WriteLine(ConsoleColor.Red, username);

					foreach(var channel in _joinChannels)
						client.Join(channel);
				}
				else
				{
					WriteTime();
					Write(ConsoleColor.White, "Failed to login [");
					Write(ConsoleColor.Red, response.Code);
					Write(ConsoleColor.White, "]: ");
					WriteLine(ConsoleColor.Red, response.Message);
				}
			};

			client.OnLogout += username =>
			{
				WriteTime();
				Write(ConsoleColor.White, "User ");
				Write(ConsoleColor.DarkYellow, username);
				WriteLine(ConsoleColor.White, " logged out");
			};

			client.OnJoin += (channel, username) =>
			{
				WriteTime();
				Write(ConsoleColor.Red, username);
				Write(ConsoleColor.White, " joined ");
				WriteLine(ConsoleColor.DarkYellow, channel.Name);
			};

			client.OnLeave += (channel, username) =>
			{
				WriteTime();
				Write(ConsoleColor.Red, username);
				Write(ConsoleColor.White, " left ");
				WriteLine(ConsoleColor.DarkYellow, channel.Name);
			};

			client.OnMessage += (channel, user, message) =>
			{
				WriteFmt(ConsoleColor.DarkYellow, "[{0}] <{1}> {2}",
					GetTime(),
					channel.Name,
					user.Group == UserGroup.Moderator ? "[M] " : "");

				Write(ConsoleColor.Red, user.Name);
				WriteLine(ConsoleColor.White, ": " + message);
			};

			client.OnUserSubscribed += (channel, username) =>
			{
				WriteTime();
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

		public static void WriteTime()
		{
			WriteFmt(ConsoleColor.DarkYellow, "[{0}] ", GetTime());
		}

		public static string GetTime()
		{
			return DateTime.Now.ToString("hh:mm:ss tt");
		}
	}
}