using System;
using System.Linq;
using Twirc.CLI.Util;

namespace Twirc.CLI
{
	public static class Commands
	{
		[Command("login", 2, "<user> <pass>", "Login with the specified information.")]
		public static void Login(string[] args)
		{
			if(!Program.Client.Alive)
				Program.Client.Reconnect();

			Program.Client.Login(args[0], args[1]);
			Program.StartProcessing();
		}

		[Command("join", 1, "<channels>", "Joins all specified channels separated by space.")]
		public static void Join(string[] args)
		{
			if(!Program.Client.LoggedIn)
			{
				PrintNotLoggedInError();
				return;
			}

			foreach(var channel in args)
			{
				if(Program.Client.IsConnectedTo(channel))
				{
					Program.WriteTime();
					Program.Write(ConsoleColor.White, "Already connected to ");
					Program.WriteLine(ConsoleColor.DarkYellow, channel);

					continue;
				}

				Program.Client.Join(channel);
			}
		}

		[Command("leave", 1, "<channels>", "Leaves all specified channels separated by space.")]
		public static void Leave(string[] args)
		{
			if(!Program.Client.LoggedIn)
			{
				PrintNotLoggedInError();
				return;
			}

			foreach(var channel in args)
			{
				if(!Program.Client.IsConnectedTo(channel))
				{
					PrintChannelNotFoundError(channel);
					continue;
				}

				Program.Client.Leave(channel);
			}
		}

		[Command("send", 2, "<channel> <message>", "Sends a message to the specified channel.")]
		public static void Send(string[] args)
		{
			if(!Program.Client.SendMessage(args[0], String.Join(" ", args.Skip(1).ToArray())))
				PrintChannelNotFoundError(args[0]);
		}

		[Command("channels", 0, "", "Lists all currently connected channels.")]
		public static void Channels(string[] args)
		{
			uint index = 1;

			foreach(var channel in Program.Client.Channels)
			{
				Program.Write(ConsoleColor.DarkYellow, index.ToString());
				Program.WriteLine(ConsoleColor.White, ". " + channel.Name);

				++index;
			}
		}

		[Command("users", 1, "<channel>", "Lists all users connected to the specified channel.")]
		public static void Viewers(string[] args)
		{
			var channel = Program.Client.GetChannelByName(args[0]);

			if(channel == null)
			{
				PrintChannelNotFoundError(args[0]);
				return;
			}

			uint index = 1;

			foreach(var user in channel.Users)
			{
				Program.Write(ConsoleColor.DarkYellow, index.ToString());
				Program.WriteLine(ConsoleColor.White, ". " + user.Name);

				++index;
			}
		}

		[Command("numusers", 1, "<channel>", "Prints the number of users connected to the specified channel.")]
		public static void NumUsers(string[] args)
		{
			var channel = Program.Client.GetChannelByName(args[0]);

			if(channel == null)
			{
				PrintChannelNotFoundError(args[0]);
				return;
			}

			Program.WriteFmt(ConsoleColor.DarkYellow, "[{0}] {1}", Program.GetTime(), channel.Name);
			Program.Write(ConsoleColor.White, " has ");
			Program.Write(ConsoleColor.DarkYellow, channel.Users.Count.ToString());
			Program.WriteLine(ConsoleColor.White, " viewer(s)");
		}

		[Command("hasuser", 2, "<channel> <username>",
			"Outputs whether or not the specified user is connected to the specified channel.")]
		public static void HasViewer(string[] args)
		{
			var channel = Program.Client.GetChannelByName(args[0]);

			if(channel == null)
			{
				PrintChannelNotFoundError(args[0]);
				return;
			}

			var viewerName   = String.Join(" ", args.Skip(1).ToArray());
			bool isConnected = channel.HasUser(viewerName);

			Program.WriteTime();
			Program.Write(ConsoleColor.White, "User ");
			Program.Write(ConsoleColor.DarkYellow, viewerName);
			Program.Write(ConsoleColor.White, (isConnected ? " is " : " isn't ") + "connected to ");
			Program.WriteLine(ConsoleColor.DarkYellow, args[0]);
		}

		[Command("help", 1, "<command>", "Displays the information for the specified command.")]
		public static void Help(string[] args)
		{
			if(!CommandProcessor.Commands.ContainsKey(args[0]))
			{
				Program.WriteTime();
				Program.Write(ConsoleColor.White, "The specified command ");
				Program.Write(ConsoleColor.DarkYellow, args[0]);
				Program.WriteLine(ConsoleColor.White, " does not exist");

				return;
			}

			var command = CommandProcessor.Commands[args[0]];

			Program.Write(ConsoleColor.White, "Information for ");
			Program.Write(ConsoleColor.DarkYellow, args[0]);
			Program.Write(ConsoleColor.White, ":\n\tUsage: ");
			Program.Write(ConsoleColor.DarkYellow, command.Item1.Usage + "\n\t");
			Program.Write(ConsoleColor.White, "Desc: ");
			Program.WriteLine(ConsoleColor.DarkYellow, command.Item1.Description);
		}

		[Command("commands", 0, "", "Lists all available commands.")]
		public static void PrintCommands(string[] args)
		{
			uint index = 1;

			foreach(var command in CommandProcessor.Commands)
			{
				Program.Write(ConsoleColor.DarkYellow, index.ToString());
				Program.WriteLine(ConsoleColor.White, ". " + command.Key);

				++index;
			}
		}

		[Command("logout", 0, "", "Logs out the current user.")]
		public static void Logout(string[] args)
		{
			if(!Program.Client.LoggedIn)
			{
				PrintNotLoggedInError();
				return;
			}

			Program.Client.Logout();
		}

		[Command("exit", 0, "", "Exits the program.")]
		public static void Exit(string[] args)
		{
			Environment.Exit(0);
		}

		private static void PrintChannelNotFoundError(string channel)
		{
			Program.WriteTime();
			Program.Write(ConsoleColor.White, "Not connected to ");
			Program.WriteLine(ConsoleColor.DarkYellow, channel);
		}

		private static void PrintNotLoggedInError()
		{
			Program.WriteTime();
			Program.WriteLine(ConsoleColor.White, "Not logged in");
		}
	}
}