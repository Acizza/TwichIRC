using System;
using Twirc.CLI.Util;
using System.Linq;

namespace Twirc.CLI
{
	public static class Commands
	{
		[Command("login", 2, "<user> <pass>", "Login with the specified information.")]
		public static void Login(string[] args)
		{
			Program.Client.Login(args[0], args[1]);
		}

		[Command("join", 1, "<channels>", "Joins all specified channels separated by space.")]
		public static void Join(string[] args)
		{
			foreach(var channel in args)
			{
				if(Program.Client.IsConnectedTo(channel))
				{
					Program.WriteFmt(ConsoleColor.DarkYellow, "[{0}] ", Program.GetTime());
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
			foreach(var channel in args)
			{
				if(!Program.Client.IsConnectedTo(channel))
				{
					Program.WriteFmt(ConsoleColor.DarkYellow, "[{0}] ", Program.GetTime());
					Program.Write(ConsoleColor.White, "Not connected to ");
					Program.WriteLine(ConsoleColor.DarkYellow, channel);

					continue;
				}

				Program.Client.Leave(channel);
			}
		}

		[Command("send", 2, "<channel> <message>", "Sends a message to the specified channel.")]
		public static void Send(string[] args)
		{
			if(!Program.Client.SendMessage(args[0], String.Join(" ", args.Skip(1).ToArray())))
			{
				Program.WriteFmt(ConsoleColor.DarkYellow, "[{0}] ", Program.GetTime());
				Program.Write(ConsoleColor.White, "Must be connected to ");
				Program.Write(ConsoleColor.DarkYellow, args[0]);
				Program.WriteLine(ConsoleColor.White, " to send a message");
			}
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

		[Command("help", 1, "<command>", "Displays the information for the specified command.")]
		public static void Help(string[] args)
		{
			if(!CommandProcessor.Commands.ContainsKey(args[0]))
			{
				Program.WriteFmt(ConsoleColor.DarkYellow, "[{0}] ", Program.GetTime());
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

		[Command("exit", 0, "", "Exits the program.")]
		public static void Exit(string[] args)
		{
			Environment.Exit(0);
		}
	}
}