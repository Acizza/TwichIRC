using System;
using Twirc.CLI.Util;

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

		[Command("exit", 0, "", "Exits the program.")]
		public static void Exit(string[] args)
		{
			Environment.Exit(0);
		}
	}
}