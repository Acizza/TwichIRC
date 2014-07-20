using System;

namespace TwitchIRC
{
	public class TwitchHandler : AClientHandler
	{
		public TwitchHandler()
		{
			CommandHandler = new TwitchCommands();
		}

		public override void PostInit()
		{
		}

		public override void OnConnect(string user, string host, int port)
		{
			ConsoleUtil.WriteLine(ConsoleColor.DarkGreen, "Connected as " + user);
		}

		public override void OnJoin(string user, string channel)
		{
			ConsoleUtil.WriteLine(ConsoleColor.DarkMagenta, user + " joined " + channel);
		}

		public override void OnMessage(string user, string channel, string msg)
		{
			if(msg.StartsWith("HISTORYEND ") || msg.StartsWith("USERCOLOR ") || msg.StartsWith("SPECIALUSER "))
				return;

			ConsoleUtil.WriteLine(ConsoleColor.DarkCyan, "<" + channel + "> " + user + ": " + msg);
		}

		public override void OnLeave(string user, string channel)
		{
			ConsoleUtil.WriteLine(ConsoleColor.DarkRed, user + " left " + channel);
		}

		public override void OnDisconnect(string user)
		{

		}

		public override void OnUnknown(string line)
		{
			#if DEBUG
			Console.WriteLine("UNKNOWN: " + line);
			#endif
		}
	}

	public class TwitchCommands : ACommandHandler
	{
		public TwitchCommands() : base()
		{
		}

		[Command("help", 1, "Command help.")]
		public void HelpCommand(string[] args)
		{
			var command = GetCommandIndex(args[0]).Key;

			if(command == null)
			{
				Log.Error("Unknown command: " + args[0]);
				return;
			}

			if(string.IsNullOrEmpty(command.Desc))
			{
				Log.Info(command.Name + " has no description.");
				return;
			}

			ConsoleUtil.WriteLine(ConsoleColor.Magenta, command.Desc);
		}

		[Command("commands", 0, "Lists all commands.")]
		public void CommandsCommand(string[] args)
		{
			uint index = 0;

			foreach(var iter in Commands)
			{
				++index;

				ConsoleUtil.Write(ConsoleColor.Red, index.ToString() + ". ");
				ConsoleUtil.WriteLine(ConsoleColor.Gray, iter.Key.Name);
			}
		}

		[Command("exit", 0, "Exits.")]
		public void ExitCommand(string[] args)
		{
			Environment.Exit(0);
		}

		[Command("close", 0, "Exits.")]
		public void CloseCommand(string[] args)
		{
			ExitCommand(null);
		}

		[Command("join", 1, "Joins channel. Multiple channels can be specified with spaces.")]
		public void JoinCommand(string[] args)
		{
			foreach(var arg in args)
				Program.Handler.Client.Join(arg);
		}

		[Command("channels", 0, "Lists all connected channels.")]
		public void ChannelsCommand(string[] args)
		{
			if(Program.Handler.Client.Channels.Count < 1)
			{
				Log.Warning("Not connected to any channels.");
				return;
			}

			uint index = 0;

			foreach(var channel in Program.Handler.Client.Channels)
			{
				++index;

				ConsoleUtil.Write(ConsoleColor.Red, index.ToString() + ". ");
				ConsoleUtil.WriteLine(ConsoleColor.Gray, channel);
			}
		}

		[Command("leave", 1, "Leaves channel. Multiple channels can be specified with spaces.")]
		public void LeaveCommand(string[] args)
		{
			foreach(var arg in args)
			{
				if(!Program.Handler.Client.Channels.Contains(arg))
				{
					Log.Warning("Not connected to channel: " + arg);
					continue;
				}

				Program.Handler.Client.Leave(arg);
			}
		}
	}
}