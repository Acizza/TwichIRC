using System;

namespace TwitchIRC
{
	public class TwitchHandler : AClientHandler
	{
		public TwitchHandler()
		{
			CommandHandler = new TwitchCommands();
		}

		public override void ProcessLine(string line)
		{
			Console.WriteLine(line);
		}
	}

	public class TwitchCommands : ACommandHandler
	{
		public TwitchCommands() : base()
		{
		}

		[Command("test", 1, "Test command.")]
		public void TestCommand(string[] args)
		{
			Console.WriteLine("TEST: " + args[0]);
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
				Log.Warning(command.Name + " has no description.");
				return;
			}

			Console.WriteLine(command.Desc);
		}
	}
}