using System;
using System.Collections.Generic;

namespace TwitchIRC
{
	using CommandDict = Dictionary<Command, Action<string[]>>;

	public abstract class AClientHandler
	{
		public IrcClient Client { get; set; }
		public CommandDict Commands { get; protected set; }

		public AClientHandler()
		{
			Commands = GetCommands();
		}

		/// <summary>
		/// Called after Client has been initialized.
		/// </summary>
		public virtual void PostInit() {}

		public virtual void OnConnect(string user, string host, int port) {}
		public virtual void OnJoin(string user, string channel) {}
		public virtual void OnMessage(string user, string channel, string msg) {}
		public virtual void OnLeave(string user, string channel) {}
		public virtual void OnDisconnect(string user) {}
		public virtual void OnUnknown(string line) {}

		public CommandDict GetCommands()
		{
			var list = new CommandDict();

			foreach(var method in this.GetType().GetMethods())
			{
				foreach(var attr in method.GetCustomAttributes(true))
				{
					if(!(attr is Command))
						continue;

					list.Add((Command)attr, (Action<string[]>)Delegate.CreateDelegate(typeof(Action<string[]>), this, method));
				}
			}

			return list;
		}

		public KeyValuePair<Command, Action<string[]>> GetCommandIndex(string name)
		{
			foreach(var iter in Commands)
			{
				if(iter.Key.Name == name)
					return iter;
			}

			return new KeyValuePair<Command, Action<string[]>>(null, null);
		}

		[Command("help", 1, "<command>", "Command help.")]
		public void HelpCommand(string[] args)
		{
			var command = GetCommandIndex(args[0]).Key;

			if(command == null)
			{
				Log.Error("Unknown command: " + args[0]);
				return;
			}

			if(String.IsNullOrEmpty(command.Desc))
			{
				Log.Info(command.Name + " has no description.");
				return;
			}

			ConsoleUtil.WriteLine(ConsoleColor.Magenta, "Desc: " + command.Desc);

			if(!String.IsNullOrEmpty(command.ArgDesc))
				ConsoleUtil.WriteLine(ConsoleColor.Magenta, "Usage: " + command.ArgDesc);
		}

		[Command("commands", 0, "", "Lists all commands.")]
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
	}

	public class Command : Attribute
	{
		public string Name { get; private set; }
		public string ArgDesc { get; private set; }
		public string Desc { get; private set; }
		public uint MinArgs { get; private set; }

		public Command(string name, uint minArgs = 0, string argDesc = "", string desc = "")
		{
			Name = name;
			Desc = desc;
			ArgDesc = argDesc;
			MinArgs = minArgs;
		}
	}
}