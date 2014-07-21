using System;
using System.Collections.Generic;

namespace TwitchIRC
{
	using CommandDict = Dictionary<Command, Action<string[]>>;

	public abstract class ACommandHandler
	{
		public CommandDict Commands { get; protected set; }

		public ACommandHandler()
		{
			Commands = GetCommands();
		}

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