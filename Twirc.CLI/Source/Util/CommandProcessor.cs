using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Reflection;

namespace Twirc.CLI.Util
{
	public static class CommandProcessor
	{
		public delegate void CommandDel(string[] args);

		public static ReadOnlyDictionary<string, Tuple<CommandAttribute, CommandDel>> Commands
		{
			get { return new ReadOnlyDictionary<string, Tuple<CommandAttribute, CommandDel>>(_commands); }
		}

		private static readonly Dictionary<string, Tuple<CommandAttribute, CommandDel>> _commands;

		static CommandProcessor()
		{
			_commands = new Dictionary<string, Tuple<CommandAttribute, CommandDel>>();
			FindAll();
		}

		/// <summary>
		/// Finds all static methods marked with a CommandAttribute and adds them to the active list of commands.
		/// </summary>
		public static void FindAll()
		{
			var types = from type in Assembly.GetExecutingAssembly().GetTypes()
			            from method in type.GetMethods(BindingFlags.Public | BindingFlags.Static)
			            let attr = method.GetCustomAttribute<CommandAttribute>(true)
			            where attr != null
			            select new { Attribute = attr, Method = method };

			_commands.Clear();

			foreach(var type in types)
			{
				_commands[type.Attribute.Name] =
					Tuple.Create(type.Attribute, (CommandDel)Delegate.CreateDelegate(typeof(CommandDel), type.Method));
			}
		}

		/// <summary>
		/// Processes the specified line and executes any found command from it.
		/// </summary>
		/// <param name="line">Line.</param>
		public static Tuple<string, bool> Process(string line)
		{
			var lineArgs = line.Split(' ');
			var comName  = lineArgs[0].ToLower();

			if(!_commands.ContainsKey(comName))
				return Tuple.Create("Unknown command: " + comName, false);

			var comArgs = lineArgs.Skip(1).ToArray();
			var command = _commands[comName];

			if(comArgs.Length < command.Item1.MinimumArguments)
			{
				return Tuple.Create(String.Format(
					"Expected {0} arguments, got {1}.",
					command.Item1.MinimumArguments,
					comArgs.Length),
					false);
			}

			command.Item2.Invoke(comArgs);

			return Tuple.Create("", true);
		}
	}
}