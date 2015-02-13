using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Reflection;

namespace Twirc.CLI.Util
{
	using CommandDict = Dictionary<string, Tuple<CommandAttribute, CommandParser.CommandDel>>;

	public static class CommandParser
	{
		public delegate void CommandDel(string[] args);

		public static ReadOnlyDictionary<string, Tuple<CommandAttribute, CommandDel>> Commands
		{
			get { return new ReadOnlyDictionary<string, Tuple<CommandAttribute, CommandDel>>(_commands); }
		}

		private static readonly CommandDict _commands;

		static CommandParser()
		{
			_commands = new CommandDict();
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
				_commands[type.Attribute.Name] = Tuple.Create(
					type.Attribute,
					(CommandDel)Delegate.CreateDelegate(typeof(CommandDel), type.Method));
			}
		}

		/// <summary>
		/// Processes the specified line and executes any found command from it.
		/// </summary>
		/// <param name="line">Line of data to process.</param>
		/// <returns>null if the command executed successfully; otherwise, the error message.</returns>
		public static string Process(string line)
		{
			var lineArgs = line.Split(' ');
			var comName  = lineArgs[0].ToLower();

			if(!_commands.ContainsKey(comName))
				return "Unknown command: " + comName;

			var comArgs = lineArgs.Skip(1).ToArray();
			var command = _commands[comName];

			if(comArgs.Length < command.Item1.MinimumArguments)
			{
				return String.Format(
					"Expected {0} arguments, got {1}.\nUsage: {2}",
					command.Item1.MinimumArguments,
					comArgs.Length,
					command.Item1.Usage);
			}

			command.Item2.Invoke(comArgs);

			return null;
		}
	}
}