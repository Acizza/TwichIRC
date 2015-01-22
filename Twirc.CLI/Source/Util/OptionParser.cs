using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;

namespace Twirc.CLI.Util
{
	public sealed class OptionParser
	{
		/// <summary>
		/// The char used to separate parameter names that do the same thing.
		/// </summary>
		public const char NameSeparator = '|';

		/// <summary>
		/// The char used to identify parameters from other arguments.
		/// </summary>
		public const char ArgSpecifier  = '-';

		/// <summary>
		/// The list of options currently being used for parsing.
		/// </summary>
		/// <value>The options.</value>
		public ReadOnlyDictionary<string, OptionDesc> Options
		{
			get
			{
				return new ReadOnlyDictionary<string, OptionDesc>(_options);
			}
		}

		/// <summary>
		/// Called when an argument that isn't an option is encountered while parsing.
		/// </summary>
		public event Action<string> OnOtherArgument = delegate {};

		public OptionDesc this[string name]
		{
			get
			{
				return _options[name];
			}
			set
			{
				var optionDesc      = value;
				optionDesc.FullName = name;

				foreach(var splitName in name.Split(NameSeparator))
					_options[splitName] = optionDesc;
			}
		}

		private Dictionary<string, OptionDesc> _options;

		public OptionParser()
		{
			_options = new Dictionary<string, OptionDesc>();
		}

		/// <summary>
		/// Parse the specified arguments for parameters and execute them.
		/// </summary>
		/// <param name="args">Arguments to parse.</param>
		public void Parse(string[] args)
		{
			for(uint i = 0; i < args.Length; ++i)
			{
				var arg = args[i];

				if(arg[0] != ArgSpecifier)
				{
					OnOtherArgument.Invoke(arg);
					continue;
				}

				// Skip the arg specifier
				var name = arg.Substring(1);

				if(!_options.ContainsKey(name))
				{
					Console.ForegroundColor = ConsoleColor.Yellow;
					Console.WriteLine("WARNING: Unknown command: " + name);
					Console.ResetColor();

					continue;
				}

				// Increase i if the next iteration won't overflow the argument list.
				if(i + 1 < args.Length)
					++i;

				_options[name].Function.Invoke(args[i]);
			}
		}

		/// <summary>
		/// Prints detailed information about every command.
		/// </summary>
		public void PrintHelp()
		{
			Console.WriteLine("Specify command values by placing a space after typing the command.");
			Console.WriteLine("Commands separated by \"{0}\" do the same thing.\n", NameSeparator);

			// The LINQ statement only selects distinct command names.
			foreach(var option in _options.GroupBy(x => x.Value.FullName).Select(x => x.First()))
			{
				Console.WriteLine("{0}{1}:\n\tDescription: {2}\n",
					ArgSpecifier,
					option.Value.FullName,
					option.Value.Description);
			}
		}
	}
}

