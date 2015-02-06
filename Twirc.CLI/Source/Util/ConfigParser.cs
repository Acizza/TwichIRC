using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace Twirc.CLI.Util
{
	public sealed class ConfigParser
	{
		public const string SettingsFile = "settings.cfg";

		/// <summary>
		/// The char used to separate parameter names that do the same thing.
		/// </summary>
		public char NameSeparator = '|';

		/// <summary>
		/// The char used to identify parameters from other arguments.
		/// </summary>
		public const char ArgSpecifier  = '-';

		/// <summary>
		/// The list of options currently being used for parsing.
		/// </summary>
		/// <value>The options.</value>
		public ReadOnlyDictionary<string, ConfigDescription> Options
		{
			get { return new ReadOnlyDictionary<string, ConfigDescription>(_settings); }
		}

		/// <summary>
		/// Called when an argument that isn't an option is encountered while parsing.
		/// </summary>
		public event Action<string> OnOtherArgument;

		public ConfigDescription this[string name]
		{
			get { return _settings[name]; }
			set { Add(name, value); }
		}

		private Dictionary<string, ConfigDescription> _settings;

		public ConfigParser()
		{
			_settings = new Dictionary<string, ConfigDescription>();
		}

		/// <summary>
		/// Adds a new setting to the parser.
		/// </summary>
		/// <param name="name">Setting name. Separated by NameSeparator.</param>
		/// <param name="callback">Callback.</param>
		/// <param name="description">Description.</param>
		public void Add(string name, Action<string> callback, string description)
		{
			Add(name, new ConfigDescription(callback, description));
		}

		/// <summary>
		/// Adds a new setting to the parser.
		/// </summary>
		/// <param name="name">Setting name.</param>
		/// <param name="desc">Setting information.</param>
		public void Add(string name, ConfigDescription desc)
		{
			desc.FullName = name;

			foreach(var uniqueName in name.Split(NameSeparator))
				_settings[uniqueName] = desc;
		}

		/// <summary>
		/// Parses the specified line for settings and executes them.
		/// </summary>
		/// <param name="line">Line to parse.</param>
		public void Parse(string line)
		{
			_Parse(line, false);
		}

		private void _Parse(string line, bool isFile)
		{
			if(String.IsNullOrWhiteSpace(line))
				return;

			if(!isFile && OnOtherArgument != null)
			{
				// Process the list of other arguments.
				line.Split(' ', '\n')
					.Where(x => x.Length > 0 && x[0] != ArgSpecifier)
					.ToList()
					.ForEach(OnOtherArgument.Invoke);
			}

			// (\w+)\s*={0,1}\s*(.+?)(?=\s+|$)

			var matches = Regex.Matches(line,
				              (isFile ? "" : ArgSpecifier + "+") + @"(\w+)\s*={0,1}\s*(.+?(?=\s+|$))?",
				              RegexOptions.Multiline | RegexOptions.Compiled);

			foreach(Match match in matches)
			{
				var name = match.Groups[1].Value;

				if(!_settings.ContainsKey(name))
				{
					Console.ForegroundColor = ConsoleColor.Yellow;
					Console.WriteLine("WARNING: Unknown command: " + name);
					Console.ResetColor();

					continue;
				}

				var value = match.Groups.Count > 2 ? match.Groups[2].Value : "";

				_settings[name].Callback.Invoke(value);
			}
		}

		/// <summary>
		/// Parses the specified file for settings and executes them.
		/// </summary>
		/// <param name="path">File to parse.</param>
		public void ParseFile(string path)
		{
			_Parse(File.ReadAllText(path), true);
		}

		/// <summary>
		/// Parses the settings file (if it exists) for settings and executes them.
		/// </summary>
		public void ParseSettingsFile()
		{
			var path = Path.Combine(Environment.CurrentDirectory, SettingsFile);

			if(File.Exists(path))
				ParseFile(path);
		}

		/// <summary>
		/// Prints detailed information about every command.
		/// </summary>
		public void PrintHelp()
		{
			Console.WriteLine("Specify setting values by placing a space or equal sign after typing the command.");
			Console.WriteLine("Commands separated by \"{0}\" do the same thing.\n", NameSeparator);

			// The LINQ statement only selects distinct command names.
			foreach(var option in _settings.GroupBy(x => x.Value.FullName).Select(x => x.First()))
			{
				Console.WriteLine("{0}{1}:\n\tDescription: {2}\n",
					ArgSpecifier,
					option.Value.FullName,
					option.Value.Description);
			}
		}
	}
}