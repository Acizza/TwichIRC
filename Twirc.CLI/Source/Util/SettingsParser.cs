using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace Twirc.CLI.Util
{
	public sealed class SettingsParser
	{
		public string SettingsFile = "settings.cfg";

		/// <summary>
		/// The char used to identify parameters from other arguments.
		/// </summary>
		public const char ArgSpecifier = '-';

		/// <summary>
		/// The list of options currently being used for parsing.
		/// </summary>
		/// <value>The options.</value>
		public Dictionary<string, SettingInformation> Settings;

		/// <summary>
		/// Called when an argument that isn't an option is encountered while parsing.
		/// </summary>
		public event Action<string> OnOtherArgument;

		public string this[string name]
		{
			get { return Settings.ContainsKey(name) ? Settings[name].Value : null; }
			set { Add(name, value); }
		}

		public SettingsParser()
		{
			Settings = new Dictionary<string, SettingInformation>();
		}

		/// <summary>
		/// Adds a new setting to the parser.
		/// </summary>
		/// <param name="name">Setting name. Separated by NameSeparator.</param>
		/// <param name="callback">Callback.</param>
		/// <param name="description">Description.</param>
		public void Add(string name, Action<string> callback, string description)
		{
			Add(name, new SettingInformation(callback, description));
		}

		/// <summary>
		/// Adds a new setting to the parser.
		/// </summary>
		/// <param name="name">Setting name.</param>
		/// <param name="description">Setting description.</param>
		public void Add(string name, string description)
		{
			Add(name, new SettingInformation(null, description));
		}

		/// <summary>
		/// Adds a new setting to the parser.
		/// </summary>
		/// <param name="name">Setting name.</param>
		/// <param name="desc">Setting information.</param>
		public void Add(string name, SettingInformation desc)
		{
			Settings[name] = desc;
		}

		/// <summary>
		/// Tries to retrieve a value from the list of settings. If the specified setting isn't found, it returns defaultValue.
		/// </summary>
		/// <returns>The retrieved value, or defaultValue.</returns>
		/// <param name="name">Setting name.</param>
		/// <param name="defaultValue">Return value to use if the setting isn't found.</param>
		/// <typeparam name="T">The type to return the result as.</typeparam>
		public T TryGet<T>(string name, T defaultValue = default(T)) where T : IConvertible
		{
			if(!Settings.ContainsKey(name))
				return defaultValue;

			if(String.IsNullOrEmpty(Settings[name].Value))
				return defaultValue;

			return (T)Convert.ChangeType(Settings[name].Value, typeof(T));
		}

		/// <summary>
		/// Parses the specified line for settings and executes them.
		/// </summary>
		/// <param name="line">Line to parse.</param>
		public void Parse(string[] args)
		{
			_Parse(args, false);
		}

		private void _Parse(string[] args, bool isFile)
		{
			if(args.Length == 0)
				return;

			foreach(var arg in args)
			{
				var match = Regex.Match(arg,
					            (isFile ? "" : ArgSpecifier + "+") + @"(\w+)\s*={0,1}\s*(.+?(?=\s+|$))?",
					            RegexOptions.Compiled);

				if(!match.Success)
				{
					OnOtherArgument.Invoke(arg);
					continue;
				}

				var name = match.Groups[1].Value;

				if(!Settings.ContainsKey(name))
				{
					Console.ForegroundColor = ConsoleColor.Yellow;
					Console.WriteLine("WARNING: Unknown command: " + name);
					Console.ResetColor();

					continue;
				}

				var value = match.Groups[2].Value;
				var setting = Settings[name];

				setting.Value = value;

				if(setting.Callback != null)
					setting.Callback.Invoke(value);
			}
		}

		/// <summary>
		/// Parses the specified file for settings and executes them.
		/// </summary>
		/// <param name="path">File to parse.</param>
		public void ParseFile(string path)
		{
			_Parse(File.ReadAllLines(path), true);
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
		/// Writes all non-empty settings to a file.
		/// </summary>
		/// <param name="path">Settings file path.</param>
		public void WriteAll(string path)
		{
			var setSettings = Settings.Where(x => !String.IsNullOrEmpty(x.Value.Value));

			if(!setSettings.Any())
				return;

			File.WriteAllLines(path, setSettings
				.Select(x => String.Format("{0} = {1}", x.Key, x.Value.Value)));
		}

		/// <summary>
		/// Writes all non-empty settings to a file.
		/// </summary>
		public void WriteAll()
		{
			WriteAll(SettingsFile);
		}

		/// <summary>
		/// Prints detailed information about every command.
		/// </summary>
		public void PrintHelp()
		{
			Console.WriteLine("Specify setting values by placing a space or equal sign after typing the command.");

			foreach(var option in Settings)
			{
				Console.WriteLine("{0}{1}:\n\tDescription: {2}\n",
					ArgSpecifier,
					option.Key,
					option.Value.Description);
			}
		}
	}
}