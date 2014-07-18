using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;

namespace TwitchIRC
{
	public class ArgParser
	{
		public delegate void Callback(string value);
		public Dictionary<string, Callback> Callbacks { get; private set; }

		public ArgParser()
		{
			Callbacks = new Dictionary<string, Callback>();
		}

		public void Add(string name, Callback cb)
		{
			Callbacks.Add(name, cb);
		}

		public void Parse(string[] args)
		{
			foreach(var arg in args)
			{
				var match = Regex.Match(arg, @"--?(\w+)\s*=\s*(.+)");

				if(!match.Success)
				{
					Log.Warning("Malformed argument: " + arg);
					continue;
				}

				var key = match.Groups[1].Captures[0].Value;

				if(!Callbacks.ContainsKey(key))
				{
					Log.Warning("Unknown argument: " + key);
					continue;
				}

				Callbacks[key].Invoke(match.Groups[2].Captures[0].Value);
			}
		}
	}
}