using System;

namespace Twirc.CLI.Util
{
	public sealed class CommandAttribute : Attribute
	{
		public string Name        { get; private set; }
		public string Usage       { get; private set; }
		public string Description { get; private set; }

		public uint MinimumArguments { get; private set; }

		public CommandAttribute(string name, uint minArgs = 0, string usage = "", string description = "")
		{
			Name  = name;
			Usage = usage;
			Description      = description;
			MinimumArguments = minArgs;
		}
	}
}