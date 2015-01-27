using System;

namespace Twirc.CLI.Util
{
	public struct OptionDesc
	{
		public Action<string> Function;
		public string Description;

		/// <summary>
		/// The full name of the option. This is automatically set by the option parser.
		/// </summary>
		public string FullName;

		public OptionDesc(Action<string> function, string description)
		{
			Function    = function;
			Description = description;
			FullName    = "";
		}
	}
}