using System;

namespace Twirc.CLI.Util
{
	public struct ConfigDescription
	{
		public Action<string> Callback;
		public string Description;

		/// <summary>
		/// The full name of the option. This is automatically set by the option parser.
		/// </summary>
		public string FullName;

		public ConfigDescription(Action<string> callback, string description)
		{
			Callback    = callback;
			Description = description;
			FullName    = "";
		}
	}
}