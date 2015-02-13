using System;

namespace Twirc.CLI.Util
{
	public sealed class SettingInformation
	{
		public Action<string> Callback;
		public string Description;
		public string Value;

		public SettingInformation(Action<string> callback, string description, string value = "")
		{
			Callback    = callback;
			Description = description;
			Value       = value;
		}
	}
}