using System;

namespace TwitchIRC
{
	public static class Log
	{
		public static void Info(string msg)
		{
			ConsoleUtil.WriteLine(ConsoleColor.Gray, "INFO: " + msg);
		}

		public static void Warning(string msg)
		{
			ConsoleUtil.WriteLine(ConsoleColor.Yellow, "WARNING: " + msg);
		}

		public static void Error(string msg)
		{
			ConsoleUtil.WriteLine(ConsoleColor.Red, "ERROR: " + msg);
		}
	}
}