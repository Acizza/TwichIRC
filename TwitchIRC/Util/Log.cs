using System;

namespace TwitchIRC
{
	public static class Log
	{
		public static void Info(string msg)
		{
			Console.WriteLine("INFO: " + msg);
		}

		public static void Warning(string msg)
		{
			Console.WriteLine("WARNING: " + msg);
		}

		public static void Error(string msg)
		{
			Console.WriteLine("ERROR: " + msg);
		}
	}
}