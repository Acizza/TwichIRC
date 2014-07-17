using System;

namespace TwitchIRC
{
	public static class Log
	{
		static void Write(string type, string msg, ConsoleColor color)
		{
			Console.ForegroundColor = color;
			Console.WriteLine(type + ": " + msg);
			Console.ResetColor();
		}

		public static void Info(string msg)
		{
			Write("INFO", msg, ConsoleColor.Gray);
		}

		public static void Warning(string msg)
		{
			Write("WARNING", msg, ConsoleColor.Yellow);
		}

		public static void Error(string msg)
		{
			Write("ERROR", msg, ConsoleColor.Red);
		}
	}
}