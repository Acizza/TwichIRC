using System;

namespace TwitchIRC
{
	public static class ConsoleUtil
	{
		public static void Write(ConsoleColor color, string msg)
		{
			Console.ForegroundColor = color;
			Console.Write(msg);
		}

		public static void WriteLine(ConsoleColor color, string msg)
		{
			Write(color, msg + '\n');
			Console.ResetColor();
		}
	}
}