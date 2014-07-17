using System;

namespace TwitchIRC
{
	public static class Extensions
	{
		public static string Range(this string source, string start, string end, int offset = 0)
		{
			var stIndex = source.IndexOf(start, offset);

			if(stIndex < 0)
				return null;

			var enIndex = source.IndexOf(end, stIndex + offset + 1);

			if(enIndex < 0)
				return null;

			return source.Substring(stIndex + 1, enIndex - stIndex - 1);
		}
	}
}