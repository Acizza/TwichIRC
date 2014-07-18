using System;

namespace TwitchIRC
{
	public static class Extensions
	{
		public static string Range(this string source, string start, string end, int offset = 0)
		{
			int stIndex = source.IndexOf(start, offset);

			if(stIndex < 0)
				return null;

			int enIndex = source.IndexOf(end, stIndex + offset + 1);

			if(enIndex < 0)
				return null;

			return source.Substring(stIndex + start.Length, enIndex - stIndex - end.Length);
		}

		public static string Range(this string source, string start, string[] ends, int offset = 0)
		{
			int stIndex = source.IndexOf(start, offset);

			if(stIndex < 0)
				return null;

			int enIndex = -1;

			foreach(var end in ends)
			{
				enIndex = source.IndexOf(end, stIndex + offset + 1);

				if(enIndex != -1)
					break;
			}

			if(enIndex < 0)
				return null;

			return source.Substring(stIndex + start.Length, enIndex - stIndex - 1);
		}
	}
}