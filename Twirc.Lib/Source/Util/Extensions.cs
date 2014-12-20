using System;

namespace Twirc.Lib.Util
{
	public static class Extensions
	{
		/// <summary>
		/// Returns the string found between start and end. Returns an empty string if nothing is found.
		/// </summary>
		/// <param name="source">Source string.</param>
		/// <param name="start">Start string.</param>
		/// <param name="end">End string.</param>
		/// <param name="offset">Offset to start looking at.</param>
		public static string Range(this string source, string start, string end, int offset = 0)
		{
			int startIdx = source.IndexOf(start, offset);

			if(startIdx < 0)
				return "";

			int endIdx = source.IndexOf(end, startIdx + offset + 1); // Add 1 to skip the start index in case they're the same

			if(endIdx < 0)
				return "";

			return source.Substring(startIdx + start.Length, endIdx - startIdx - end.Length);
		}

		/// <summary>
		/// Returns the string found between start. Returns an empty string if nothing is found.
		/// </summary>
		/// <param name="source">Source string.</param>
		/// <param name="start">Start string.</param>
		/// <param name="offset">Offset to start looking at.</param>
		public static string Range(this string source, string start, int offset = 0)
		{
			return Range(source, start, start, offset);
		}
	}
}