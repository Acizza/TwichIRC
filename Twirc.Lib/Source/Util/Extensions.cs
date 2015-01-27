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

			int endIdx = source.IndexOf(end, startIdx + 1); // Add 1 to skip the start index in case they're the same

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

		/// <summary>
		/// Returns the string found between start and end. Returns an empty string if nothing is found.
		/// </summary>
		/// <param name="source">Source string.</param>
		/// <param name="start">Start string.</param>
		/// <param name="ends">End strings.</param>
		/// <param name="offset">Offset to start looking at.</param>
		public static string Range(this string source, string start, string[] ends, int offset = 0)
		{
			int startIdx = source.IndexOf(start, offset);

			if(startIdx < 0)
				return "";

			int endIdx = -1;
			int endLength = 0;

			foreach(var end in ends)
			{
				endIdx = source.IndexOf(end, startIdx + 1); // Add 1 to skip the start index in case they're the same

				if(endIdx != -1)
				{
					endLength = end.Length;
					break;
				}
			}

			if(endIdx == -1)
				return "";

			return source.Substring(startIdx + start.Length, endIdx - startIdx - endLength);
		}

		/// <summary>
		/// Returns the string found between start and end. Returns an empty string if nothing is found.
		/// </summary>
		/// <param name="source">Source string.</param>
		/// <param name="start">Start string.</param>
		/// <param name="offset">Offset to start looking at.</param>
		/// <param name="ends">End strings.</param>
		public static string Range(this string source, string start, int offset, params string[] ends)
		{
			return source.Range(start, ends, offset);
		}

		/// <summary>
		/// Returns a string up until the string to.
		/// </summary>
		/// <param name="source">Source string.</param>
		/// <param name="to">String to return at.</param>
		/// <param name="exclude">If set to <c>true</c>, the to string is excluded from the end of the returned string.</param>
		/// <param name="offset">Index to start looking for the to string.</param>
		public static string To(this string source, string to, bool exclude = true, int offset = 0)
		{
			int idx = source.IndexOf(to, offset);

			if(idx == -1)
				return "";

			return source.Substring(0, exclude ? idx + to.Length : idx);
		}

		/// <summary>
		/// Returns a string starting from the string fromStr.
		/// </summary>
		/// <param name="source">Source string.</param>
		/// <param name="fromStr">String to start at.</param>
		/// <param name="exclude">If set to <c>true</c>, the fromStr string is excluded from the beginning of the returned string.</param>
		/// <param name="offset">Index to start looking for the fromStr string..</param>
		public static string From(this string source, string fromStr, bool exclude = true, int offset = 0)
		{
			int idx = source.IndexOf(fromStr, offset);

			if(idx == -1)
				return "";

			return source.Substring(exclude ? idx + fromStr.Length : idx);
		}
	}
}