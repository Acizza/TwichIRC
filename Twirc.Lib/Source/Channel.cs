using System;
using System.Collections.Generic;

namespace Twirc.Lib
{
	public sealed class Channel
	{
		/// <summary>
		/// The list of users connected to the channel.
		/// </summary>
		public List<string> Users;

		/// <summary>
		/// The name of the channel.
		/// </summary>
		/// <value>The name.</value>
		public string Name { get; private set; }

		public Channel(string name)
		{
			Name  = name;
			Users = new List<string>();
		}
	}
}