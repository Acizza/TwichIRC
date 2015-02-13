using System;
using System.Collections.Generic;

namespace Twirc.Lib
{
	public sealed class Channel
	{
		/// <summary>
		/// The list of users connected to the channel.
		/// </summary>
		public List<User> Users;

		/// <summary>
		/// The name of the channel.
		/// </summary>
		/// <value>The name.</value>
		public string Name { get; private set; }

		public Channel(string name)
		{
			Name  = name;
			Users = new List<User>();
		}

		public void AddUser(string name, UserGroup group = UserGroup.User)
		{
			Users.Add(new User(name, group));
		}

		public bool HasUser(string name)
		{
			return Users.Exists(x => x.Name == name);
		}

		public User GetUserByName(string name)
		{
			return Users.Find(x => x.Name == name);
		}

		public void RemoveUserByName(string name)
		{
			Users.RemoveAll(x => x.Name == name);
		}
	}
}