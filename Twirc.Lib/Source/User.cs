using System;

namespace Twirc.Lib
{
	public sealed class User
	{
		public string Name { get; private set; }
		public UserGroup Group;

		public User(string name, UserGroup group = UserGroup.User)
		{
			Name  = name;
			Group = group;
		}
	}
}