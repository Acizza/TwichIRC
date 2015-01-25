using System;

namespace Twirc.Lib
{
	public struct LoginResponse
	{
		public string Code;
		public string Message;
		public bool Success;

		public LoginResponse(bool success, string code, string message)
		{
			Code    = code;
			Message = message;
			Success = success;
		}
	}
}