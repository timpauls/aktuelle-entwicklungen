-module(pingpong).
-export([pingTest/0]).

receiveLoop() ->
			receive
				{ping, Who} -> Who ! pong,
				receiveLoop()
			end.

pingTest() -> spawn(fun() -> receiveLoop() end).