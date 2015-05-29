-module(patternMatching).
-export([test/0]).

test() ->
	self() ! 2,
	self() ! 1,

	receive
		1->base:putStrLn("1");
		2->base:putStrLn("2")
	end,

	receive
		1->base:putStrLn("1");
		2->base:putStrLn("2")
	end.