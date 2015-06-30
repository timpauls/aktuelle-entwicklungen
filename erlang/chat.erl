-module(chat).
-export([join/2, init/1]).

help() ->
	register(chat, self()).

init(Name) ->
	help(),
	keyboard(self(), Name),
	loop().

join(Node, Name) ->
	help(),
	net_adm:ping(Node),
	timer:sleep(500),
	broadcast(nodes(), Name, join, node()),
	keyboard(self(), Name),
	monitor(nodes()),
	loop().

loop() ->
	receive
		{msg, Name, Msg} -> base:putStrLn(Name++": "++Msg), loop();
		{quit} -> ok;
		{nodedown, Node} -> base:putStrLn(atom_to_list(Node)++" crashed."),loop();
		{join, Name, Node} -> base:putStrLn(Name ++ " has joined."), monitor([Node]), loop()
	end.

keyboard(Cid, Name) ->
	spawn_link(fun() -> chat(Cid, Name) end).

chat(Cid, Name) ->
	case base:getLine(Name++"> ") of
		"quit" -> 
			broadcast(nodes(), Name, msg, Name++" has left."),
			Cid!{quit};
		Str -> broadcast(nodes(), Name, msg, Str), chat(Cid, Name)
	end.

broadcast([], _, _, _) -> ok;
broadcast([Node], Name, Type, Str) -> {chat, Node}!{Type, Name, Str};
broadcast([H|T], Name, Type, Str) -> broadcast([H], Name,Type, Str), broadcast(T, Name,Type, Str).

monitor([]) -> ok;
monitor([Node]) -> monitor_node(Node, true);
monitor([H|T]) -> monitor([H]), monitor(T).
