-module(mvar).
-export([create/0, isEmpty/1, put/2, overwrite/2, take/1, take/2, read/1, tryPut/2, tryTake/1, swap/2, clear/1]).

empty() ->
	receive
		{isEmpty, Who} -> Who!{mvar_isempty, true}, empty();
		{put, V, Who} -> Who!{mvar_put, true}, full(V);
		{tryPut, V, Who} -> Who!{mvar_tryput, true}, full(V);
		{tryTake, Who} -> Who!{mvar_trytake, false}, empty();
		{overwrite, V, Who} -> Who!{mvar_overwrite, true}, full(V);
		clear -> empty()
	end.

full(C) ->
	receive
		{isEmpty, Who} -> Who!{mvar_isempty, false}, full(C);
		{take, Who} -> Who!{mvar_take, C}, empty();
		{read, Who} -> Who!{mvar_read, C}, full(C);
		{tryPut, _, Who} -> Who!{mvar_tryput, false}, full(C);
		{tryTake, Who} -> Who!{mvar_trytake, C}, empty();
		{swap, V, Who} -> Who!{mvar_swap, C}, full(V);
		{overwrite, V, Who} -> Who!{mvar_overwrite, true}, full(V);
		clear -> empty()
	end.

isEmpty(M) ->
	M!{isEmpty, self()},
	receive
		{mvar_isempty, X} -> X
	end.

put(M, V) ->
	M!{put, V, self()},
	receive
		{mvar_put, X} -> X
	end. 

overwrite(M, V) ->
	M!{overwrite, V, self()},
	receive
		{mvar_overwrite, X} -> X
	end.

take(M) ->
	M!{take, self()},
	receive
		{mvar_take, X} -> X
	end.

take(M, Ms) ->
	Self = self(),
	spawn(	fun() ->
				M!{take, self()},
				receive
					{mvar_take, X} -> Self!{mvar_take, X}
					after Ms ->
						Self!timeout
				end
			end),
	receive
		{mvar_take, X} -> X;
		timeout -> timeout
	end.


read(M) ->
	M!{read, self()},
	receive
		{mvar_read, X} -> X
	end.

tryPut(M, V) ->
	M!{tryPut, V, self()},
	receive
		{mvar_tryput, X} -> X
	end.

tryTake(M) ->
	M!{tryTake, self()},
	receive
		{mvar_trytake, X} -> X
	end.

swap(M, V) ->
	M!{swap, V, self()},
	receive
		{mvar_swap, X} -> X
	end. 

clear(M) ->
	M!clear.

create() ->
	spawn(fun() -> empty() end).