-module(primes).
-export([init/0, give/1, init/2]).

init() -> spawn(fun() -> allNumbers(2) end).
init(N, Gen) -> spawn(fun() -> sieve(N, Gen) end).

allNumbers(N) ->
	receive
		{give, Who} -> Who!N
	end,
	allNumbers(N+1).

give(Np) ->
	Np!{give, self()},
	receive
		N -> N
	end.

sieve(Gen, N) ->
	receive
		{collect, Who} -> Who!N, sieve(Gen, N);
		{give, Who} ->
			Who!sieveNext(N, give(Gen), Gen),
			sieve(Gen, N)
	end.

sieveNext(N, Actual, Gen) when (Actual rem N == 0) -> sieveNext(N, give(Gen), Gen);  
sieveNext(N, _, _) -> N. 
