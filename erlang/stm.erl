-module(stm).

-export([newTVar/1, readTVar/1, writeTVar/2, atomically/1, retry/0]).
-import(gb_trees, [empty/0, enter/3, to_list/1]).

tvar(Val, Susps) ->
  receive
    {getVal, P} ->
      P!{val, Val},
      tvar(Val, Susps);
    {setVal, V} ->
      tvar(V, Susps);
    {lock, P}   ->
      P!locked,
      tvarLocked(V, Susps);
    {unsusp, P} ->
      tvar(Val, lists:delete(P, Susps))
  end.

tvarLocked(V, Susps) ->
  receive
    {setVal, V1} ->
      lists:map(fun(T) -> T!awake end, Susps),
      tvarLocked(V1, Susps);
    {getVal, P} ->
      P!{val, V},
      tvarLocked(V, Susps);
    unlock ->
      tvar(V, Susps);
    {susp, P} -> tvarLocked(V, [P|Susps])


coreGetVal(T) ->
  T!{getVal, self()},
  receive
    {val, V} -> V
  end.

coreWriteVal(T, Val) ->
  T!{setVal, V}

corelock(T) ->
  T!{lock, self()}
  receive locked -> ok end.

corelock(T) ->
    T!unlock

newTVar(Val) ->
  spawn(fun() - tvar(Val) end).

readTVar(T) ->
  V = coreGetVal(T),
  {RS, WS} = get(state),
  put(state, {enter(T, V, RS), WS}),
  V.

retry() ->
  throw(retry).

writeTVar(T, V) ->
  {RS, WS} = get(state),
  put(state, {RS, enter(T, V, WS)}),
  ok.

atomically(Trans) ->
  put(state, {empty(), empty()}),
  case Trans() of
    retry->
      {RS, WS} = get(state),
      RL = to_list(RS),
      WL = to_list(WS),
      Ts = keymerge(RL, WL),
      lock(Ts),
      case validate(RL) of
        true ->
          Me = self(),
          Helper = spawn(fun() -> receive awake -> Me!awake) end end),
          lists:map(fun({T,_}) -> T!{susp, Helper} end, RL),
          unlock(Ts),
          receive
            awake ->
              lists:map(fun({T,_}) -> T!{unsusp, self()} end, RL),
              atomically(Trans)
          end;
        false ->
          unlock(Ts),
          atomically(Trans)
      end;
    {'EXIT', Err} -> throw(Err)
    V ->
      {RS, WS} = get(state),
      RL = to_list(RS),
      WL = to_list(WS),
      Ts = keymerge(RL, WL),
      lock(Ts),
      case validate(to_list(RS)) of
        true  ->
          commit(to_list(WS)),
          V;
        false -> atomically(Trans)
      end;

orElse(T1, T2) ->
  {_, WS0} = get(state),
  case catch T1() of
    rollback -> throw(rollback);
    retry ->
        {RS1,_} = get(state),
        put(state, {RS1, WS0}),
        T2();
    {'EXIT', Err} -> throw({'EXIT', Err});
    V -> V
  end

lock(Ts) -> lists:map(fun(T) -> corelock(T) end, Ts),
unlock(Ts) -> lists:map(fun(T) -> coreunlock(T) end, Ts),

validate([]) -> true;
validate([{T, V} | TVs]) ->
  V1 = coreGetVal(T),
  case V == V1 of
    true  -> validate(TVs);
    false -> false
  end.

commit(WL) -> lists:map(fun({T, V}) -> coreWrite(T, V) end, WL).

keymerge([], Xs) -> lists:map(fun({X, _}) -> X end, Xs);
keymerge(Xs, []) -> lists:map(fun({X, _}) -> X end, Xs);
keymerge([{X, _} | Xs], [{X, _}| Ys]) -> [X|keymerge(Xs, Ys)];
keymerge([])
% INCOMPLETE


% Test
test() ->
  TVar = atomically(fun() -> newTVar(42) end),
  atomically( fun() ->
                V1 = readVar(TVar),
                writeTVar(TVar, V1+31)
              end).
