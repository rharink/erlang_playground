-module(recursive).
-export([ fac/1
          , tail_fac/1
          , len/1
          , tail_len/1
          , duplicate/2
          , tail_duplicate/2
          , reverse/1
          , tail_reverse/1
          , sublist/2
          , tail_sublist/2
          , zip/2
          , tail_zip/2
          , quicksort/1
          , bestest_qsort/1
        ]).

%% simple fac function
%% fac(4) = 4 * fac(4-1)
%%        = 4 * 3 * fac(3-1)
%%        = 4 * 3 * 2 * fac(2-1)
%%        = 4 * 3 * 2 * 1 * fac(1-1)
%%        = 4 * 3 * 2 * 1 * 1
%%        = 24
fac(N) when N == 0 -> 1;
fac(N) when N > 0 -> N*fac(N-1).

%% recursive length.
len([]) -> 0;
len([_|T]) -> 1 + len(T).

% exported tail_len function
tail_len([_|T]) -> tail_len(T,0).

%% internal tail_len function
%% tail_len([1,2,3],0) = tail_len([2,3],1)
%% tail_len([2,3],1)   = tail_len([3],2)
%% tail_len([3],2)     = tail_len([],3)
%% tail_len([], 3)     = 3
tail_len([],Acc) -> Acc;
tail_len([_|T],Acc) -> tail_len(T,Acc+1).

%% exported tail_fac function.
%% fac(4) = 4 * fac(4-1)
tail_fac(N) -> tail_fac(N,1).

%% internal tail_fac function.
%% tail_fac(4)    = tail_fac(4,1)
%% tail_fac(4,1)  = tail_fac(4-1, 4*1)
%% tail_fac(3,4)  = tail_fac(3-1, 3*4)
%% tail_fac(2,12) = tail_fac(2-1, 2*12)
%% tail_fac(1,24) = tail_fac(1-1, 1*24)
%% tail_fac(0,24) = 24
tail_fac(0,Acc) -> Acc;
tail_fac(N,Acc) when N > 0 -> tail_fac(N-1, N*Acc).

%% duplicate things N times.
duplicate(0, _) ->
  [];
duplicate(N, Term) when N > 0 ->
  [Term|duplicate(N-1,Term)].

%% exported tail_duplicate function
tail_duplicate(N, Term) -> tail_duplicate(N, Term, []).

%% internal tail_duplicate function.
tail_duplicate(0, _, Acc) -> Acc;
tail_duplicate(N, Term, Acc) when N > 0 ->
  tail_duplicate(N-1, Term, [Term|Acc]).

%% reverse([1,2,3,4]) = [4]++[3]++[2]++[1]
%%                    = [4,3]++[2]++[1]
%%                    = [4,3,2]++[1]
%%                    = [4,3,2,1]
reverse([]) -> [];
reverse([H|T]) -> reverse(T) ++ [H].

%% tail_reverse exported function.
tail_reverse(List) -> tail_reverse(List, []).

%% tail_reverse internal function.
tail_reverse([], Acc) -> Acc;
tail_reverse([H|T], Acc) -> tail_reverse(T, [H|Acc]).

sublist(_, 0) -> [];
sublist([],_) -> [];
sublist([H|T], N) when N > 0 ->
  [H|sublist(T,N-1)].

tail_sublist(List, N) -> 
  lists:reverse(tail_sublist(List, N, [])).

%% tail_sublist([1,2,3,4], 3) = 
tail_sublist(_, 0, Acc) -> Acc;
tail_sublist([], _, Acc) -> Acc;
tail_sublist([H|T], N, Acc) when N > 0 ->
  tail_sublist(T, N-1, [H|Acc]).

zip([], _) -> [];
zip(_, []) -> [];
zip([X|Xs],[Y|Ys]) -> [{X,Y}|zip(Xs,Ys)].

tail_zip(X,Y) -> 
  lists:reverse(tail_zip(X,Y,[])).

tail_zip([],_, Acc) -> Acc;
tail_zip(_,[],Acc) -> Acc;
tail_zip([X|Xs],[Y|Ys],Acc) ->
  tail_zip(Xs, Ys, [{X,Y}|Acc]).

quicksort([]) -> [];
quicksort([Pivot|Rest]) ->
  quicksort([S || S <- Rest, S =< Pivot])
            ++ [Pivot] ++
  quicksort([L || L <- Rest, L > Pivot]).

%% Better performing quicksort.
bestest_qsort([]) -> [];
bestest_qsort(L=[_|_]) ->
  bestest_qsort(L, []).

bestest_qsort([], Acc) -> Acc;
bestest_qsort([Pivot|Rest], Acc) ->
  bestest_partition(Pivot, Rest, {[], [Pivot], []}, Acc).

bestest_partition(_, [], {Smaller, Equal, Larger}, Acc) ->
  bestest_qsort(Smaller, Equal ++ bestest_qsort(Larger, Acc));

bestest_partition(Pivot, [H|T], {Smaller, Equal, Larger}, Acc) ->
  if H < Pivot ->
      bestest_partition(Pivot, T, {[H|Smaller], Equal, Larger},Acc);
     H > Pivot ->
      bestest_partition(Pivot, T, {Smaller, Equal, [H|Larger]},Acc);
     H == Pivot ->
      bestest_partition(Pivot, T, {Smaller, [H|Equal], Larger},Acc)
  end.


