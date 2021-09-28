:- module('hex-prefix', ['HP'/3]).

%%
%  ?- 'HP'([0x1,0x2,0xc], 1, A).
%: A = [49, 44].
%  ?- 'HP'([0x2,0xc], 1, A).
%: A = [32, 44].
%  ?- 'HP'([0x1,0x2,0xc], 0, A).
%: A = [17, 44].
%  ?- 'HP'([0x2,0xc], 0, A).
%: A = [0, 44].
%
'HP'(X, T, ByteArray) :-
    length(X, Lx),
    f(T, F_t),
    (
        Lx mod 2 =:= 0,
        !,
        nibbles_bytes([F_t,0|X], ByteArray);

        [H|T1] = X,
        nibbles_bytes([F_t+1,H|T1], ByteArray)
    ).

f(T, 2) :-
    T =\= 0,
    !.
f(_, 0).

nibbles_bytes([], []).
nibbles_bytes([H1,H2|T], [H3|T1]) :-
    H1 < 0x10,
    H2 < 0x10,
    H3 is 16 * H1 + H2,
    nibbles_bytes(T, T1).
