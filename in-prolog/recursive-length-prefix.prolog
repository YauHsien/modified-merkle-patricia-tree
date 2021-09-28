:- module('recursive-length-prefix', ['RLP'/2]).

%%
%  ?- atom_codes(abcdefg,Cs),
%     'RLP'(Cs,R).
%: Cs = [97, 98, 99, 100, 101, 102, 103],
%: R = [135, 97, 98, 99, 100, 101, 102, 103].
%  ?- atom_codes(abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmn,Cs),
%     'RLP'(Cs,R).
%: Cs = [97, 98, 99, 100, 101, 102, 103, 104, 105|...],
%: R = [185, 1, 44, 97, 98, 99, 100, 101, 102|...].
%
'RLP'(B, R) :-
    is_list(B),
    [H|_] = B,
    not(is_list(H)),
    !,
    'R_b'(B, R).
'RLP'(L, R) :-
    is_list(L),
    [H|_] = L,
    is_list(H),
    !,
    'R_l'(L, R).
'RLP'(I, R) :-
    integer(I),
    not(I < 0),
    !,
    'BE'(I, B),
    'RLP'(B, R).
%%
%  ?- atom_codes(hello,Cs),
%     'R_b'(Cs,A).
%: Cs = [104, 101, 108, 108, 111],
%: A = [133, 104, 101, 108, 108, 111].
%  ?- atom_codes(hell,Cs),
%     'R_b'(Cs,A).
%: Cs = [104, 101, 108, 108],
%: A = [132, 104, 101, 108, 108].
%  ?- atom_codes(abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabc,Cs),
%     'R_b'(Cs,A).
%: Cs = [97, 98, 99, 100, 101, 102, 103, 104, 105|...],
%: A = [183, 97, 98, 99, 100, 101, 102, 103, 104|...].
%  ?- atom_codes(abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmn,Cs),
%     'R_b'(Cs,A).
%: Cs = [97, 98, 99, 100, 101, 102, 103, 104, 105|...],
%: A = [185, 1, 44, 97, 98, 99, 100, 101, 102|...].
%
'R_b'(X, ByteArray) :-
    length(X, Lx),
    (
        Lx = 1,
        [X_0] = X,
        X_0 < 128,
        !,
        ByteArray = X;

        Lx < 56,
        !,
        P is 128 + Lx,
        ByteArray = [P|X];

        Lx < 2 << 64,
        !,
        'BE'(Lx, B),
        length(B, Lb),
        P is 183 + Lb,
        append([P|B], X, ByteArray)
    ).

%%
%  ?- 'BE'(0x12c, E).
%: E = [1, 44].
%  ?- 'BE'(0,E).
%: E = [0].
%  ?- 'BE'(1,E).
%: E = [1].
%  ?- 'BE'(255,E).
%: E = [255].
%  ?- 'BE'(256,E).
%: E = [1, 0].
%  ?- 'BE'(-1,E).
%: false.
%
'BE'(0, [0]) :- !.
'BE'(X, E) :-
    integer(X),
    X > 0,
    big_endian(X, [], E).

big_endian(0, AccAsResult, AccAsResult) :- !.
big_endian(N, Acc, Result) :-
    N1 is N >> 8,
    R is N - N1 << 8,
    big_endian(N1, [R|Acc], Result).

'R_l'(X, ByteArray) :-
    s(X, X1),
    length(X1, Lx1),
    (
        Lx1 < 56,
        !,
        P is 192 + Lx1,
        ByteArray = [P|X1];

        Lx1 < 2 << 64,
        !,
        'BE'(Lx1, B),
        length(B, Lb),
        P is 247 + Lb,
        append([P|B], X1, ByteArray)
    ).

s(X, R) :-
    s(X, [], R).

s([], Acc, Result) :-
    append(Acc, Result).
s([H|T], Acc, Result) :-
    'RLP'(H, H1),
    s(T, [H1|Acc], Result).
