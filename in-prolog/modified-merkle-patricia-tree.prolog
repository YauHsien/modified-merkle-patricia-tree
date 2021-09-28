:- module('modified-merkle-patricia-tree',
          [
              'TRIE'/2,
              example/2
          ]).
:- use_module('recursive-length-prefix.prolog').
:- use_module('hex-prefix.prolog').

%%
%  By using `'KEC'(X, X).`, the example T will be coded as
%:     [134,0,114,111,109,
%:       [17,110,
%:         [ [32,101,3,2],
%:           [32,97,3,3]
%:         ],
%:         [52,105,99,3,4]],
%:       [53,108,117,115,3,5]]
%
example(T, B) :-
    T = [
        "romance"/"2",
        "romulus"/"5",
        "romania"/"3",
        "roman"/"1",
        "romantic"/"4"
    ],
    % Bytes
    % [114,111,109,97,110,99,101]
    % [114,111,109,117,108,117,115]
    % [114,111,109,97,110,105,97]
    % [114,111,109,97,110]
    % [114,111,109,97,110,116,105,99]
    % Nibbles
    % [7,2,6,15,6,13,6,1,6,14,6,3,6,5]
    % [7,2,6,15,6,13,7,5,6,12,7,5,7,3]
    % [7,2,6,15,6,13,6,1,6,14,6,9,6,1]
    % [7,2,6,15,6,13,6,1,6,14]
    % [7,2,6,15,6,13,6,1,6,14,7,4,6,9,6,3]
    'TRIE'(T, B).

%%
%  The input value T here shell be represented as an associated list:
%      [ "hello" / "value for key hello",
%        "world" / "value for key world"
%      ]
%
'TRIE'(T, R) :-
    [Key/Value|_] = T,
    string(Key),
    string(Value),
    !,
    nibble_key_values(T, T1),
    'TRIE'(T1, R).
'TRIE'(T, R) :-
    [_Key/_Value|_KVs] = T,
    !,
    c(T, T1),
    'RLP'(T1, T2),
    'KEC'(T2, R).

nibble_key_values([], []).
nibble_key_values([Key/Value|KVs], [KeyNibbles/ValueNibbles|KVs1]) :-
    string_nibbles(Key, KeyNibbles),
    string_nibbles(Value, ValueNibbles),
    nibble_key_values(KVs, KVs1).

string_nibbles(S, Ns) :-
    string(S),
    string_codes(S, Cs),
    findall(HL, (member(C,Cs),code_nibbles(C,HL)), HLs),
    append(HLs, Ns).

%%
%  ?- atom_char(h, Code),
%     code_nibbles(Code, H_L).
%: Code = 104,
%: H_L = [6, 8].
%
code_nibbles(C, [H,L]) :-
    byte(C),
    H is C >> 4,
    L is C /\ 0xf.

byte(B) :-
    B >> 8 =:= 0.

%%
%  n/2 is a modified version of n/2 in Ethereum yellow paper, formula 196,
%  that it does not pass in an empty T.
%
n(T, Result) :-
    c(T, R),
    'RLP'(R, R_p),
    length(R_p, Lr_p),
    (
        Lr_p < 32,
        !,
        Result = R;

        'KEC'(R_p, Result)
    ).

%%
%  c/2 is a modified version of c/3: c(T, I, T1).
%  In G. Wood's Ethereum yellow paper, a node cap function was introduced.
%  There's a parameter `i` as the global index for approach of the whole key-value list T.
%  Though in this implementation, I deal with sub-parts of T by using subset retrieval.
%
c(T, Result) :-
    length(T, 1),
    !,
    [I_0/I_1] = T,
    'HP'(I_0, 1, I_0e),
    append(I_0e, I_1, Result).
c(T, Result) :-
    common_key_prefix(T, K/T1),
    not(length(K, 0)),
    !,
    'HP'(K, 0, I_0e),
    n(T1, I_1),
    append(I_0e, I_1, Result).
c(T, Result) :-
    !,
    findall(
        R,
        (
            u(N),
            findall( K1/V,
                     ( member(K/V, T),
                       [N|K1] = K
                     ),
                     T1
                   ),
            [] \= T1,
            n(T1, R)
        ),
        Rs
    ),
    append(Rs, Result).

common_key_prefix(T, K/T1) :-
    find_common_key_prefix(T, [], K),
    populate_common_key_prefix(K, T, T1).

find_common_key_prefix([], AccAsKeyPrefix, AccAsKeyPrefix).
find_common_key_prefix([K/_|T], Acc, Result) :-
    [] \= Acc,
    !,
    common_prefix(K, Acc, K1),
    find_common_key_prefix(T, K1, Result).
find_common_key_prefix([K/_|T], [], Result) :-
    find_common_key_prefix(T, K, Result).


common_prefix([H|T1], [H|T2], [H|T3]) :-
    common_prefix(T1, T2, T3),
    !.
common_prefix(_, _, []).

populate_common_key_prefix(_, [], []).
populate_common_key_prefix(Key, [K/V|T], [K1/V|T1]) :-
    append(Key, K1, K),
    populate_common_key_prefix(Key, T, T1).

u(0).
u(1).
u(2).
u(3).
u(4).
u(5).
u(6).
u(7).
u(8).
u(9).
u(10).
u(11).
u(12).
u(13).
u(14).
u(15).

'KEC'(X, X).
