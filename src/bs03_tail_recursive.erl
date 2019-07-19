%% coding: utf-8
%BS02: Разделить строку на слова

-module(bs03_tail_recursive).
-export([split/2]).
-import(bs01_mod, [first_word/2]).

split(Bin, Delim) ->
    lists:reverse(
        split(Bin, Delim, [])
    ).

% Private
split(<<>>, _Delim, Acc) ->
    Acc;
split(Bin, Delim, Acc) ->
    {Fw, Rest} = first_word(Bin, Delim),
    split(Rest, Delim, [Fw|Acc]).
