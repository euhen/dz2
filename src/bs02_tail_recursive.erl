%% coding: utf-8
%BS02: Разделить строку на слова

-module(bs02_tail_recursive).
-export([words/1]).
-import(bs01_mod, [first_word/1]).

words(Bin) ->
    lists:reverse(
        words(Bin, [])
    ).

% Private
words(<<>>, Acc) ->
    Acc;
words(Bin, Acc) ->
    {Fw, Rest} = first_word(Bin),
    words(Rest, [Fw|Acc]).
