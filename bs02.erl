%% coding: utf-8
%BS02: Разделить строку на слова

-module(bs02).
-export([words/1]).
-import(bs01_mod, [first_word/1]).

words(Bin) ->
    case first_word(Bin) of
        {Fw, <<>>} -> [Fw];
        {Fw, Rest} -> [Fw|words(Rest)]
    end.
