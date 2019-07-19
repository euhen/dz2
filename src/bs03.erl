%% coding: utf-8
%BS02: Разделить строку на слова

-module(bs03).
-export([split/2]).
-import(bs01_mod, [first_word/2]).

split(Bin, Delim) ->
    case first_word(Bin, Delim) of
        {Fw, <<>>} -> [Fw];
        {Fw, Rest} -> [Fw|split(Rest, Delim)]
    end.
