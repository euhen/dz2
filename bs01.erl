%% coding: utf-8
%BS01: Извлечь из строки первое слово

-module(bs01).
-export([first_word/1]).

first_word(Bin) ->
    first_word(Bin, <<>>).

% Private
first_word(<<" ", R/binary>>, <<>>) ->
    first_word(<<R/binary>>, <<>>);
first_word(<<" ", _R/binary>>, Acc) ->
    Acc;
first_word(<<C/utf8, R/binary>>, Acc) ->
    first_word(<<R/binary>>, <<Acc/binary, C/utf8>>);
first_word(<<>>, Acc) ->
    Acc.
