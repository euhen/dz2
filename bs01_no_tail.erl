%% coding: utf-8
%BS01: Извлечь из строки первое слово

-module(bs01_no_tail).
-export([first_word/1]).

first_word(<<" ", R/binary>>) ->
    fw(cs(<<" ", R/binary>>));
first_word(Bin) ->
    fw(Bin).

% Private helpers

% Function of extracting the first word
fw(<<" ">>) ->
    <<"">>;
fw(<<" ", R/binary>>) ->
    <<"">>;
fw(<<>>) ->
    <<"">>;
fw(<<C/utf8, R/binary>>) ->
    Tmp = fw(R),
    <<C/utf8, Tmp/binary>>.

% Function of croping the leading spaces
cs(<<" ", R/binary>>) ->
    cs(R);
cs(<<>>) ->
    <<"">>;
cs(<<C/utf8, R/binary>>) ->
    <<C/utf8, R/binary>>.
