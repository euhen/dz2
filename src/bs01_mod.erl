%% coding: utf-8
%BS01: Извлечь из строки первое слово
% Функция возвращает кортеж, состоящий из первого слова и остатка строки

-module(bs01_mod).
-export([first_word/1, first_word/2]).

first_word(Bin) ->
    first_word(Bin, <<" ">>).

first_word(Bin, <<_/binary>>=DelimStr) ->
    DelimSize = byte_size(DelimStr),
    first_word(Bin, {delimiter, DelimStr, DelimSize}, <<>>);
first_word(Bin, [_|_]=DelimStr) ->
    first_word(Bin, list_to_binary(DelimStr)).

% Private
first_word(Bin, {delimiter, DelimStr, DelimSize}=Delimiter, Acc) ->
    case {Bin, Acc} of
        {<<DelimStr:DelimSize/binary, Rest/binary>>, <<>>} ->
            first_word(Rest, Delimiter, <<>>);
        {<<DelimStr:DelimSize/binary, Rest/binary>>, Acc} ->
            {Acc, Rest};
        {<<C/utf8, Rest/binary>>, Acc} ->
            first_word(Rest, Delimiter, <<Acc/binary, C/utf8>>);	
        {<<>>, Acc} ->
            {Acc, <<>>}
    end.
