-module(test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-import(bs01, [first_word/1]).
-import(bs02, [words/1]).
-import(bs03, [split/2]).

first_word_test_() ->
    ?_assert(bs01:first_word(<<"Some text">>) =:= <<"Some">>).

words_test_() ->
    ?_assert(bs02:words(<<"Text with four words">>) =:=
        [<<"Text">>, <<"with">>, <<"four">>, <<"words">>]).

split_test_() ->
    ?_assert(bs03:split(<<"Col1-:-Col2-:-Col3-:-Col4-:-Col5">>, "-:-") =:=
        [<<"Col1">>, <<"Col2">>, <<"Col3">>, <<"Col4">>, <<"Col5">>]).
