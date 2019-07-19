-module(test2).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-import(bs01_no_tail, [first_word/1]).
-import(bs02_tail_recursive, [words/1]).
-import(bs03_tail_recursive, [split/2]).

first_word_test_() ->
    ?_assert(bs01_no_tail:first_word(<<"Some text">>) =:= <<"Some">>).

words_test_() ->
    ?_assert(bs02_tail_recursive:words(<<"Text with four words">>) =:=
        [<<"Text">>, <<"with">>, <<"four">>, <<"words">>]).

split_test_() ->
    ?_assert(bs03_tail_recursive:split(<<"Col1-:-Col2-:-Col3-:-Col4-:-Col5">>, "-:-") =:=
        [<<"Col1">>, <<"Col2">>, <<"Col3">>, <<"Col4">>, <<"Col5">>]).
