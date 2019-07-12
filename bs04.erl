%% coding: utf-8
%BS04: Разобрать JSON

-module(bs04).
-export([start/0]).
-export([decode/1]).

start() ->

    JSON = 
		<<"{
		    \"firstName\": \"John\",
		    \"lastName\": \"Smith\",
		    \"age\": 25,
		    \"address\": {
		        \"streetAddress\": \"21 2nd Street\",
		        \"city\": \"New York\",
		        \"state\": \"NY\",
		        \"postalCode\": \"10021\"
		    },
		    \"phoneNumber\": [
		        {
		            \"type\": \"home\",
		            \"number\": \"212 555-1234\"
		        },
		        {
		            \"type\": \"fax\",
		            \"number\": \"646 555-4567\"
		        }
		    ]
        }">>,

    Parsed = decode(JSON),
    io:fwrite( "\n\nResult: \n" ),
    io:fwrite("~p~n",[ Parsed ]).

decode(Bin) ->
    {Result, _} = parse(Bin),
    Result.

parse(<<Char/utf8, Rest/binary>>=Bin) ->
    io:fwrite(<<Char>>),
    case <<Char>> of
        <<" ">> -> parse(Rest);
        <<A>> when A >= 0, A =< 31 -> parse(Rest);
        <<>> -> stub3;
        _ -> detect_type(Bin)
    end.

detect_type(<<Char/utf8, Rest/binary>>=Bin) ->
    io:fwrite(<<Char>>),
    case <<Char>> of
        <<"{">> -> extract_obj_items(Bin);
        <<"[">> -> extract_list_items(Bin);
        <<"\"">> -> extract_quoted(Rest);
        <<A>> when A >= 48, A =< 57 -> extract_unquoted(Bin);
        <<A>> when A >= 64, A =< 122 -> extract_unquoted(Bin)
    end.

extract_obj_items(<<Char/utf8, Rest/binary>>=Bin) ->
    io:fwrite("extract_obj_items1"),
    io:fwrite(<<Char>>),
    case <<Char>> of
        <<"\"">> -> process_pairs(Bin);
        <<"{">> -> process_pairs(Rest);
        <<"}">> -> {{no1, no1}, <<>>};
        <<",">> -> extract_obj_items(Rest);
        <<" ">> -> extract_obj_items(Rest);
        <<A>> when A >= 0, A =< 31 -> extract_obj_items(Rest);
        <<A>> when A >= 48, A =< 122 -> process_pairs(Bin)
    end;
extract_obj_items(<<>>) ->
    io:fwrite("extract_obj_items2"),
    {#{no2=>no2}, <<>>}.

process_pairs(<<Char/utf8, Rest/binary>>=Bin) ->
    io:fwrite("process_pairs1"),
    io:fwrite(<<Char>>),
    case <<Char>> of
        <<" ">> -> process_pairs(Rest);
        <<A>> when A >= 0, A =< 31 -> process_pairs(Rest);
        <<"}">> -> {#{no3=>no3}, <<>>};
        <<_>> ->
            {{Key, Value}, Rest2} = extract_pair(Rest),
            %io:fwrite({{Key, Value}, Rest2}),
            Map1 = #{Key=>Value},
            {Map2, Rest3} = process_pairs(Rest2),
            {maps:merge(Map1, Map2), Rest3}
    end;
process_pairs(<<>>) ->
    io:fwrite("process_pairs2"),
    {#{no4=>no4}, <<>>}.

extract_pair(Bin) ->
    io:fwrite("extract_pair"),
    io:fwrite(Bin),
    {Key, Bin2} = extract_key(Bin),
    Bin2a = del_dots(Bin2),
    {Value, Rest3} = extract_value(Bin2a),
    {{Key, Value}, Rest3}.

del_dots(<<Char/utf8, Rest/binary>>=Bin) ->
    case <<Char>> of
        <<":">> -> spaces_cleaner(Rest);
        <<_>> -> spaces_cleaner(Bin)
    end.

spaces_cleaner(<<Char/utf8, Rest/binary>>=Bin) ->
    case <<Char>> of
        <<" ">> -> spaces_cleaner(Rest);
        <<A>> when A >= 0, A =< 31 -> spaces_cleaner(Rest);
        <<_>> -> Bin
    end.

extract_key(<<Char/utf8, Rest/binary>>=Bin) ->
    io:fwrite("extract_key"),
    io:fwrite(<<Char>>),
    case <<Char>> of
        <<" ">> -> extract_key(Rest);
        <<"{">> -> extract_key(Rest);
        <<A>> when A >= 0, A =< 31 -> extract_key(Rest);
        <<"\"">> -> extract_quoted(Rest);
        <<A>> when A >= 48, A =< 57 -> extract_unquoted(Bin);
        <<A>> when A >= 64, A =< 122 -> extract_unquoted(Bin)
    end.

extract_value(<<Char/utf8, Rest/binary>>=Bin) ->
    io:fwrite("extract_value"),
    io:fwrite(<<Char>>),
    case <<Char>> of
        <<"{">> -> parse(Bin);
        <<"[">> -> parse(Bin);
        <<":">> -> parse(Rest);
        <<" ">> -> extract_value(Rest);
        <<A>> when A >= 0, A =< 31 -> extract_value(Rest);
        <<"\"">> -> extract_quoted(Rest);
        <<A>> when A >= 48, A =< 57 -> extract_unquoted(Bin);
        <<A>> when A >= 64, A =< 122 -> extract_unquoted(Bin)
    end.

extract_list_items(<<Char/utf8, Rest/binary>>=Bin) ->
    io:fwrite("extract_list_items"),
    io:fwrite(<<Char>>),
    case <<Char>> of
        <<"[">> -> extract_list_items(Rest);
        <<"]">> -> {<<>>, Rest};
        <<_>> ->
            {Tmp, Rest2} = extract_list_items(Rest),
            {<<Char/utf8, Tmp/binary>>, Rest2}
    end;
extract_list_items(<<>>) ->
    {<<>>, <<>>}.

extract_quoted(<<Char/utf8, Rest/binary>>=Bin) ->
    io:fwrite("process_pairs"),
    io:fwrite(<<Char>>),
    case <<Char>> of
        <<"\"">> -> {<<>>, Rest};
        <<_>> ->
            {Tmp, Rest2} = extract_quoted(Rest),
            {<<Char/utf8, Tmp/binary>>, Rest2}
    end;
extract_quoted(<<>>) ->
    {<<>>, <<>>}.

extract_unquoted(<<Char/utf8, Rest/binary>>=Bin) ->
    io:fwrite("process_pairs"),
    io:fwrite(<<Char>>),
    case <<Char>> of
        <<" ">> -> {<<>>, Bin};
        <<A>> when A >= 0, A =< 31 -> {<<>>, Bin};
        <<_>> ->
            {Tmp, Rest2} = extract_quoted(Rest),
            {<<Char/utf8, Tmp/binary>>, Rest2}
    end.
