%% coding: utf-8
%BS04: Разобрать JSON

-module(bs04).
-export([start/0]).
-export([decode/1]).

start() ->

    JSON = 
		<<"{
		    \"firstNa me\": \"John\",
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

    JSON2 = 
		<<"
{
  \"squadName\": \"Super hero squad\",
  \"homeTown\": \"Metro City\",
  \"formed\": 2016,
  \"secretBase\": \"Super tower\",
  \"active\": true,
  \"members\": [
    {
      \"name\": \"Molecule Man\",
      \"age\": 29,
      \"secretIdentity\": \"Dan Jukes\",
      \"powers\": [
        \"Radiation resistance\",
        \"Turning tiny\",
        \"Radiation blast\"
      ]
    },
    {
      \"name\": \"Madame Uppercut\",
      \"age\": 39,
      \"secretIdentity\": \"Jane Wilson\",
      \"powers\": [
        \"Million tonne punch\",
        \"Damage resistance\",
        \"Superhuman reflexes\"
      ]
    },
    {
      \"name\": \"Eternal Flame\",
      \"age\": 1000000,
      \"secretIdentity\": \"Unknown\",
      \"powers\": [
        \"Immortality\",
        \"Heat Immunity\",
        \"Inferno\",
        \"Teleportation\",
        \"Interdimensional travel\"
      ]
    }
  ]
}
">>,

    JSON3 = 
		<<"
{
  'squadName': 'Super hero squad',
  'homeTown': 'Metro City',
  'formed': 2016,
  'secretBase': 'Super tower',
  'active': true,
  'members': [
    {
      'name': 'Molecule Man',
      'age': 29,
      'secretIdentity': 'Dan Jukes',
      'powers': [
        'Radiation resistance',
        'Turning tiny',
        'Radiation blast'
      ]
    },
    {
      'name': 'Madame Uppercut',
      'age': 39,
      'secretIdentity': 'Jane Wilson',
      'powers': [
        'Million tonne punch',
        'Damage resistance',
        'Superhuman reflexes'
      ]
    },
    {
      'name': 'Eternal Flame',
      'age': 1000000,
      'secretIdentity': 'Unknown',
      'powers': [
        'Immortality',
        'Heat Immunity',
        'Inferno',
        'Teleportation',
        'Interdimensional travel'
      ]
    }
  ]
}
">>,

    Parsed = decode(JSON3),
    io:fwrite( "\n\nResult: \n" ),
    io:fwrite("~p~n",[ Parsed ]).

decode(Bin) ->
    {Result, _} = parse(Bin),
    Result.

parse(<<>>) ->
    {<<>>, <<>>};
parse(OrigBin) ->
    <<Char/utf8, Rest/binary>>=Bin = spaces_cleaner(OrigBin),
    case <<Char>> of
        <<>> -> stub3;
        _ -> detect_type(Bin)
    end.

detect_type(<<Char/utf8, Rest/binary>>=Bin) ->
    case <<Char>> of
        <<"{">> -> extract_obj_items(Bin);
        <<"[">> -> extract_list_items(Bin);
        <<"\"">> -> extract_quoted(Bin);
        <<"'">> -> extract_quoted(Bin);
        <<A>> when A >= 48, A =< 57 -> extract_numeric(Bin);
        <<A>> when A >= 97, A =< 122 -> extract_unquoted(Bin)
    end.

extract_obj_items(<<_, _/binary>>=OrigBin) ->
    <<Char/utf8, Rest/binary>>=Bin = spaces_cleaner(OrigBin),
    case <<Char>> of
        <<"\"">> -> process_pairs(Bin);
        <<"'">> -> process_pairs(Bin);
        <<"{">> -> process_pairs(Rest);
        <<"}">> -> {#{}, Rest};
        <<",">> -> extract_obj_items(Rest);
        <<A>> when A >= 48, A =< 122 -> process_pairs(Bin)
    end;
extract_obj_items(<<>>) ->
    {#{}, <<>>}.

process_pairs(<<_, _/binary>>=OrigBin) ->
    <<Char/utf8, Rest/binary>>=Bin = spaces_cleaner(OrigBin),
    case <<Char>> of
        <<"}">> -> {#{}, Rest};
        <<_>> ->
            {{Key, Value}, Rest2} = extract_pair(Bin),
            Map1 = #{Key=>Value},
            {Map2, Rest3} = extract_obj_items(Rest2),
            {maps:merge(Map1, Map2), Rest3}
    end;
process_pairs(<<>>) ->
    {#{}, <<>>}.

extract_pair(Bin) ->
    {Key, Bin2} = extract_key(Bin),
    Bin2a = del_dots(Bin2),
    {Value, Rest3} = extract_value(Bin2a),
    {{Key, Value}, Rest3}.

del_dots(<<Char/utf8, Rest/binary>>=Bin) ->
    case <<Char>> of
        <<":">> -> spaces_cleaner(Rest);
        <<_>> -> spaces_cleaner(Bin)
    end;
del_dots(<<>>) ->
    <<>>.

spaces_cleaner(<<Char/utf8, Rest/binary>>=Text) ->
    case <<Char>> of
        <<" ">> -> spaces_cleaner(Rest);
        <<A>> when A >= 0, A =< 31 -> spaces_cleaner(Rest);
        <<>> -> (Text);
        _ -> (Text)
    end;
spaces_cleaner(<<>>) ->
    <<>>.

extract_key(OrigBin) ->
    <<Char/utf8, Rest/binary>>=Bin = spaces_cleaner(OrigBin),
    case <<Char>> of
        <<"\"">> -> extract_quoted(Bin);
        <<"'">> -> extract_quoted(Bin);
        <<A>> when A >= 48, A =< 57 -> extract_numeric(Bin);
        <<A>> when A >= 64, A =< 122 -> extract_unquoted(Bin)
    end.

extract_value(OrigBin) ->
    parse(OrigBin).

extract_list_items(<<>>) ->
    {[], <<>>};
extract_list_items(OrigBin) ->
    <<Char/utf8, Rest/binary>>=Bin = spaces_cleaner(OrigBin),
    case <<Char>> of
        <<"[">> -> extract_list_items(Rest);
        <<",">> -> extract_list_items(Rest);
        <<"]">> -> {[], Rest};
        <<_>> -> 
            {Obj1, Rest1} = parse(Bin),
            {List2, Rest2} = extract_list_items(Rest1),
            {[Obj1] ++ List2, Rest2}
    end.

extract_quoted(<<"\"", Rest/binary>>) ->
    extract_quoted1(Rest);
extract_quoted(<<"'", Rest/binary>>) ->
    extract_quoted1(Rest).
extract_quoted1(<<Char/utf8, Rest/binary>>=Bin) ->
    case <<Char>> of
        <<"\"">> -> {<<>>, Rest};
        <<"'">> -> {<<>>, Rest};
        <<_>> ->
            {Tmp, Rest2} = extract_quoted1(Rest),
            {<<Char/utf8, Tmp/binary>>, Rest2}
    end.

extract_unquoted(<<"true", Rest/binary>>) ->
    {true, Rest};
extract_unquoted(<<"false", Rest/binary>>) ->
    {false, Rest};
extract_unquoted(<<Char/utf8, Rest/binary>>=Bin) ->
    case <<Char>> of
        <<" ">> -> {<<>>, Bin};
        <<A>> when A >= 0, A =< 31 -> {<<>>, Bin};
        <<",">> -> {<<>>, Bin};
        <<":">> -> {<<>>, Bin};
        <<"}">> -> {<<>>, Bin};
        <<"]">> -> {<<>>, Bin};
        <<_>> ->
            {Tmp, Rest2} = extract_unquoted(Rest),
            {<<Char/utf8, Tmp/binary>>, Rest2}
    end.

extract_numeric(Bin) ->
    {Field, Rest} = extract_unquoted(Bin),
    NumField = try
        list_to_integer(binary_to_list(Field), 10)
    catch
        error:badarg -> list_to_float( binary_to_list(Field) )
    end,
    {NumField, Rest}.
