%% coding: utf-8
%BS04: Разобрать JSON

-module(bs04).
-export([start/0]).
-export([decode/2]).

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

    Parsed = decode(JSON3, proplist),
    io:fwrite( "\n\nResult: \n" ),
    io:fwrite("~p~n",[ Parsed ]).

decode(Bin, ResultType) ->
    {Result, _} = parse(Bin, ResultType),
    Result.

parse(<<>>, _ResultType) ->
    {<<>>, <<>>};
parse(OrigBin, ResultType) ->
    Bin = spaces_cleaner(OrigBin),
    case Bin of
        <<>> -> <<>>;
        <<_Char/utf8, _Rest/binary>> -> detect_type(Bin, ResultType)
    end.

detect_type(<<Char/utf8, _Rest/binary>>=Bin, ResultType) ->
    case <<Char>> of
        <<"{">> -> extract_obj_items(Bin, ResultType);
        <<"[">> -> extract_list_items(Bin, ResultType);
        <<"\"">> -> extract_quoted(Bin);
        <<"'">> -> extract_quoted(Bin);
        <<A>> when A >= 48, A =< 57 -> extract_numeric(Bin);
        <<A>> when A >= 97, A =< 122 -> extract_unquoted(Bin)
    end.

extract_obj_items(<<_, _/binary>>=OrigBin, ResultType) ->
    <<Char/utf8, Rest/binary>>=Bin = spaces_cleaner(OrigBin),
    case <<Char>> of
        <<"\"">> -> process_pairs(Bin, ResultType);
        <<"'">> -> process_pairs(Bin, ResultType);
        <<"{">> -> process_pairs(Rest, ResultType);
        <<"}">> ->
            case ResultType of
                map ->
                    {#{}, Rest};
                proplist ->
                    {[], Rest}
            end;
        <<",">> -> extract_obj_items(Rest, ResultType);
        <<A>> when A >= 48, A =< 122 -> process_pairs(Bin, ResultType)
    end;
extract_obj_items(<<>>, map) ->
    {#{}, <<>>};
extract_obj_items(<<>>, proplist) ->
    {[], <<>>}.

process_pairs(<<_, _/binary>>=OrigBin, ResultType) ->
    <<Char/utf8, Rest/binary>>=Bin = spaces_cleaner(OrigBin),
    case <<Char>> of
        <<"}">> -> {#{}, Rest};
        <<_>> ->
            {{Key, Value}, Rest2} = extract_pair(Bin, ResultType),
            {Map2, Rest3} = extract_obj_items(Rest2, ResultType),
            case ResultType of
                map ->
                    Map1 = #{Key=>Value},
                    {maps:merge(Map1, Map2), Rest3};
                proplist ->
                    Map1 = {Key, Value},
                    {[Map1 | Map2], Rest3}
            end
    end;
process_pairs(<<>>, map) ->
    {#{}, <<>>};
process_pairs(<<>>, proplist) ->
    {[], <<>>}.

extract_pair(Bin, ResultType) ->
    {Key, Bin2} = extract_key(Bin),
    Bin2a = del_dots(Bin2),
    {Value, Rest3} = extract_value(Bin2a, ResultType),
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
    <<Char/utf8, _Rest/binary>>=Bin = spaces_cleaner(OrigBin),
    case <<Char>> of
        <<"\"">> -> extract_quoted(Bin);
        <<"'">> -> extract_quoted(Bin);
        <<A>> when A >= 48, A =< 57 -> extract_numeric(Bin);
        <<A>> when A >= 64, A =< 122 -> extract_unquoted(Bin)
    end.

extract_value(OrigBin, ResultType) ->
    parse(OrigBin, ResultType).

extract_list_items(<<>>, _ResultType) ->
    {[], <<>>};
extract_list_items(OrigBin, ResultType) ->
    <<Char/utf8, Rest/binary>>=Bin = spaces_cleaner(OrigBin),
    case <<Char>> of
        <<"[">> -> extract_list_items(Rest, ResultType);
        <<",">> -> extract_list_items(Rest, ResultType);
        <<"]">> -> {[], Rest};
        <<_>> -> 
            {Obj1, Rest1} = parse(Bin, ResultType),
            {List2, Rest2} = extract_list_items(Rest1, ResultType),
            {[Obj1 | List2], Rest2}
    end.

extract_quoted(<<"\"", Rest/binary>>) ->
    extract_quoted1(Rest);
extract_quoted(<<"'", Rest/binary>>) ->
    extract_quoted1(Rest).
extract_quoted1(<<Char/utf8, Rest/binary>>) ->
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
