-module(bs04_test).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

-import(bs04, [decode/2]).

decode_test_() ->

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

    JSON2 = <<"
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

    JSON3 = <<"
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

    JSON_proplist =
        [{<<"firstNa me">>,<<"John">>},
         {<<"lastName">>,<<"Smith">>},
         {<<"age">>,25},
         {<<"address">>,
          [{<<"streetAddress">>,<<"21 2nd Street">>},
           {<<"city">>,<<"New York">>},
           {<<"state">>,<<"NY">>},
           {<<"postalCode">>,<<"10021">>}]},
         {<<"phoneNumber">>,
          [[{<<"type">>,<<"home">>},{<<"number">>,<<"212 555-1234">>}],
           [{<<"type">>,<<"fax">>},{<<"number">>,<<"646 555-4567">>}]]}],

    JSON_map =
        #{<<"address">> =>
              #{<<"city">> => <<"New York">>,<<"postalCode">> => <<"10021">>,
                <<"state">> => <<"NY">>,<<"streetAddress">> => <<"21 2nd Street">>},
          <<"age">> => 25,<<"firstNa me">> => <<"John">>,
          <<"lastName">> => <<"Smith">>,
          <<"phoneNumber">> =>
              [#{<<"number">> => <<"212 555-1234">>,<<"type">> => <<"home">>},
               #{<<"number">> => <<"646 555-4567">>,<<"type">> => <<"fax">>}]},

    JSON2_proplist =
        [{<<"squadName">>,<<"Super hero squad">>},
         {<<"homeTown">>,<<"Metro City">>},
         {<<"formed">>,2016},
         {<<"secretBase">>,<<"Super tower">>},
         {<<"active">>,true},
         {<<"members">>,
          [[{<<"name">>,<<"Molecule Man">>},
            {<<"age">>,29},
            {<<"secretIdentity">>,<<"Dan Jukes">>},
            {<<"powers">>,
             [<<"Radiation resistance">>,<<"Turning tiny">>,<<"Radiation blast">>]}],
           [{<<"name">>,<<"Madame Uppercut">>},
            {<<"age">>,39},
            {<<"secretIdentity">>,<<"Jane Wilson">>},
            {<<"powers">>,
             [<<"Million tonne punch">>,<<"Damage resistance">>,
              <<"Superhuman reflexes">>]}],
           [{<<"name">>,<<"Eternal Flame">>},
            {<<"age">>,1000000},
            {<<"secretIdentity">>,<<"Unknown">>},
            {<<"powers">>,
             [<<"Immortality">>,<<"Heat Immunity">>,<<"Inferno">>,<<"Teleportation">>,
              <<"Interdimensional travel">>]}]]}],

    JSON2_map =
        #{<<"active">> => true,<<"formed">> => 2016,
          <<"homeTown">> => <<"Metro City">>,
          <<"members">> =>
              [#{<<"age">> => 29,<<"name">> => <<"Molecule Man">>,
                 <<"powers">> =>
                     [<<"Radiation resistance">>,<<"Turning tiny">>,
                      <<"Radiation blast">>],
                 <<"secretIdentity">> => <<"Dan Jukes">>},
               #{<<"age">> => 39,<<"name">> => <<"Madame Uppercut">>,
                 <<"powers">> =>
                     [<<"Million tonne punch">>,<<"Damage resistance">>,
                      <<"Superhuman reflexes">>],
                 <<"secretIdentity">> => <<"Jane Wilson">>},
               #{<<"age">> => 1000000,<<"name">> => <<"Eternal Flame">>,
                 <<"powers">> =>
                     [<<"Immortality">>,<<"Heat Immunity">>,<<"Inferno">>,
                      <<"Teleportation">>,<<"Interdimensional travel">>],
                 <<"secretIdentity">> => <<"Unknown">>}],
          <<"secretBase">> => <<"Super tower">>,
          <<"squadName">> => <<"Super hero squad">>},

    JSON3_proplist = 
        [{<<"squadName">>,<<"Super hero squad">>},
         {<<"homeTown">>,<<"Metro City">>},
         {<<"formed">>,2016},
         {<<"secretBase">>,<<"Super tower">>},
         {<<"active">>,true},
         {<<"members">>,
          [[{<<"name">>,<<"Molecule Man">>},
            {<<"age">>,29},
            {<<"secretIdentity">>,<<"Dan Jukes">>},
            {<<"powers">>,
             [<<"Radiation resistance">>,<<"Turning tiny">>,<<"Radiation blast">>]}],
           [{<<"name">>,<<"Madame Uppercut">>},
            {<<"age">>,39},
            {<<"secretIdentity">>,<<"Jane Wilson">>},
            {<<"powers">>,
             [<<"Million tonne punch">>,<<"Damage resistance">>,
              <<"Superhuman reflexes">>]}],
           [{<<"name">>,<<"Eternal Flame">>},
            {<<"age">>,1000000},
            {<<"secretIdentity">>,<<"Unknown">>},
            {<<"powers">>,
             [<<"Immortality">>,<<"Heat Immunity">>,<<"Inferno">>,<<"Teleportation">>,
              <<"Interdimensional travel">>]}]]}],

    JSON3_map =
        #{<<"active">> => true,<<"formed">> => 2016,
          <<"homeTown">> => <<"Metro City">>,
          <<"members">> =>
              [#{<<"age">> => 29,<<"name">> => <<"Molecule Man">>,
                 <<"powers">> =>
                     [<<"Radiation resistance">>,<<"Turning tiny">>,
                      <<"Radiation blast">>],
                 <<"secretIdentity">> => <<"Dan Jukes">>},
               #{<<"age">> => 39,<<"name">> => <<"Madame Uppercut">>,
                 <<"powers">> =>
                     [<<"Million tonne punch">>,<<"Damage resistance">>,
                      <<"Superhuman reflexes">>],
                 <<"secretIdentity">> => <<"Jane Wilson">>},
               #{<<"age">> => 1000000,<<"name">> => <<"Eternal Flame">>,
                 <<"powers">> =>
                     [<<"Immortality">>,<<"Heat Immunity">>,<<"Inferno">>,
                      <<"Teleportation">>,<<"Interdimensional travel">>],
                 <<"secretIdentity">> => <<"Unknown">>}],
          <<"secretBase">> => <<"Super tower">>,
          <<"squadName">> => <<"Super hero squad">>},

    ?_assert(decode(JSON, proplist) =:= JSON_proplist),
    ?_assert(decode(JSON, map) =:= JSON_map),
    ?_assert(decode(JSON2, proplist) =:= JSON2_proplist),
    ?_assert(decode(JSON2, map) =:= JSON2_map),
    ?_assert(decode(JSON3, proplist) =:= JSON3_proplist),
    ?_assert(decode(JSON3, map) =:= JSON3_map).
