-module (flake_harness).
-author ('Dietrich Featherston <d@boundary.com>').
-export ([timed_generate_ids/1,generate_ids/1]).
-include_lib("eunit/include/eunit.hrl").

timed_generate_ids(N) ->
  ?debugTime("generating ids",generate_ids(N)).

generate_ids(0) ->
  ok;
generate_ids(N) ->
  {ok,_Flake} = flake_server:id(62),
  generate_ids(N-1).
