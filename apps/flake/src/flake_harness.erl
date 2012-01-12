%%%
%%% Copyright 2012, Boundary
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%

-module (flake_harness).
-author ('Dietrich Featherston <d@boundary.com>').

-export ([
	  generate/1,
	  generate/2,
	  timed_generate/1,
	  timed_generate/2
	 ]).

-include_lib("eunit/include/eunit.hrl").

generate(N) ->
    generate_ids(N, undefined, []).

timed_generate(N) ->
    ?debugTime("generating ids", generate(N)).

generate(N, Base) ->
    generate_ids(N, Base, []).

timed_generate(N, Base) ->
    ?debugTime("generating ids", generate(N, Base)).

generate_ids(0, _Base, Acc) ->
    Acc;
generate_ids(N, Base, Acc) ->
    {ok, Flake} = case Base of
		      undefined ->
			  flake_server:id();
		      _ ->
			  flake_server:id(Base)
		  end,
    generate_ids(N-1, Base, [Flake|Acc]).
