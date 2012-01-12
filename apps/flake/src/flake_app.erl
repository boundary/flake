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

%% @doc Callbacks for the snowflake application.

-module (flake_app).
-author ('Dietrich Featherston <d@boundary.com>').

-behaviour (application).

-export([
	 start/2,
	 stop/1
	]).

-include_lib ("eunit/include/eunit.hrl").

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for snowflake.
start(_Type, _StartArgs) ->
    flake_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for snowflake.
stop(_State) ->
    ok.
