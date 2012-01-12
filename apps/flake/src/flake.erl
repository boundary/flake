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

-module (flake).
-author ('Dietrich Featherston <d@boundary.com>').

%%====================================================================
%% API
%%====================================================================
-export([
	 start/0,
	 start_link/0,
	 stop/0,
	 get_config_value/2
	]).

-include ("flake.hrl").

-include_lib ("eunit/include/eunit.hrl").

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    flake_sup:start_link().

%% @spec start() -> ok
%% @doc Start the snowflake server.
start() ->
    application:start(flake).

%% @spec stop() -> ok
%% @doc Stop the snowflake server.
stop() ->
    Res = application:stop(flake),
    Res.

get_config_value(Key, Default) ->
    case application:get_env(flake, Key) of
	{ok, Value} -> Value;
	_ -> Default
    end.
