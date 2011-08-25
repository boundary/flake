%% @doc Callbacks for the snowflake application.

-module (flake_app).
-author('Dietrich Featherston <d@boundary.com>').

-behaviour(application).
-export([start/2,stop/1]).

-include_lib("eunit/include/eunit.hrl").

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for snowflake.
start(_Type, _StartArgs) ->
  flake_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for snowflake.
stop(_State) ->
  ok.
