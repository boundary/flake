-module (flake).
-author ('Dietrich Featherston <d@boundary.com>').
%%====================================================================
%% API
%%====================================================================
-export([start/0, start_link/0, stop/0]).
-export([get_config_value/2]).

-include("flake.hrl").

-include_lib("eunit/include/eunit.hrl").

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
