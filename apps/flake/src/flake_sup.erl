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

-module (flake_sup).
-author('Dietrich Featherston <d@boundary.com>').
-include("flake.hrl").

-include_lib("eunit/include/eunit.hrl").

-define (DEBUG,debug).

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list([Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),
  
    sets:fold(
      fun(Id, ok) ->
	      supervisor:terminate_child(?MODULE, Id),
	      supervisor:delete_child(?MODULE, Id),
	      ok
      end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    If = flake:get_config_value(interface, "eth0"),
    error_logger:info_msg("starting flake with hardware address of ~p as worker id~n", [If]),
    {ok,WorkerId} = flake_util:get_if_hw_int(If),
    error_logger:info_msg("using worker id: ~p~n", [WorkerId]),
  
    FlakeConfig = [
		   {worker_id, WorkerId}
		  ],
    Flake = {flake,
	     {flake_server, start_link, [FlakeConfig]},
	     permanent, 5000, worker, [flake_server]},
    
    TimestampPath = flake:get_config_value(timestamp_path, "/tmp/flake-timestamp-dets"),
    AllowableDowntime = flake:get_config_value(allowable_downtime, 0),

    {ok, TimestampTable} =
	dets:open_file(timestamp_table,[
					{estimated_no_objects, 10},
					{type, set},
					{file, TimestampPath}
				       ]),

    {ok,TS} = persistent_timer:read_timestamp(TimestampTable),
    ?debugVal(TS),
    Now = flake_util:curr_time_millis(),
    ?debugVal(Now),
    TimeSinceLastRun = Now - TS,

    %% fail startup if
    %% 1) the clock time last recorded is later than the current time
    %% 2) the last recorded time is more than N ms in the past to prevent
    %%    generating future ids in the event that the system clock is set to some point far in the future
    check_for_clock_error(Now >= TS, TimeSinceLastRun < AllowableDowntime),

    error_logger:info_msg("saving timestamps to ~p every 1s~n", [TimestampPath]),
    TimerConfig = [
		   {table, TimestampTable},
		   {interval, 1000}
		  ],
    PersistentTimer = {persistent_timer,
		       {persistent_timer,start_link,[TimerConfig]},
		       permanent, 5000, worker, [persistent_timer]},
    
    {ok, { {one_for_one, 10, 10}, [Flake, PersistentTimer]} }.

check_for_clock_error(true,true) ->
    ok;
check_for_clock_error(false,_) ->
    error_logger:error_msg("system running backwards, failing startup of snowflake service~n"),
    exit(clock_running_backwards);
check_for_clock_error(_,false) ->
    error_logger:error_msg("system clock too far advanced, failing startup of snowflake service~n"),
    exit(clock_advanced).
