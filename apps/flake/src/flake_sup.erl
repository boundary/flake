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
  If = flake:get_config_value("interface","en0"),
  {ok,WorkerId} = flake_util:get_if_hw_int(If),
  Config = [
      {worker_id,WorkerId}
    ],
  Flake = {flake,
    {flake_server,start_link,[Config]},
    permanent, 5000, worker, [flake_server]},
    
  % SyslogOpts = flake:get_config_value(syslog, [{hostname, "syslog.cid1.boundary.com"}, {facility, local5}]), 
  % Syslog = {syslog, {syslog, start_link, [flake, 
  %                                         flake, 
  %                                         proplists:get_value(hostname, SyslogOpts),
  %                                         514, 
  %                                         proplists:get_value(facility, SyslogOpts)]}, permanent, 2000, worker, [syslog]},
  Processes = [Flake],
  {ok, { {one_for_one, 10, 10}, Processes} }.
