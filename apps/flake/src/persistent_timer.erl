-module (persistent_timer).
-behaviour (gen_server).
-export ([start_link/1]).
-export ([write_timestamp/1,read_timestamp/1]).
-export ([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-record (state, {path}).

-include_lib("eunit/include/eunit.hrl").

%% ----------------------------------------------------------
%% API
%% ----------------------------------------------------------

% start and link to a new flake id generator
start_link(Config) ->
  gen_server:start_link({local,persistent_clock},?MODULE,Config,[]).

%% ----------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------

init(Config) ->
  Path = proplists:get_value(path,Config),
  Interval = proplists:get_value(interval,Config,1000),
  {ok,TimerRef} = timer:send_interval(Interval,save),
  {ok,#state{path=Path}}.

handle_call(_, _From, State) -> {reply,ok,State}.

handle_cast(_, State) -> {noreply, State}.

handle_info(save, State = #state{path=Path}) ->
  write_timestamp(Path),
  TS = flake_util:curr_time_millis(),
  {ok,_} = write_timestamp(Path),
  {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_, State, _) -> {ok, State}.

%% ----------------------------------------------------------
%% utils
%% ----------------------------------------------------------

% write the current time stamp to disk
% {ok,Timestamp=int()} | {error,Reason}
write_timestamp(Path) ->
  TS = flake_util:curr_time_millis(),
  L = io_lib:format("~p~n",[TS]),
  Bin = erlang:list_to_binary(L),
  case file:write_file(Path,Bin) of
    ok ->
      {ok,TS};
    X ->
      X
  end.

% read the timestamp from the given file. will write the current timestamp to disk if the file does not exist
% {ok,Timestamp=int()} | {error,Reason}
read_timestamp(Path) ->
  case file:read_file(Path) of
    {ok,Bin} ->
      {ok,to_int(Bin)};
    {error,enoent} ->
      write_timestamp(Path);
    X ->
      ?debugVal(X),
      X
  end.

to_int(B) when is_binary(B) ->
  to_int(erlang:binary_to_list(B));
to_int(L) ->
  [I|_] = re:replace(L, "\\s+", "", [global]),
  erlang:list_to_integer(erlang:binary_to_list(I)).

%% ----------------------------------------------------------
%% tests
%% ----------------------------------------------------------

persistent_clock_test() ->
  Path = "/tmp/timestamp",
  {ok,TS0} = write_timestamp(Path),
  {ok,TS1} = read_timestamp(Path),
  ?assert(?debugVal(TS0) =:= ?debugVal(TS1)).