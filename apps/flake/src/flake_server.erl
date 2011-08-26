-module (flake_server).
-behaviour (gen_server).
-export ([start_link/1]).
-export ([id/0,id/1]).
-export ([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-record (state, {max_time,worker_id,sequence}).

-include_lib("eunit/include/eunit.hrl").

%% ----------------------------------------------------------
%% API
%% ----------------------------------------------------------

% start and link to a new flake id generator
start_link(Config) ->
  gen_server:start_link({local,flake},?MODULE,Config,[]).

% generate a new snowflake id
id() ->
  respond(gen_server:call(flake,get)).

id(Base) ->
  respond(gen_server:call(flake,{get,Base})).

respond({ok,Flake}) ->
  {ok,Flake};
respond(X) ->
  X.


%% ----------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------

% Example config
% [ {worker_id, int()} ]
%

init([{worker_id,WorkerId}]) ->
  {ok,#state{max_time=flake_util:curr_time_millis(),worker_id=WorkerId,sequence=0}}.

handle_call(get, _From, State = #state{max_time=MaxTime,worker_id=WorkerId,sequence=Sequence}) ->
  {Resp,S0} = get(flake_util:curr_time_millis(),MaxTime,WorkerId,Sequence,State),
  {reply,Resp,S0};

handle_call({get,Base}, _From, State = #state{max_time=MaxTime,worker_id=WorkerId,sequence=Sequence}) ->
  {Resp,S0} = get(flake_util:curr_time_millis(),MaxTime,WorkerId,Sequence,State),
  case Resp of
    {ok,Id} ->
      {reply,{ok,flake_util:as_list(Id,Base)},S0};
    E ->
      {reply,E,S0}
  end;
  
handle_call(X, _From, State) ->
  error_logger:error_msg("unrecognized msg in ~p:handle_call -> ~p~n",[?MODULE,X]),
  {reply,ok,State}.

handle_cast(_, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_, State, _) -> {ok, State}.
  
% clock hasn't moved, increment sequence
get(Time,Time,WorkerId,Seq0,State) ->
  Sequence = Seq0 + 1,
  {{ok,flake_util:gen_id(Time,WorkerId,Sequence)},State#state{sequence=Sequence}};
% clock has progressed, reset sequence
get(CurrTime,MaxTime,WorkerId,_,State) when CurrTime > MaxTime ->
  {{ok,flake_util:gen_id(CurrTime,WorkerId,0)},State#state{max_time=CurrTime,sequence=0}};
% clock is running backwards
get(CurrTime,MaxTime,_WorkerId,_Sequence,State) when MaxTime > CurrTime ->
  {{error,clock_running_backwards},State}.


%% ----------------------------------------------------------
%% tests
%% ----------------------------------------------------------

flake_test() ->
  TS = flake_util:curr_time_millis(),
  Worker = flake_util:hw_addr_to_int(lists:seq(1,6)),
  Flake = flake_util:gen_id(TS,Worker,0),
  ?debugVal(Flake),
  <<Time:64/unsigned-integer,WorkerId:48/unsigned-integer,Sequence:16/unsigned-integer>> = <<Flake:128/unsigned-integer>>,
  ?assert(?debugVal(Time) =:= TS),
  ?assert(?debugVal(Worker) =:= WorkerId),
  ?assert(?debugVal(Sequence) =:= 0),
  ?debugVal(flake_util:as_list(Flake,62)),
  test_gen_server(),
  ok.

test_gen_server() ->
  Worker = flake_util:hw_addr_to_int(lists:seq(1,6)),
  {ok,FlakeServer} = flake_server:start_link([{worker_id,Worker}]),
  ?debugVal(flake_server:id()),
  ?debugVal(flake_server:id(62)),
  ok.
