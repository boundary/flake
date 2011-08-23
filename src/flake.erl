-module (flake).
-behaviour (gen_server).
-export ([new/1]).
-export ([id/0]).
-export ([get_if_hw_int/0,get_if_hw_int/1]).
-export ([next_id/3]).
-record (state, {max_time,epoch,worker_id,sequence}).

-include_lib("eunit/include/eunit.hrl").

%% ----------------------------------------------------------
%% API
%% ----------------------------------------------------------

new_from_if(IfName) ->
  new([
    {worker_id,get_if_hw_int(IfName)}
  ]).

% start and link to a new flake id generator
new(Config) ->
  gen_server:start_link({local,flake},?MODULE,[Config],[]).

% generate a new snowflake id
id() ->
  gen_server:call(flake,id).


%% ----------------------------------------------------------
%% gen_server callbacks
%% ----------------------------------------------------------

% Example config
% [ {worker_id,int()} ]
%

init(Config) ->
  WorkerId = proplists:get_value(worker_id,Config),
  MaxTime   = curr_time(),
  Sequence  = 0,
  {ok,#state{max_time=curr_time(),worker_id=WorkerId,sequence=Sequence}}.

handle_call(id, _From, State = #state{max_time=MaxTime,epoch=Epoch,worker_id=WorkerId,sequence=Sequence}) ->
  {Resp,S0} = next_id(curr_time(),MaxTime,WorkerId,Sequence,State),
  {reply,Resp,S0};
  
handle_call(X, _From, State) ->
  error_logger:error_msg("unrecognized msg in ~p:handle_call -> ~p~n",[?MODULE,X]),
  {reply,ok,State}.

handle_cast(_, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_, State, _) -> {ok, State}.
  
% clock is running backwards
next_id(CurrTime,MaxTime,WorkerId,Sequence,State) when MaxTime > CurrTime ->
  {{error,clock_running_backwards},State};
next_id(CurrTime,MaxTime,WorkerId,_,State) when CurrTime > MaxTime ->
  {next_id(CurrTime,WorkerId,0),State#state{sequence=0}};
next_id(Time,Time,WorkerId,Seq0,State) ->
  Sequence = Seq0 + 1,
  {next_id(Time,WorkerId,Sequence),State#state{sequence=Sequence}}.

next_id(Time,WorkerId,Sequence) ->
  ( Time bsl 64 ) + ( WorkerId bsl 16 ) + Sequence.

%% ----------------------------------------------------------
%% helpers
%% ----------------------------------------------------------

get_if_hw_int() ->
  get_if_hw_int("eth0").

get_if_hw_int(IfName) ->
  {ok,IfAddrs} = inet:getifaddrs(),
  IfProps = proplists:get_value(IfName,IfAddrs),
  HwAddr  = proplists:get_value(hwaddr,IfProps),
  <<WorkerId:48/integer>> = erlang:list_to_binary(HwAddr).

curr_time() ->
  {MegaSec,Sec,MicroSec} = erlang:now(),
  1000000000*MegaSec + Sec*1000 + MicroSec/1000.

flake_test() ->
  % Time = curr_time(),
  % WorkerId = get_if_hw_int(),
  % Flake = next_id(curr,WorkerId,0),
  % FlakeBin = erlang:term_to_binary(Flake),
  % ?debugVal(Flake),
  % ?debugVal(FlakeBin),
  ?assert(true).
