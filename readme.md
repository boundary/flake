# Flake: A decentralized, k-ordered id generation service in Erlang


Flake produces 128-bit, k-ordered ids (read time-ordered lexically). Run one on each node in your infrastructure and they will generate conflict-free ids on-demand without  coordination.

To get started

	git clone git://github.com/boundary/flake.git
	
Then edit <tt>rel/files/sys.config</tt> to fit your environment.

* <tt>interface</tt> - set to an available network interface from which to pull a 48-bit mac address as the worker id.
* <tt>timestamp_path</tt> - set to a location where flake can periodically save the current time. If flake detects on startup that this file contains timestamps in the future or the distant past, it will refuse to startup. This is to prevent problematic ids from being distributed.
* <tt>allowable_downtime</tt> - an added safeguard to prevent flake from starting up if it sees it hasn't run in a long period of time according to the system clock since this might be an indication that the clock has been skewed far into the future.

Example configuration:

	[
	 {flake, [
	    {interface, "en0"},
	    {timestamp_path, "/srv/flake/timestamp-dets"},
	    {allowable_downtime, 2592000000}
	  ]}
	].

Then simply run the server inline

	./start.sh

And use the embedded test harness to ensure that you are able to generate ids.

Generate 1 id and receive the erlang binary

	(flake@localhost)1> flake_harness:generate(1).

	[<<0,0,1,52,212,33,45,67,16,154,221,94,14,143,0,0>>]

Generate 10 base-62 encoded ids

	(flake@localhost)2> flake_harness:generate(10,62).

	["8HFaR8qWtRlGDHnO57","8HFaR8qWtRlGDHnO56",
	 "8HFaR8qWtRlGDHnO55","8HFaR8qWtRlGDHnO54",
	 "8HFaR8qWtRlGDHnO53","8HFaR8qWtRlGDHnO52",
	 "8HFaR8qAulTgCBd6Wp","8HFaR8qAulTgCBd6Wo",
	 "8HFaR8qAulTgCBd6Wn","8HFaR8qAulTgCBd6Wm"]

Time how long it takes to generate 100,000 ids

	(flake@localhost)3> flake_harness:timed_generate(100000).

	src/flake_harness.erl:33: generating ids: 0.402 s

To use in practice, simply send a gen call to the flake server to fetch an id.

	flake() ->
	    Node = {flake, flake@localhost},
	    {ok, FlakeId} = gen_server:call(Node, get),
	    {ok, FlakeIdBase62} = gen_server:call(Node, {get,62}),
		%% example id decomposition for demonstration only
    	%% <<_Time:64/integer,_WorkerId:48/integer,_Sequence:16/integer>> = FlakeId,
   	 	FlakeId.



