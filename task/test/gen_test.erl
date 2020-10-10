-module(gen_test).
-include_lib("eunit/include/eunit.hrl").


%%Run tests
run_test_() ->
  {spawn, [connect(),
           generate_nums(),
           generate_required_count_nums()]}.

%%Internal functions
start() ->
  {ok, PidRed} = redis_client:start_link("localhost", 6379, 0, "qk_gen_test", "", undefined),
  {ok, PidGen} = generator:start_link(3000, redis_client),
  {PidRed, PidGen}.

stop({PidRed, PidGen}) ->
  redis_client:del_nums(),
  gen_server:stop(PidGen),
  gen_server:stop(PidRed).

%%Tests

%%Check the connection for success
connect() ->
  {setup,
    fun() -> {ok, Pid} = generator:start_link(10, redis_client), Pid end,
    fun(Pid) -> gen_server:stop(Pid) end,
    ?_assertMatch({error,{already_started,_}}, generator:start_link(10, redis_client))}.


%%Check the ability to generate numbers
generate_nums() ->
  {setup,
    fun start/0,
    fun stop/1,
    fun(_) -> timer:sleep(1000),
    		  Nums = redis_client:get_nums(),
      		  ?_assert(length(Nums) /= 0) end}.


%%Check the ability to generate required count of numbers in 1sec
generate_required_count_nums() ->
  {setup,
    fun start/0,
    fun stop/1,
    fun(_) -> timer:sleep(1000),
    		  Nums = redis_client:get_nums(),
      		  ?_assert(length(Nums) == 3000) end}.