-module(redis_test).
-include_lib("eunit/include/eunit.hrl").


%%Run tests
run_test_() ->
  {spawn, [connect(),
           add_get_nums(),
           add_get_prime_nums(),
           add_unique_prime_nums(),
           del_nums(),
           del_prime_nums()]}.

%%Internal functions
start() ->
  {ok, Pid} = redis_client:start_link("localhost", 6379, 0, "qk_redis_test", "rk_redis_test", undefined), Pid.

stop(Pid) ->
  redis_client:del_nums(),
  redis_client:del_prime_nums(),
  gen_server:stop(Pid).


%%Tests

%%Check the connection for success
connect() ->
  {setup,
    fun start/0,
    fun stop/1,
    ?_assertMatch({error,{already_started,_}}, redis_client:start_link("localhost", 6379, 0, "", "", undefined))}.


%%Check the ability to add and get numbers    
add_get_nums() ->
  {setup,
    fun start/0,
    fun stop/1,
    fun(_) -> redis_client:add_nums([42, 43, 44, 45, 47, 48]), 
              ?_assertEqual([42, 43, 44, 45, 47, 48], redis_client:get_nums()) end}.


%%Check the ability to add and get prime numbers
add_get_prime_nums() ->
  {setup,
    fun start/0,
    fun stop/1,
    fun(_) -> redis_client:add_prime_nums([43, 47, 53, 37]),
             ?_assertEqual([37, 43, 47, 53], redis_client:get_prime_nums()) end}.


%%Check the ability to add unique prime numbers
add_unique_prime_nums() ->
  {setup,
    fun start/0,
    fun stop/1,
    fun(_) -> redis_client:add_prime_nums([43, 47, 53, 37]),
              redis_client:add_prime_nums([47, 53]),
              ?_assertEqual([37, 43, 47, 53], redis_client:get_prime_nums()) end}.


%%Check the ability to delete numbers from queue
del_nums() ->
  {setup,
    fun start/0,
    fun stop/1,
    fun(_) -> redis_client:add_nums([42, 43, 44, 45, 47, 48]),
              redis_client:del_nums(),
              ?_assertEqual([], redis_client:get_nums()) end}.


%%Check the ability to delete numbers from set
del_prime_nums() ->
  {setup,
    fun start/0,
    fun stop/1,
    fun(_) -> redis_client:add_prime_nums([43, 47, 53, 37]),
              redis_client:del_prime_nums(),
              ?_assertEqual([], redis_client:get_prime_nums()) end}.
