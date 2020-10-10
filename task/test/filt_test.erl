-module(filt_test).
-include_lib("eunit/include/eunit.hrl").


%%Run tests
run_test_() ->
  {spawn, [connect(),
           send_notice(),
           add_get_prime_nums()]}.

%%Tests

%%Check the connection for success
connect() ->
  {setup,
    fun() -> {ok, Pid} = filtrator:start_link(), Pid end,
    fun(Pid) -> gen_event:stop(Pid) end,
    ?_assertMatch({error,{already_started,_}}, filtrator:start_link())}.


%%Check the ability to send notice
send_notice() ->
  {setup,
    fun() -> {ok, Pid} = filtrator:start_link(),
              filtrator:add_handler(filtrator, undefined), Pid end,
    fun(Pid) -> gen_event:stop(Pid) end,
    ?_assertMatch(ok, filtrator:send_notice())}.


%%Check the ability to add and get prime numbers
add_get_prime_nums() ->
  {setup,
    fun() -> {ok, Pid1} = filtrator:start_link(),
             {ok, Pid2} = redis_client:start_link("localhost", 6379, 0, "qk_filt_test", "rk_filt_test", filtrator),
              timer:sleep(1000),
              redis_client:add_nums([42, 43, 44, 45, 47, 48]),
              [Pid1, Pid2] end,
    fun([Pid1, Pid2]) -> gen_event:stop(Pid1), gen_server:stop(Pid2) end,
    ?_assertEqual([43, 47], redis_client:get_prime_nums())}.
