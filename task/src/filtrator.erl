-module(filtrator).
-behaviour(gen_event).

-export([start_link/0, stop/0, add_handler/2, send_notice/0]).
-export([init/1, handle_event/2, handle_info/2, handle_call/2, terminate/2]).

-record(state, {redisClient, nMax}).


%%API
start_link() ->
  gen_event:start_link({local, ?MODULE}).

add_handler(Module, RedisClient) ->
 gen_event:add_handler(?MODULE, Module, RedisClient).

send_notice() ->
 gen_event:notify(?MODULE, send_notice).

stop() -> gen_event:stop(?MODULE).


%%gen_event callbacks
init(RedisClient) ->
  {ok, #state{redisClient = RedisClient}}.

terminate(_Reason, _State) -> ok.

handle_call(_, State) ->
  {ok, ok, State}.

handle_event(send_notice, State) ->
  erlang:start_timer(0, self(), notice),
  {ok, State};

handle_event(_Event, State) ->
  {ok, State}.

handle_info({timeout, Tref, notice}, #state{redisClient = RedisClient} = State) ->
  erlang:cancel_timer(Tref),
  erlang:start_timer(1000, self(), notice),
  if
    RedisClient /= undefined ->
      RandNums = RedisClient:get_nums(),
      PrimeNums = lists:filtermap(fun(X) -> case is_prime(X) of true -> {true, X}; _ -> false end end, RandNums),
      if erlang:length(PrimeNums) > 0 -> RedisClient:add_prime_nums(PrimeNums); true -> ok end;
    true -> ok
  end,
  {ok, State}.

%%internal function
is_prime(X) -> is_prime(X,2).
is_prime(X,X) -> true;
is_prime(X,Y)->
  ChPrime = X rem Y,
  if 
    ChPrime == 0 -> false;
    true -> is_prime(X, Y+1)
  end.
