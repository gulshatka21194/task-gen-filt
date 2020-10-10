-module(generator).
-behaviour(gen_server).

-export([start_link/2, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(MIN_V, 1).
-record(state, {redisClient, nMax}).

%%API
start_link(Nmax, RedisClient) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [Nmax, RedisClient], []).

stop() -> gen_server:stop(?MODULE).

%%gen_server callbacks
init([Nmax, RedisClient]) ->
  erlang:start_timer(0, self(), generate),
  rand:seed(exs1024s),
  {ok, #state{redisClient = RedisClient,
              nMax = Nmax}}.

handle_call(_, _, State) ->
  {ok, ok, State}.

handle_cast(_, State) ->
  {noreply, State}.

terminate(_Reason, _State) -> ok.

handle_info({timeout, Tref, generate}, #state{redisClient = RedisClient} = State) ->
  erlang:cancel_timer(Tref),
  erlang:start_timer(1000, self(), generate),
  RandNums = lists:map(fun(_) -> rand:uniform(State#state.nMax - ?MIN_V) + ?MIN_V end, lists:seq(1, 3000)),
  RedisClient:add_nums(RandNums),
  {noreply, State}.
