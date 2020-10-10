-module(redis_client).
-behaviour(gen_server).

-export([start_link/6, get_nums/0, add_prime_nums/1, get_prime_nums/0, add_nums/1, del_nums/0, del_prime_nums/0, stop/0]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3, terminate/2]).

-record(state, {redisClient, queueKey, resultSetKey, filtratorMod}).


%%API
start_link(RedisHost, RedisPort, RedisDB, QueueKey, ResultSetKey, FiltratorMod) ->
  Args = {RedisHost, RedisPort, RedisDB, QueueKey, ResultSetKey, FiltratorMod},
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

get_nums() ->
  gen_server:call(?MODULE, get_nums).

get_prime_nums() ->
  gen_server:call(?MODULE, get_prime_nums).

del_nums() ->
  gen_server:call(?MODULE, del_nums).

del_prime_nums() ->
  gen_server:call(?MODULE, del_prime_nums).

add_nums(RandNums) when is_list(RandNums) ->
  gen_server:cast(?MODULE, {add_nums, RandNums}).

add_prime_nums(PrimeNums) when is_list(PrimeNums) ->
  gen_server:cast(?MODULE, {add_prime_nums, PrimeNums}).

stop() -> gen_server:stop(?MODULE).


%%gen_server callbacks
init({Host, Port, DB, QueueKey, ResultSetKey, FiltratorMod}) ->
  {ok, Pid} = eredis:start_link(Host, Port, DB),
  if
    FiltratorMod /= undefined -> 
      FiltratorMod:add_handler(FiltratorMod, redis_client),
      FiltratorMod:send_notice();
    true -> ok
  end,
  {ok, #state{redisClient = Pid, queueKey = QueueKey, resultSetKey = ResultSetKey, filtratorMod = FiltratorMod}}.

terminate(_Reason, _State) -> ok.

handle_call(get_nums, _From, #state{redisClient = RedisClient, queueKey = QueueKey} = State) ->
  % [{ok, Nums}, _] = eredis:qp(RedisClient, [["LRANGE", QueueKey, 0, -1], ["DEL", QueueKey]]),
  {ok, Nums} = eredis:q(RedisClient, ["LRANGE", QueueKey, 0, -1]),
  {reply, lists:map(fun(X) -> binary_to_integer(X) end, lists:reverse(Nums)), State};
handle_call(get_prime_nums, _From, #state{redisClient = RedisClient, resultSetKey = ResultSetKey} = State) ->
  {ok, PrimeNums} = eredis:q(RedisClient, ["SMEMBERS", ResultSetKey]),
  {reply, lists:map(fun(X) -> binary_to_integer(X) end, PrimeNums), State};
handle_call(del_nums, _From, #state{redisClient = RedisClient, queueKey = QueueKey} = State) ->
  eredis:q(RedisClient, ["DEL", QueueKey]),
  {reply, ok, State};
handle_call(del_prime_nums, _From, #state{redisClient = RedisClient, resultSetKey = ResultSetKey} = State) ->
  eredis:q(RedisClient, ["DEL", ResultSetKey]),
  {reply, ok, State}.

handle_cast({add_nums, RandNums}, #state{redisClient = RedisClient, queueKey = QueueKey} = State) ->
  % io:format("RandNums = ~p~n", [RandNums]),
  eredis:q(RedisClient, ["LPUSH", QueueKey] ++ lists:map(fun(X) -> integer_to_list(X) end, RandNums)),
  {noreply, State};
handle_cast({add_prime_nums, PrimeNums}, #state{redisClient = RedisClient, resultSetKey = ResultSetKey} = State) ->
  % io:format("PrimeNums = ~p~n", [PrimeNums]),
  eredis:q(RedisClient, ["SADD", ResultSetKey] ++ lists:map(fun(X) -> integer_to_list(X) end, PrimeNums)),
  {noreply, State}.

handle_info(_, State) ->
  {noreply, State}.
