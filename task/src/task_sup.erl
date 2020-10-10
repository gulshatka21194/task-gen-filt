-module(task_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

%%API
start_link([Host, Port, Db, QueueKey, ResultKey, MaxNum]) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Host, Port, Db, QueueKey, ResultKey, MaxNum]).


%%supervisor callback
init([Host, Port, Db, QueueKey, ResultKey, MaxNum]) ->
  ChildList = [child(filtrator, []),
               child(redis_client, [Host, Port, Db, QueueKey, ResultKey, filtrator]),
               child(generator, [MaxNum, redis_client])],
  {ok, {{one_for_all, 4, 1}, ChildList}}.


%%internal function
child(Module, ArgList) ->
  {Module, {Module, start_link, ArgList}, permanent, 1000, worker, [Module]}.
