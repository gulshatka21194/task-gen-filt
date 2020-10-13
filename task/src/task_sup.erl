-module(task_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

%%API
start_link([Host, Port, Db, QueueKey, ResultKey, MaxNum]) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Host, Port, Db, QueueKey, ResultKey, MaxNum]).

%%supervisor callback
init([Host, Port, Db, QueueKey, ResultKey, MaxNum]) ->
  SupSpecList = #{strategy => one_for_all,
   		  		  intensity => 10,
   		  		  period => 1000},
  ChildList = [child(filtrator, []),
               child(redis_client, [Host, Port, Db, QueueKey, ResultKey, filtrator]),
               child(generator, [MaxNum, redis_client])],
  {ok, {SupSpecList, ChildList}}.


%%internal function
child(Module, ArgList) ->
  #{id => Module,
   start => {Module, start_link, ArgList},
   restart => permanent,
   shutdown => 1000,
   type => worker,
   modules => [Module]}.
