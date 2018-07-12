-module(ar_alarm).
-include("ar.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start/0,
	stop/0]).

-behavior(gen_event).

-export([init/1, 
	handle_call/2,
	handle_event/2,
	handle_info/2,
	terminate/2,
	code_change/3]).

start() ->
	ok = alarm_handler:add_alarm_handler(?MODULE, []).

stop() ->
	ok = alarm_handler:delete_alarm_handler(?MODULE).
  
init([]) ->
	prometheus_gauge:declare([{name, ar_alarms},
							  {help, "Alarms raised by alertmanager"},
							  {labels, [alarm_name,alarm_param]}]),
	ar:report({ar_alarm, init}),
	{ok, []}.

handle_call(_Req, State) ->
	{ok, badreq, State}.

handle_event({set_alarm, Alarm={process_memory_high_watermark, Pid}}, Alarms) ->
	ar:report({ar_alarm, Alarm},
			  erlang:process_info(Pid)),
	prometheus_gauge:set(ar_alarms, [process_memory_high_watermark,
									 erlang:process_info(Pid,initial_call)], 1),
	{ok, [Alarm | Alarms]};
handle_event({set_alarm, Alarm={AlarmId, AlarmDescr}}, Alarms) ->
	%ar:report({set_alarm, Alarm}),
	prometheus_gauge:set(ar_alarms, [AlarmId, AlarmDescr], 1),
	{ok, [Alarm | Alarms]};

handle_event({clear_alarm, AlarmId}, Alarms) ->
	%ar:report({clear_alarm, AlarmId}),
	case lists:keyfind(AlarmId, 1, Alarms) of
		{AlarmId,AlarmDescr} -> prometheus_gauge:set(ar_alarms, [AlarmId,AlarmDescr], 0);
		false ->  ok
	end,
	{ok, lists:keydelete(AlarmId, 1, Alarms)};

handle_event(Event, State) ->
	ar:report({ar_alarm_unhandled, Event}),
	{ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Arg, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
{ok, State}.
