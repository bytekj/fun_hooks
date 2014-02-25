-module(test_hooks).
-export([loop/1, run/0, run1/0]).
loop(Arg) ->
	timer:sleep(timer:seconds(Arg)),
	io:format("~p", [Arg]),
	finished.
run() ->
	spawn(fun() ->add() end).
run1() ->
	spawn(fun() ->invoke() end).
add() ->
	gen_hooks_async:add(test_hooks, loop, 1),
	%io:format("%%%%%%%%%"),
	receive
	{Msg} ->
		io:format("~p", [Msg])
	end.
invoke() ->
	gen_hooks_async:invoke(test_hooks, loop, [3]),
	receive
	{Msg1} ->
		io:format("~p", [Msg1])
	end.
