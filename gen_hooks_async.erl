
-module(gen_hooks_async).
%%%-------------------------------------------------------------------
%%%author  : kiran <kiran.khaladkar@geodesic.com>
%%% Description : 
%%%
%%% Created :7 Jan 2009 
%%%-------------------------------------------------------------------

-behaviour(gen_server).
-export([add/3, delete/3, invoke/3]).
%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function : add() -> {ok| error | present}
%% Description : adds an api function to the gen server
%%--------------------------------------------------------------------
add(Module, Function, Arity) ->
	gen_server:call(gen_hooks_async, {add ,Module, Function, Arity}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function : delete() -> {ok| error | absent}
%% Description : deletes an api function from the gen server
%%--------------------------------------------------------------------
delete(Module, Function, Arity) ->
	gen_server:call(gen_hooks_async, {delete, Module, Function, Arity}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function : invoke() -> {{ok, result} | error | absent}
%% Description : calls an api function stored the gen server
%%--------------------------------------------------------------------
invoke(Module, Function, Arguments) ->
	gen_server:call(gen_hooks_async, {invoke, Module, Function, Arguments}, infinity).

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
	State = [],
	{ok, State}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call({add, M, F, A}, From, State) ->
	gen_server:cast(self(), {{add, M, F, A}, From}),
	Reply = ok,
	{reply, Reply, State};

handle_call({delete, M,F,A}, From, State) ->
	gen_server:cast(self(), {{delete, M, F, A}, From}),
	Reply = ok,
	{reply, Reply, State};

handle_call({invoke, M,F,A}, From, State) ->
	gen_server:cast(self(), {{invoke, M, F, A}, From}),
	Reply = ok,
	{reply, Reply, State};

handle_call(_Request, _From, State) ->
	Reply = ok,
	_From!hehe,
	{reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------

handle_cast({{add, M, F, A}, From}, State) ->
	%%check if module is loaded
	case search_hook({M,F,A}, State) of
	found ->
		%io:format("####################"),
		%io:format("something"),
		Reply = replication,
		NewState = State;
	notfound ->
		%io:format("~w", [[M, F, A]]),
		Reply = added,
		NewState = [{M,F,A}|State]
	end,
	%self()!{send, From, Reply},	
	spawn(fun() ->gen_server:cast(self(), {send, From, Reply}) end),
	{noreply, NewState};

handle_cast({{delete, M, F, A}, From}, State) ->
	case search_hook({M,F,A}, State) of
	found ->
		NewState = lists:delete({M,F,A}, State),
		Reply = deleted;
	notfound ->
		Reply = notdeleted,
		NewState = State
	end,
	%From!{Reply},
	spawn(fun() ->gen_server:cast(self(), {send, From, Reply}) end),
	{reply,  NewState};

handle_cast({{invoke, M, F, A},From }, State) ->
	case search_hook({M,F,length(A)}, State) of
	found ->
		%io:format("~w", [M]),
		case code:is_loaded(M) of
		{file, _} ->
			Reply = apply(M,F,A);
		_ -> 
			case code:load_file(M) of
			{module, Module} ->
				Reply = apply(M,F,A);
			Other ->
				Reply = Other
			end
	end;
	notfound ->
		Reply = badfun
	end,
	%From!{Reply},
	io:format("@@@@@@@~p~n", [Reply]),
	spawn(fun() ->gen_server:cast(self(), {send, From, Reply}) end),
	{reply, State};

handle_cast({send, To, Reply}, State) ->
	%io:format("!!!!!!!!!!!!!!"),
	To!Reply,
	{noreply, State};

handle_cast(_Msg, State) ->
	{reply, State}.
%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({send, To, Reply},State) ->
	To!Reply, 
	{noreply, State};
handle_info(_Info, State) ->
     {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
     ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
     {ok, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
search_hook({M,F,A}, [{M,F,A}|Rest] )->
	found;
search_hook(MFA, [_, Rest]) ->
	search_hook(MFA, Rest);
search_hook(_, []) ->
	notfound.
