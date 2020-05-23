%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2020
%%%-------------------------------------------------------------------
-module(mrowka).
-author("Michal Stanisz").

-behaviour(gen_server).

%% API
-export([start_link/3, start/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(mrowka_state, {
    id :: atom(),
    current_vertex :: atom(),
    module :: module()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Id, V, Module) ->
    gen_server:start_link({local, Id}, ?MODULE, [Id, V, Module], []).

start(Id) ->
    io:format("Mrowka ~p starting~n", [Id]),
    gen_server:cast(Id, {start}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #mrowka_state{}} | {ok, State :: #mrowka_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Id, V, Module]) ->
    {ok, #mrowka_state{current_vertex = V, id = Id, module = Module}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #mrowka_state{}) ->
    {reply, Reply :: term(), NewState :: #mrowka_state{}} |
    {reply, Reply :: term(), NewState :: #mrowka_state{}, timeout() | hibernate} |
    {noreply, NewState :: #mrowka_state{}} |
    {noreply, NewState :: #mrowka_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #mrowka_state{}} |
    {stop, Reason :: term(), NewState :: #mrowka_state{}}).
handle_call(_Request, _From, State = #mrowka_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #mrowka_state{}) ->
    {noreply, NewState :: #mrowka_state{}} |
    {noreply, NewState :: #mrowka_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mrowka_state{}}).
handle_cast({start}, State = #mrowka_state{id = Id}) ->
    gen_server:cast(Id, {move_to_next}),
    {noreply, State};
handle_cast({move_to_next}, State = #mrowka_state{current_vertex = V, id = Id, module = Module}) ->
    io:format("Mrowka ~p is in vertex ~p~n", [Id, V]),
    {ok, N} = vertex:get_neighbours(V),
    {Next, Cost} = Module:select_next(N),
    vertex:add_pheromon(V, Next, 1),
    io:format("Mrowka ~p moving to vertex ~p~n", [Id, Next]),
    timer:sleep(Cost * 400),
    gen_server:cast(Id, {move_to_next}),
    {noreply, State#mrowka_state{current_vertex = Next}};
handle_cast(_Request, State = #mrowka_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #mrowka_state{}) ->
    {noreply, NewState :: #mrowka_state{}} |
    {noreply, NewState :: #mrowka_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #mrowka_state{}}).
handle_info(_Info, State = #mrowka_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #mrowka_state{}) -> term()).
terminate(_Reason, _State = #mrowka_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #mrowka_state{},
    Extra :: term()) ->
    {ok, NewState :: #mrowka_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #mrowka_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
