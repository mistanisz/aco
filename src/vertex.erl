%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2020
%%%-------------------------------------------------------------------
-module(vertex).
-author("Michal Stanisz").

-behaviour(gen_server).

%% API
-export([start_link/1, add_neighbour/3, get_neighbours/1, add_pheromon/2, add_pheromon/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(vertex_state, {
    id :: atom(),
    neighbours = #{} :: #{atom() => {non_neg_integer(), non_neg_integer()}}
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Id) ->
    gen_server:start_link({local, Id}, ?MODULE, Id, []).

add_neighbour(Id, N, Weight) ->
    gen_server:cast(Id, {add_neighbour, N, Weight}).

get_neighbours(Id) ->
    gen_server:call(Id, {get_neighbours}).

add_pheromon(Id, Value) ->
    gen_server:cast(Id, {add_pheromon_all_edges, Value}).
    
add_pheromon(Id, N, Value) ->
    gen_server:cast(N, {add_pheromon, Id, Value}),
    gen_server:cast(Id, {add_pheromon, N, Value}).
    

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #vertex_state{}} | {ok, State :: #vertex_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init(Id) ->
    {ok, #vertex_state{id = Id}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #vertex_state{}) ->
    {reply, Reply :: term(), NewState :: #vertex_state{}} |
    {reply, Reply :: term(), NewState :: #vertex_state{}, timeout() | hibernate} |
    {noreply, NewState :: #vertex_state{}} |
    {noreply, NewState :: #vertex_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #vertex_state{}} |
    {stop, Reason :: term(), NewState :: #vertex_state{}}).
handle_call({get_neighbours}, _From, State = #vertex_state{neighbours = N}) ->
    {reply, {ok, N}, State};
handle_call(_Request, _From, State = #vertex_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #vertex_state{}) ->
    {noreply, NewState :: #vertex_state{}} |
    {noreply, NewState :: #vertex_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #vertex_state{}}).
handle_cast({add_neighbour, Id, Weight}, State = #vertex_state{neighbours = N}) ->
    {noreply, State#vertex_state{neighbours = N#{Id => {Weight, 0}}}};
handle_cast({add_pheromon_all_edges, Value}, State = #vertex_state{neighbours = Neighbours}) ->
    NewNeighbours = maps:map(fun(_Key, {Cost, OldPheromon}) -> {Cost, OldPheromon + Value} end, Neighbours),
    {noreply, State#vertex_state{neighbours = NewNeighbours}};
handle_cast({add_pheromon, N, Value}, State = #vertex_state{neighbours = Neighbours}) ->
    {Cost, OldPheromon} = maps:get(N, Neighbours),
    {noreply, State#vertex_state{neighbours = Neighbours#{N => {Cost, OldPheromon + Value}}}};
handle_cast(_Request, State = #vertex_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #vertex_state{}) ->
    {noreply, NewState :: #vertex_state{}} |
    {noreply, NewState :: #vertex_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #vertex_state{}}).
handle_info(print_state, State = #vertex_state{}) ->
    io:format("~p~n", [State]),
    {noreply, State};
handle_info(_Info, State = #vertex_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #vertex_state{}) -> term()).
terminate(_Reason, _State = #vertex_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #vertex_state{},
    Extra :: term()) ->
    {ok, NewState :: #vertex_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #vertex_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
