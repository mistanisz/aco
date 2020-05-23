%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2020
%%%-------------------------------------------------------------------
-module(master).
-author("Michal Stanisz").

-behaviour(gen_server).

%% API
-export([start_link/0, create_ants/1, create_vertex/1, add_edge/3, create_mrowka/2]).
-export([demo/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(master_state, {
    vertices = #{} :: #{atom() => pid()},
    ants = #{} :: #{atom() => pid()}
}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

create_ants(Num) ->
    gen_server:cast(?SERVER, {create_ants, Num}).

create_vertex(Id) ->
    gen_server:cast(?SERVER, {create_vertex, Id}).

add_edge(V1, V2, Weight) ->
    gen_server:cast(?SERVER, {add_edge, V1, V2, Weight}).

create_mrowka(Id, Module) ->
    gen_server:cast(?SERVER, {create_mrowka, Id, Module}).

start_all() ->
    gen_server:cast(?SERVER, {start_all}).

demo() ->
    start_link(),
    create_vertex(v1),
    create_vertex(v2),
    create_vertex(v3),
    add_edge(v1, v2, 5),
    add_edge(v1, v3, 8),
    add_edge(v2, v3, 2),
    
    create_mrowka(m1, random_mrowka),
    create_mrowka(m2, random_mrowka),
    create_mrowka(m3, random_mrowka),
    create_mrowka(m4, random_mrowka),
    start_all(),
    ok.
    

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
    {ok, State :: #master_state{}} | {ok, State :: #master_state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([]) ->
    {ok, #master_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #master_state{}) ->
    {reply, Reply :: term(), NewState :: #master_state{}} |
    {reply, Reply :: term(), NewState :: #master_state{}, timeout() | hibernate} |
    {noreply, NewState :: #master_state{}} |
    {noreply, NewState :: #master_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #master_state{}} |
    {stop, Reason :: term(), NewState :: #master_state{}}).
handle_call(_Request, _From, State = #master_state{}) ->
    {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #master_state{}) ->
    {noreply, NewState :: #master_state{}} |
    {noreply, NewState :: #master_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #master_state{}}).
handle_cast({create_vertex, Id}, State = #master_state{vertices = V}) ->
    Pid = vertex:start_link(Id),
    {noreply, State#master_state{vertices = V#{Id => Pid}}};
handle_cast({create_mrowka, Id, Module}, State = #master_state{vertices = V, ants = A}) ->
    [S | _ ] = maps:keys(V),
    Pid = mrowka:start_link(Id, S, Module),
    {noreply, State#master_state{ants = A#{Id => Pid}}};
handle_cast({add_edge, V1, V2, W}, State = #master_state{}) ->
    vertex:add_neighbour(V2, V1, W),
    vertex:add_neighbour(V1, V2, W),
    {noreply, State};
handle_cast({start_all}, State = #master_state{ants = A}) ->
    lists:foreach(fun mrowka:start/1, maps:keys(A)),
    {noreply, State};
handle_cast(_Request, State = #master_state{}) ->
    {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #master_state{}) ->
    {noreply, NewState :: #master_state{}} |
    {noreply, NewState :: #master_state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #master_state{}}).
handle_info(_Info, State = #master_state{}) ->
    {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #master_state{}) -> term()).
terminate(_Reason, _State = #master_state{}) ->
    ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #master_state{},
    Extra :: term()) ->
    {ok, NewState :: #master_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #master_state{}, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
