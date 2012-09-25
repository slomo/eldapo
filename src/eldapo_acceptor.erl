-module(eldapo_acceptor).

-export([start_link/0]).

-behaviour(gen_listener_tcp).

-export([init/1,
        handle_accept/2,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).


%% behaviour gen_listner_tcp

-define(TCP_DEFAULT_PORT, 1389).
-define(TCP_OPTS, [binary, inet,
        {active,    false},
        {backlog,   10},
        {nodelay,   true},
        {packet,    raw},
        {reuseaddr, true}]).

start_link() ->
    gen_listener_tcp:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
    case application:get_env(eldapo, listen_port) of
        undefined ->
            TcpPort = ?TCP_DEFAULT_PORT;
        {ok, Port } ->
            TcpPort = Port
    end,
    {ok, {TcpPort, ?TCP_OPTS}, nil}.


handle_accept(Sock, State) ->
    % TODO: get initial state here
    {ok,Pid} = eldapo_handler_sup:start_child(),
    eldapo_hanlder:inject_socket(Sock, Pid),
    {noreply, State}.

handle_call(Request, _From, State) ->
    {reply, {illegal_request, Request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


