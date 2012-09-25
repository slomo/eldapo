-module(eldapo_handler).

-export([start_link/0, inject_socket/2]).

-behaviour(gen_fsm).

-export([init/1,
        waitingForSocket/2,
        recvData/2,
        handle_info/3,
        terminate/3,
        code_change/4]).

%% Implementation of asn1 ber message handling

% TODO:
%   * improve connection handling (ensure that all sockets are closed)
%   * add ssl support
%   * add support for callback module and proper state handling
%   * implement ldapserach command
%   * implement other ldap commands


start_link() ->
    gen_fsm:start_link({local, ?MODULE}, [], []).

inject_socket(Sock, Pid) ->
    gen_tcp:controlling_process(Sock, Pid),
    gen_fsm:send_event({socket, Sock}).

% states of fsm

-record(state,{
        socket,
        has_session = false,
        callback = ?MODULE,
        callback_state,
        addr,
        data_received,
        message_counter = 0
    }).

init() ->
    init([?MODULE]).

init([Callback_Module]) ->
    % FIXME: get init state
    {ok, waitingForSocket, #state{}}.

waitingForSocket({socket, Socket},State) ->
    inet:setopts(Socket, [{active, once}, {packet, 2}, binary]),
    {ok, {IP, _Port}} = inet:peername(Socket),
    { next_state, recvData, State#state{socket = Socket, addr=IP}}.

recvData({data, NewData, Socket}, State=#state{data_received = Recvd, socket=Socket}) ->
    Data = << NewData/binary , Recvd/binary >>,
    case erlang:decode_packet(asn1, Data, []) of
        {ok, Packet, Rest} ->
            case erlang:decode_packet(Packet, State) of
                { reply, Reply, NewState } ->
                    gen_tcp:send(Reply),
                    { nextState, recvData, NewState#state{data_received = Rest }};
                { stop, Reason, _FinalState } ->
                    gen_tcp:close(Socket),
                    { stop, Reason }
            end;
        {more, _Length} ->
            { nextState, recvData, State#state{data_received = Data} };
        {error, _Reason} ->
            gen_tcp:close(Socket)
    end.


process_packet(Data, State) ->
    case 'LDAP-V3':decode('LDAPMessage',Data) of
        {ok, Packet} ->
            % TODO remove io:writes
            io:fwrite("Got Packet: ~p ~n from ~p ~n", [Packet, Data]),

            case handle_packet(Packet, State) of
                {ok, Response, NewState} ->
                    io:fwrite("Resp: ~p ~n", [Response]),
                    {ok,ASNResponse} = 'LDAP-V3':encode('LDAPMessage', Response),
                    { reply, ASNResponse, State};
                Resp ->
                    io:fwrite("Resp: ~p ~n, there for terminate"),
                    { stop, error, State }
            end;
    {error, Reason} ->
        error_logger:info_msg("Unable to decode packet, there fore shutting down! ~n Undecodeable packet: ~p ~n",[Reason]),
        { stop, Reason, State }
    end.


handle_packet({'LDAPMessage', MessageID, Content = { MessageType, _ } , Options } , State = #state{has_session = HasSession, message_counter = MessageCounter } ) ->

    %% check weather package schould be handled
    if
        MessageID < MessageCounter ->
            { error, wrongMessageCounter };
        MessageType =/= bindRequest and not HasSession ->
            { error, noSession };
        Options =/= asn1_NOVALUE ->
            { error, optionNotSupported };
        true ->

            case Content of
                {bindRequest, {'BindRequest', ProtocolVersion, BindDN, Auth }} ->

                    % FIXME: only support ldapV3
                    ProtocolVersion = 3,

                    % FIXME: only suport simple auth
                    { simple, Password } = Auth,

                    % call bind handler
                    case bindHandler(eldpo_proto:parse_dn(BindDN), Password, State#state.callback_state) of
                        % FIXME: handle state
                        { authorized, ModuleState } ->
                            { reply, eldapo_proto:bind_success(MessageID),
                                State#state{ callback_state = ModuleState, message_counter = MessageID + 1, has_session = true }};
                        { unauthorized, _ModuleState} ->
                            { reply, eldapo_proto:bind_failed(MessageID), State}
                    end;
                {unbindRequest, 'NULL' } ->
                    { ok, _ } = unbindHanlder(),
                    { stop, normal, State };
                {Other, UnprocessedContent} ->
                    io:fwrite("Unable to process ~p protocolOp: ~p",[Other, UnprocessedContent])
            end
    end.



% FIXME: move to ldap_handler (client code)

bindHandler(BindDN, Password, State) ->
    io:fwrite("Tries to auth ~p ~p ~n",[BindDN,Password]),
    case {BindDN, Password} of
        { _Any, "1234" } ->
            { authorized, {BindDN}, State };
        { "cn=root,o=test", "123" } ->
            { authorized, {BindDN}, State };
        _ ->
            { unauthorized, State}
    end.

unbindHanlder() ->
    ok.

% None fsm specific callbacks

handle_info({tcp, Socket, Bin}, StateName, State) ->
    inet:setopts(Socket, [{active, once}]),
    ?MODULE:StateName({data, Bin}, State);
handle_info({tcp_closed, Socket}, _StateName,
    #state{socket=Socket, addr=Addr} = StateData) ->
    error_logger:info_msg("~p Client ~p disconnected.\n", [self(), Addr]),
    {stop, normal, StateData};
handle_info(_Info, StateName, StateData) ->
    {noreply, StateName, StateData}.


terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.


