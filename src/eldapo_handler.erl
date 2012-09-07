-module(eldapo_handler).

-behaviour(gen_listener_tcp).

-export([init/1,
        handle_accept/2,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

%% Implementation of asn1 ber message handling

% TODO:
%   * improve connection handling (ensure that all sockets are closed)
%   * add ssl support
%   * add support for callback module and proper state handling
%   * implement ldapserach command
%   * implement other ldap commands


do_recv(Sock, Recved, State) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Recv} ->
            Data = << Recved/binary, Recv/binary >>,

            case 'LDAP-V3':decode('LDAPMessage',Data) of
                {ok, Packet} ->
                    io:fwrite("Got Packet: ~p ~n from ~p ~n",[Packet, Data]),
                    {ok,Response, NewState} = packetHandler(Packet, State),
                    io:fwrite("Resp: ~p ~n",[Response]),
                    {ok,ASNResponse} = 'LDAP-V3':encode('LDAPMessage', Response),
                    io:fwrite("Respond with: ~p ~nASN1 encode: ~p ~n",[Response,ASNResponse]),
                    gen_tcp:send(Sock, ASNResponse),
                    do_recv(Sock, <<>>, NewState);
                {error, Reason} ->
                    io:fwrite("Couldn't decode: ~p ~n",[Reason]),
                    do_recv(Sock, Data, State)
            end;
        {error, closed} ->
            gen_tcp:close(Sock),
            ok
    end.



packetHandler({'LDAPMessage', MessageID, Content, Options } ,{ MessageCounter } ) ->

    % FIXME: this is a hack! incremention can not be assumed.
    MessageID = MessageCounter + 1,
    NewState = { MessageCounter + 1},

    if
        Options =/= asn1_NOVALUE ->
            io:fwrite("Reveived Options: ~p . Ignoring those",[Options]);
        true ->
            skip
    end,

    case Content of
        {bindRequest, {'BindRequest', ProtocolVersion, BindDN, Auth }} ->

            % FIXME: only support ldapV3
            ProtocolVersion = 3,

            % FIXME: only suport simple auth
            { simple, Password } = Auth,

            % call bind handler
            Response  = case bindHandler(ldapDNParser(BindDN), Password) of
                % FIXME: handle state
                { authorized, _ThrowAwayState } ->
                    {'LDAPMessage',
                        MessageID,
                        { bindResponse,
                            { 'BindResponse',
                                success,        % result code
                                "",             % mathedDn, must be string
                                "",             % Error message, must be string
                                asn1_NOVALUE,   % regerral (optional)
                                asn1_NOVALUE    % serverSaslCreds
                            }
                        },
                        asn1_NOVALUE   % Options
                    };
                unauthorized ->
                    {'LDAPMessage',
                        MessageID,
                        { bindResponse,
                            { 'BindResponse',
                                invalidCredentials,
                                "",             % mathedDn ???
                                "",
                                asn1_NOVALUE,   % regerral (optional)
                                asn1_NOVALUE    % serverSaslCreds
                            }
                        },
                        asn1_NOVALUE   % Options
                    }
            end,
            {ok, Response, NewState};
        {unbindRequest, 'NULL' } ->
            ok = unbindHanlder();
        {Other, UnprocessedContent} ->
            io:fwrite("Unable to process ~p protocolOp: ~p",[Other, UnprocessedContent])
    end.



% FIXME: move to ldap_handler (client code)

bindHandler(BindDN,Password) ->
    io:fwrite("Tries to auth ~p ~p ~n",[BindDN,Password]),
    case {BindDN, Password} of
        { _Any, "1234" } ->
            { authorized, {BindDN} };
        { "cn=root,o=test", "123" } ->
            { authorized, {BindDN} };
        _ ->
            unauthorized
    end.

unbindHanlder() ->
    ok.

% FIXME: move to libary

ldapDNParser(PathString) ->
    PathString.

%% behaviour gen_listner_tcp

-define(TCP_PORT, 1389).
-define(TCP_OPTS, [binary, inet,
        {active,    false},
        {backlog,   10},
        {nodelay,   true},
        {packet,    raw},
        {reuseaddr, true}]).


init([]) ->
    {ok, {?TCP_PORT, ?TCP_OPTS}, nil}.


handle_accept(Sock, State) ->
    % TODO: get initial state here
    Pid = spawn(fun() -> do_recv(Sock, <<>> , {0}) end),
    gen_tcp:controlling_process(Sock, Pid),
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


