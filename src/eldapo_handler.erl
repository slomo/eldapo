-module(erldap).

-export([start/0]).

% TODO:
%   * improve connection handling (ensure that all sockets are closed)
%   * add ssl support
%   * add support for callback module and proper state handling
%   * implement ldapserach command
%   * implement other ldap commands


start() ->
    ok = asn1ct:compile('LDAP-V3'),
    {ok, LSock} = gen_tcp:listen(1389,
        [binary, {packet, 0}, {active, false}]),
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn( fun() -> do_recv(Sock, <<>>, {0}),ok = gen_tcp:close(Sock) end),
    ok = gen_tcp:close(LSock).

do_recv(Sock, Recved, State) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Recv} ->
            Data = << Recved/binary, Recv/binary >>,

            case 'LDAP-V3':decode('LDAPMessage',Data) of
                {ok, Packet} ->
                    io:fwrite("Got Packet: ~p ~n",[Packet]),
                    {ok,Response, NewState} = packetHandler(Packet, State),
                    {ok,ASNResponse} = 'LDAP-V3':encode('LDAPMessage', Response),
                    io:fwrite("Respond with: ~p ~nASN1 encode: ~p ~n",[Response,ASNResponse]),
                    gen_tcp:send(Sock, ASNResponse),
                    do_recv(Sock, <<>>, NewState);
                {error, Reason} ->
                    io:fwrite("Couldn't decode: ~p ~n",[Reason]),
                    do_recv(Sock, Data, State)
            end;
        {error, closed} ->
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
                                asn1_NOVALUE,   % mathedDn ???
                                asn1_NOVALUE,    % Error message ???
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
                                asn1_NOVALUE,   % mathedDn ???
                                asn1_NOVALUE,   % mathedDn ???
                                %"BindDN or Password was wrong",
                                asn1_NOVALUE,   % regerral (optional)
                                asn1_NOVALUE    % serverSaslCreds
                            }
                        }
                    }
            end,
            {ok, Response, NewState};
        {Other, _UnprocessedContent} ->
            io:fwrite("Unable to process ~p protocolOp",[Other])
    end.




bindHandler(BindDN,Password) ->
    case {BindDN, Password} of
        { _Any, 1234 } ->
            { authorized, {BindDN} };
        { "cn=root,o=test", 12345 } ->
            { authorized, {BindDN} };
        _ ->
            unauthorized
    end.



ldapDNParser(PathString) ->
    PathString.
