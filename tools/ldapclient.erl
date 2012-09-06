-module(ldapclient).
-export([connection/2, doRequest/2]).

connection(Host,Port) ->
    ok = asn1ct:compile('LDAP-V3'),
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]),
    Sock.

doRequest(Sock, Request) ->
    {ok,ASNRequest} = 'LDAP-V3':encode('LDAPMessage', Request),
    gen_tcp:send(Sock, ASNRequest),
    {ok, Response} = gen_tcp:revc(Sock, 0),
    'LDAP-V3':decode('LDAPMessage', Response).

