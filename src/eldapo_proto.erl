-module(eldapo_proto).

-export([bind_failed/1, bind_success/1, parse_dn/1]).

base_message(MessageID, Message) ->
    {
        'LDAPMessage',
        MessageID,
        Message,
        asn1_NOVALUE   % Options
    }.

bind_failed(MessageId) ->
    base_message(MessageId,
        { bindResponse,
            { 'BindResponse',
                invalidCredentials,
                "",             % mathedDn ???
                "",
                asn1_NOVALUE,   % regerral (optional)
                asn1_NOVALUE    % serverSaslCreds
            }
        }).


bind_success(MessageId) ->
    base_message(MessageId,{ bindResponse,
        { 'BindResponse',
            success,        % result code
            "",             % mathedDn, must be string
            "",             % Error message, must be string
            asn1_NOVALUE,   % regerral (optional)
            asn1_NOVALUE    % serverSaslCreds
        }
    }).




parse_dn(PathString) ->
    Elements = string:words(PathString,","),
    lists:map(
        fun(Element) ->
                string:strip(Element)
        end, Elements).
