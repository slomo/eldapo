
-module(eldapo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
% -define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================


init([]) ->
    {ok, { {one_for_one, 5, 10},
            [
                % first child tcp listner
                { eldapo_tcp_acceptor,
                    {eldapo_tcp_acceptor, start_link, []},
                    permanent, brutal_kill, worker, []},
                % second child eldapo handler supervisor
                { eldapo_handler_sup,
                    {eldapo_handler_sup,start_link, []},
                    permanent, infinity, supervisor, []}
            ]
        }}.

