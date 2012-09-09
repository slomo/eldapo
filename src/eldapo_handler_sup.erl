-module(eldapo_handler_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/0]).

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
            [] %  add childs here
        }}.


start_child() ->
    supervisor:start_child(?MODULE,
        {
            make_ref(),
            { eldapo_handler, start_link, [] },
            temporary,
            brutal_kill,
            worker,
            [ eldapo_hanlde ]
        }
    ).
