-module('hexpm-cli').

-export([main/1]).

%% -------------------------------------------------------------------
%% Main command dispatcher (entry point)
%% -------------------------------------------------------------------

main([]) ->
    usage(standard_io),
    run_rebar(["help", "hex"]);
main(["-v" | _]) ->
    usage(standard_io);
main(["-h" | _]) ->
    usage(standard_io),
    run_rebar(["help", "hex"]);
main(["help" | Args]) ->
    run_rebar(["help", "hex" | Args]);
main(Args) ->
    run_rebar(["hex" | Args]).

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

run_rebar(Args) ->
    rebar3:main(Args).

usage(IO) ->
    io:format(IO, "Usage: ~s <task>~n", [progname()]).

progname() ->
    filename:basename(escript:script_name()).
