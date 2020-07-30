-module(hexpm_cli_test).

-include_lib("eunit/include/eunit.hrl").

-define(USAGE_PREAMBULE,
        [$U,$s,$a,$g,$e,$:,$\s,$h,$e,$x,$p,$m,$\s,$<,$t,$a,$s,$k,$> | _]).

noarg_test() ->
    ?assertMatch(
       ?USAGE_PREAMBULE,
       ?cmd("./hexpm")).

dash_h_test() ->
    ?assertMatch(
       ?USAGE_PREAMBULE,
       ?cmd("./hexpm -h")).

dash_v_test() ->
    ?assertMatch(
       [$h,$e,$x,$p,$m,$\s | _],
       ?cmd("./hexpm -v")).

help_cmd_test() ->
    ?assertMatch(
       match,
       re:run(
         ?cmd("./hexpm help publish"),
         "^Usage: rebar3 hex publish",
         [{capture, none}, multiline])
      ).
