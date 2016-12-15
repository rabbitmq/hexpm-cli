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
       [$U,$s,$a,$g,$e,$:,$\s,
        $r,$e,$b,$a,$r,$3,$\s,$h,$e,$x,$\s,$p,$u,$b,$l,$i,$s,$h | _],
       ?cmd("./hexpm help publish")).

info_test() ->
    ?assertMatch(
       [$r,$a,$b,$b,$i,$t,$_,$c,$o,$m,$m,$o,$n | _],
       ?cmd("./hexpm info rabbit_common")).
