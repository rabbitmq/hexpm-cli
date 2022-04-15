-module('hexpm-cli').

-export([main/1]).

%% -------------------------------------------------------------------
%% Main command dispatcher (entry point)
%% -------------------------------------------------------------------

main([]) ->
    usage(standard_io),
    run_rebar(["help", "hex"]);
main([VersionFlag | _])
  when VersionFlag =:= "-v" orelse VersionFlag =:= "--version" ->
    version(standard_io),
    run_rebar(["-v"]);
main([HelpFlag | _])
  when HelpFlag =:= "-h" orelse HelpFlag =:= "--help" ->
    usage(standard_io),
    run_rebar(["help", "hex"]);
main(["help" | Args]) ->
    run_rebar(["help", "hex" | Args]);
main(["publish" | _] = Args) ->
    run_rebar(["hex" | Args]);
main(Args) ->
    run_rebar(["hex" | Args]).

%% -------------------------------------------------------------------
%% Internal functions.
%% -------------------------------------------------------------------

run_rebar(Args) ->
    application:set_env([{rebar, [
        %% Default log level
        {log_level, warn},

        {resources, [{git, rebar_git_resource},
                     {git_subdir, rebar_git_subdir_resource},
                     {pkg, rebar_pkg_resource},
                     {hg, rebar_hg_resource}]},

        {compilers, [rebar_compiler_xrl, rebar_compiler_yrl,
                     rebar_compiler_mib, rebar_compiler_erl]},

        {providers, [rebar_prv_app_discovery,
                     rebar_prv_as,
                     rebar_prv_bare_compile,
                     rebar_prv_clean,
                     rebar_prv_common_test,
                     rebar_prv_compile,
                     rebar_prv_cover,
                     rebar_prv_deps,
                     rebar_prv_deps_tree,
                     rebar_prv_dialyzer,
                     rebar_prv_do,
                     rebar_prv_edoc,
                     rebar_prv_escriptize,
                     rebar_prv_eunit,
                     rebar_prv_get_deps,
                     rebar_prv_help,
                     rebar_prv_install_deps,
                     rebar_prv_local_install,
                     rebar_prv_local_upgrade,
                     rebar_prv_lock,
                     rebar_prv_new,
                     rebar_prv_packages,
                     rebar_prv_path,
                     rebar_prv_plugins,
                     rebar_prv_plugins_upgrade,
                     rebar_prv_release,
                     rebar_prv_relup,
                     rebar_prv_report,
                     rebar_prv_repos,
                     rebar_prv_shell,
                     rebar_prv_state,
                     rebar_prv_tar,
                     rebar_prv_unlock,
                     rebar_prv_update,
                     rebar_prv_upgrade,
                     rebar_prv_version,
                     rebar_prv_xref,
                     rebar_prv_alias]} % must run last to prevent overloads
        ]}]),
    rebar3:main(Args).

usage(IO) ->
    io:format(IO, "Usage: ~s <task>~n", [progname()]).

version(IO) ->
    application:load('hexpm-cli'),
    {ok, Version} = application:get_key('hexpm-cli', vsn),
    io:format(IO, "~s ~s~n", [progname(), Version]).

progname() ->
    filename:basename(escript:script_name()).
