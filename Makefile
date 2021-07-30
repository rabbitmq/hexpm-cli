PROJECT = hexpm-cli
PROJECT_VERSION = 0.8.0
PROJECT_DESCRIPTION = CLI on top of rebar3_hex plugin
define PROJECT_ENV
[
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
        ]
endef

DEPS = rebar
dep_rebar_commit = 3.16.1

ESCRIPT_FILE = hexpm

define PROJECT_APP_EXTRA_KEYS
%% Hex.pm package informations.
	{maintainers, ["RabbitMQ <info@rabbitmq.com>"]},
	{licenses, ["BSD 2-Clause"]},
	{links, [
	    {"GitHub", "https://github.com/rabbitmq/"}
	  ]},
	%% TODO: Fill this automatically using $$(MAKEFILES) - $$those_in_$$(DEPS_DIR)
	{include_files, [
	    "Makefile",
	    "erlang.mk"
	  ]}
endef

# --------------------------------------------------------------------
# ERLC_FLAGS for rebar.
# --------------------------------------------------------------------

define compare_version
$(shell awk 'BEGIN {
	split("$(1)", v1, ".");
	version1 = v1[1] * 1000000 + v1[2] * 10000 + v1[3] * 100 + v1[4];

	split("$(2)", v2, ".");
	version2 = v2[1] * 1000000 + v2[2] * 10000 + v2[3] * 100 + v2[4];

	if (version1 $(3) version2) {
		print "true";
	} else {
		print "false";
	}
}')
endef

# Erlang R16B03 has no support for new types in Erlang 17.0, leading to
# a build-time error.
ERTS_VER := $(shell erl -version 2>&1 | sed -E 's/.* version //')
old_builtin_types_MAX_ERTS_VER = 6.0
ifeq ($(call compare_version,$(ERTS_VER),$(old_builtin_types_MAX_ERTS_VER),<),false)
ERLC_OPTS += -Dnamespaced_types
TEST_ERLC_OPTS += -Dnamespaced_types
endif

include erlang.mk

clean:: clean-rebar-build-dir

.PHONY: clean-rebar-build-dir
clean-rebar-build-dir:
	$(gen_verbose) rm -rf _build

# --------------------------------------------------------------------
# Tweaks to the escript zip.
# --------------------------------------------------------------------

# Remove rebar modules we duplicated so ours are picked.

DUPLICATED_MODS = $(patsubst src/%.erl,rebar/ebin/%.beam,$(wildcard src/rebar*.erl))

escript-zip::
	$(verbose) 7z d $(ESCRIPT_ZIP_FILE) $(DUPLICATED_MODS) \
		$(if $(filter-out 0,$(V)),,> /dev/null)
