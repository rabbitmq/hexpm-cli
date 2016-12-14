-module(rebar_dir).

-export([base_dir/1,
         profile_dir/2,
         deps_dir/1,
         deps_dir/2,
         root_dir/1,
         checkouts_dir/1,
         checkouts_dir/2,
         plugins_dir/1,
         lib_dirs/1,
         home_dir/0,
         global_config_dir/1,
         global_config/1,
         global_config/0,
         global_cache_dir/1,
         local_cache_dir/1,
         get_cwd/0,
         template_globals/1,
         template_dir/1,
         processing_base_dir/1,
         processing_base_dir/2,
         make_relative_path/2,
         src_dirs/1, src_dirs/2,
         src_dir_opts/2, recursive/2,
         extra_src_dirs/1, extra_src_dirs/2,
         all_src_dirs/1, all_src_dirs/3,
         retarget_path/2]).

-include_lib("rebar/src/rebar.hrl").

-spec base_dir(rebar_state:t()) -> file:filename_all().
base_dir(State) ->
    profile_dir(rebar_state:opts(State), rebar_state:current_profiles(State)).

-spec profile_dir(rebar_dict(), [atom()]) -> file:filename_all().
profile_dir(Opts, Profiles) ->
    {BaseDir, ProfilesStrings} = case [ec_cnv:to_list(P) || P <- Profiles] of
        ["global" | _] -> {?MODULE:global_cache_dir(Opts), [""]};
        ["bootstrap", "default"] -> {rebar_opts:get(Opts, base_dir, ?DEFAULT_BASE_DIR), ["default"]};
        ["default"] -> {rebar_opts:get(Opts, base_dir, ?DEFAULT_BASE_DIR), ["default"]};
        %% drop `default` from the profile dir if it's implicit and reverse order
        %%  of profiles to match order passed to `as`
        ["default"|Rest] -> {rebar_opts:get(Opts, base_dir, ?DEFAULT_BASE_DIR), Rest}
    end,
    ProfilesDir = string:join(ProfilesStrings, "+"),
    filename:join(BaseDir, ProfilesDir).

-spec deps_dir(rebar_state:t()) -> file:filename_all().
deps_dir(State) ->
    filename:join(base_dir(State), rebar_state:get(State, deps_dir, ?DEFAULT_DEPS_DIR)).

-spec deps_dir(file:filename_all(), file:filename_all()) -> file:filename_all().
deps_dir(DepsDir, App) ->
    filename:join(DepsDir, App).

root_dir(State) ->
    filename:absname(rebar_state:get(State, root_dir, ?DEFAULT_ROOT_DIR)).

-spec checkouts_dir(rebar_state:t()) -> file:filename_all().
checkouts_dir(State) ->
    filename:join(root_dir(State), rebar_state:get(State, checkouts_dir, ?DEFAULT_CHECKOUTS_DIR)).

-spec checkouts_dir(rebar_state:t(), file:filename_all()) -> file:filename_all().
checkouts_dir(State, App) ->
    filename:join(checkouts_dir(State), App).

-spec plugins_dir(rebar_state:t()) -> file:filename_all().
plugins_dir(State) ->
    case lists:member(global, rebar_state:current_profiles(State)) of
        true ->
            filename:join([base_dir(State), global_config_dir(State), rebar_state:get(State, plugins_dir, ?DEFAULT_PLUGINS_DIR)]);
        false ->
            filename:join(base_dir(State), rebar_state:get(State, plugins_dir, ?DEFAULT_PLUGINS_DIR))
    end.

-spec lib_dirs(rebar_state:t()) -> file:filename_all().
lib_dirs(State) ->
    rebar_state:get(State, project_app_dirs, ?DEFAULT_PROJECT_APP_DIRS).

home_dir() ->
    {ok, [[Home]]} = init:get_argument(home),
    Home.

global_config_dir(State) ->
    Home = home_dir(),
    rebar_state:get(State, global_rebar_dir, filename:join([Home, ".config", "rebar3"])).

global_config(State) ->
    filename:join(global_config_dir(State), "rebar.config").

global_config() ->
    %% hexpm-cli: We don't want hexpm-cli to pick the global
    %% configuration; a built-in configuration is returned by
    %% rebar_file_utils:try_consult/1. However, the file returned
    %% by rebar_dir:global_config/0 must exists on disk so
    %% rebar3:init_config/0 honors that built-in configuration.
    filename:absname(escript:script_name()).

-spec global_cache_dir(rebar_dict()) -> file:filename_all().
global_cache_dir(Opts) ->
    Home = home_dir(),
    rebar_opts:get(Opts, global_rebar_dir, filename:join([Home, ".cache", "rebar3"])).

local_cache_dir(Dir) ->
    filename:join(Dir, ".rebar3").

get_cwd() ->
    {ok, Dir} = file:get_cwd(),
    %% On windows cwd may return capital letter for drive,
    %% for example C:/foobar. But as said in http://www.erlang.org/doc/man/filename.html#join-1
    %% filename:join/1,2 anyway will convert drive-letter to lowercase, so we have to "internalize"
    %% cwd as soon as it possible.
    filename:join([Dir]).

template_globals(State) ->
    filename:join([global_config_dir(State), "templates", "globals"]).

template_dir(State) ->
    filename:join([global_config_dir(State), "templates"]).

processing_base_dir(State) ->
    Cwd = get_cwd(),
    processing_base_dir(State, Cwd).

processing_base_dir(State, Dir) ->
    AbsDir = filename:absname(Dir),
    AbsDir =:= rebar_state:get(State, base_dir).

make_absolute_path(Path) ->
    case filename:pathtype(Path) of
        absolute ->
            Path;
        relative ->
            {ok, Dir} = file:get_cwd(),
            filename:join([Dir, Path]);
        volumerelative ->
            Volume = hd(filename:split(Path)),
            {ok, Dir} = file:get_cwd(Volume),
            filename:join([Dir, Path])
    end.

make_normalized_path(Path) ->
    AbsPath = make_absolute_path(Path),
    Components = filename:split(AbsPath),
    make_normalized_path(Components, []).

make_normalized_path([], NormalizedPath) ->
    filename:join(lists:reverse(NormalizedPath));
make_normalized_path([H|T], NormalizedPath) ->
    case H of
        "."  -> make_normalized_path(T, NormalizedPath);
        ".." -> make_normalized_path(T, tl(NormalizedPath));
        _    -> make_normalized_path(T, [H|NormalizedPath])
    end.

make_relative_path(Source, Target) ->
    AbsSource = make_normalized_path(Source),
    AbsTarget = make_normalized_path(Target),
    do_make_relative_path(filename:split(AbsSource), filename:split(AbsTarget)).

do_make_relative_path([H|T1], [H|T2]) ->
    do_make_relative_path(T1, T2);
do_make_relative_path(Source, Target) ->
    Base = lists:duplicate(max(length(Target) - 1, 0), ".."),
    filename:join(Base ++ Source).

%%%-----------------------------------------------------------------
%%% 'src_dirs' and 'extra_src_dirs' can be configured with options
%%% like this:
%%%
%%% {src_dirs,[{"foo",[{recursive,false}]}]}
%%% {extra_src_dirs,[{"bar",[recursive]}]} (equivalent to {recursive,true})
%%%
%%% src_dirs/1,2 and extra_src_dirs/1,2 return only the list of
%%% directories for the 'src_dirs' and 'extra_src_dirs' options
%%% respectively, while src_dirs_opts/2 return the options list for
%%% the given directory, no matter if it is configured as 'src_dirs' or
%%% 'extra_src_dirs'.
%%%
-spec src_dirs(rebar_dict()) -> list(file:filename_all()).
src_dirs(Opts) -> src_dirs(Opts, []).

-spec src_dirs(rebar_dict(), list(file:filename_all())) -> list(file:filename_all()).
src_dirs(Opts, Default) ->
    src_dirs(src_dirs, Opts, Default).

-spec extra_src_dirs(rebar_dict()) -> list(file:filename_all()).
extra_src_dirs(Opts) -> extra_src_dirs(Opts, []).

-spec extra_src_dirs(rebar_dict(), list(file:filename_all())) -> list(file:filename_all()).
extra_src_dirs(Opts, Default) ->
    src_dirs(extra_src_dirs, Opts, Default).

src_dirs(Type, Opts, Default) ->
    lists:usort([case D0 of {D,_} -> D; _ -> D0 end ||
                    D0 <- raw_src_dirs(Type,Opts,Default)]).

raw_src_dirs(Type, Opts, Default) ->
    ErlOpts = rebar_opts:erl_opts(Opts),
    Vs = proplists:get_all_values(Type, ErlOpts),
    case lists:append([rebar_opts:get(Opts, Type, []) | Vs]) of
        []   -> Default;
        Dirs -> Dirs
    end.

-spec all_src_dirs(rebar_dict()) -> list(file:filename_all()).
all_src_dirs(Opts) -> all_src_dirs(Opts, [], []).

-spec all_src_dirs(rebar_dict(), list(file:filename_all()), list(file:filename_all())) ->
    list(file:filename_all()).
all_src_dirs(Opts, SrcDefault, ExtraDefault) ->
    lists:usort(src_dirs(Opts, SrcDefault) ++ extra_src_dirs(Opts, ExtraDefault)).

%%%-----------------------------------------------------------------
%%% Return the list of options for the given src directory
%%% If the same option is given multiple times for a directory in the
%%% config, the priority order is: first occurence of 'src_dirs'
%%% followed by first occurence of 'extra_src_dirs'.
-spec src_dir_opts(rebar_dict(), file:filename_all()) -> [{atom(),term()}].
src_dir_opts(Opts, Dir) ->
    RawSrcDirs = raw_src_dirs(src_dirs, Opts, []),
    RawExtraSrcDirs = raw_src_dirs(extra_src_dirs, Opts, []),
    AllOpts = [Opt || {D,Opt} <- RawSrcDirs++RawExtraSrcDirs,
                      D==Dir],
    lists:ukeysort(1,proplists:unfold(lists:append(AllOpts))).

%%%-----------------------------------------------------------------
%%% Return the value of the 'recursive' option for the given directory.
%%% If not given, the value of 'recursive' in the 'erlc_compiler'
%%% options is used, and finally the default is 'true'.
-spec recursive(rebar_dict(), file:filename_all()) -> boolean().
recursive(Opts, Dir) ->
    DirOpts = src_dir_opts(Opts, Dir),
    Default = proplists:get_value(recursive,
                                  rebar_opts:get(Opts, erlc_compiler, []),
                                  true),
    R = proplists:get_value(recursive, DirOpts, Default),
    R.

%% given a path if that path is an ancestor of an app dir return the path relative to that
%% apps outdir. if the path is not an ancestor to any app dirs but is an ancestor of the
%% project root return the path relative to the project base_dir. if it is not an ancestor
%% of either return it unmodified
-spec retarget_path(rebar_state:t(), string()) -> string().

retarget_path(State, Path) ->
    ProjectApps = rebar_state:project_apps(State),
    retarget_path(State, Path, ProjectApps).

%% not relative to any apps in project, check to see it's relative to
%% project root
retarget_path(State, Path, []) ->
    case rebar_file_utils:path_from_ancestor(rebar_file_utils:canonical_path(Path), rebar_state:dir(State)) of
        {ok, NewPath}      -> filename:join([base_dir(State), NewPath]);
        %% not relative to project root, don't modify
        {error, badparent} -> Path
    end;
%% relative to current app, retarget to the same dir relative to
%% the app's out_dir
retarget_path(State, Path, [App|Rest]) ->
    case rebar_file_utils:path_from_ancestor(rebar_file_utils:canonical_path(Path), rebar_app_info:dir(App)) of
        {ok, NewPath}      -> filename:join([rebar_app_info:out_dir(App), NewPath]);
        {error, badparent} -> retarget_path(State, Path, Rest)
    end.
