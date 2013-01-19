-module(mu2_logger).
-export([start_link/2, log/3, stop/1]).

-export([init/1, handle_call/3, handle_cast/2]).

-behaviour(gen_server).

start_link(Name, Filename) ->
    gen_server:start_link({local, Name}, ?MODULE, Filename, []).

init(Filename) ->
    {ok, Filename}.

log_to_file(File, {F,A}) ->
    Msg = io_lib:format("~p ~p~n", [F,A]),
    file:write_file(File, Msg, [append]).

close_file(File) ->
    file:close(File),
    ok.

handle_call({F,A}, _From, File) ->
    log_to_file(File, {F,A}),
    {noreply, File};
handle_call(stop, _From, File) ->
    %%close_file(File),
    {stop,normal,File}.

handle_cast({F,A}, File) ->
    log_to_file(File, {F,A}),
    {noreply, File};
handle_cast(stop, File) ->
    %%close_file(File),
    {stop,normal,File}.

make_name(M) ->
    list_to_atom(atom_to_list(M) ++ "_logger").

log(M, F, A) ->
    process_flag(trap_exit, true),
    Name = make_name(M),
    case whereis(Name) of
	undefined ->
	    Filename = atom_to_list(M) ++ ".log",
	    start_link(Name, Filename);
	_ ->
	    ok
    end,
    gen_server:cast(Name, {F,A}).

stop(Module) ->
    gen_server:call(make_name(Module), stop).
