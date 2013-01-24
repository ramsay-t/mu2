-module(mu2_logger).
-export([start_link/2, log/3, log/4, parse_log/2, log_to_traces/1, log_to_trace_file/2]).

-export([init/1, handle_call/3, handle_cast/2]).

-behaviour(gen_server).

start_link(Name, Filename) ->
    gen_server:start_link({local, Name}, ?MODULE, Filename, []).

init(Filename) ->
    {ok, Filename}.

log_to_file(File, Data) ->
    Msg = io_lib:format("~w.~n", [Data]),
    %% Convert PIDs and Refs to strings...
    MidMsg = re:replace(Msg, "<[^>]*>", "\"&\"", [{return,list},global]),
    ProcMsg = re:replace(MidMsg, "#Ref\"", "\"Ref", [{return,list},global]),
    file:write_file(File, ProcMsg, [append]).

handle_call({F,A}, _From, File) ->
    log_to_file(File, {F,A}),
    {noreply, File};
handle_call(stop, _From, File) ->
    %%close_file(File),
    {stop,normal,File}.

handle_cast({M,F,A}, File) ->
    log_to_file(File, {M,F,A}),
    {noreply, File};
handle_cast(stop, File) ->
    %%close_file(File),
    {stop,normal,File}.

make_name(LogID) ->
    list_to_atom(atom_to_list(LogID) ++ "_logger").

log(M, F, A) ->
    log(M, F, A, M).

log(M, F, A, LogID) ->
    Name = make_name(LogID),
    case whereis(Name) of
	undefined ->
	    Filename = atom_to_list(LogID) ++ ".log",
	    start_link(Name, Filename);
	_ ->
	    ok
    end,
    gen_server:cast(Name, {M,F,A}).

parse_log(File, Module) ->
    {ok, Content} = file:consult(File),
    split_for_module(Module, Content).

split_for_module(_Module, []) ->
    [[]];
split_for_module(Module, [{Module, F, A}| More]) ->
    Sub = split_for_module(Module, More),
    [[{Module,F,A} | hd(Sub)] | tl(Sub)];
split_for_module(Module, [{_NotModule, _F, _A} | More]) ->
    Sub = split_for_module(Module, More),
    case hd(Sub) of
	[] ->
	    Sub;
	_List ->
	    [[] | Sub]	
    end.

log_to_trace_file([], _File) ->
    ok;
log_to_trace_file([Trace | Ts], File) ->
    Msg = io_lib:format("+ ~s~n", [trace_to_string(Trace)]),
    file:write_file(File, Msg, [append]),
    log_to_trace_file(Ts, File).

log_to_traces([]) ->
    [];
log_to_traces([Trace | Ts]) ->
    [io_lib:format("+ ~s", [trace_to_string(Trace)]) | log_to_traces(Ts)].

trace_to_string([]) ->
    "";
trace_to_string([{M,F,A} | Ts]) ->
    lists:flatten(io_lib:format("~900000000p ", [{M,F,A}]) ++ trace_to_string(Ts)).

