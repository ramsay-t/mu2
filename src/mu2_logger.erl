-module(mu2_logger).
-export([start_link/2, log/5, log/6, parse_log/2, log_to_traces/1, log_to_trace_file/2, logfile_to_tracefile/1, logfile_to_statechum/1, logfile_to_efsmfile/1]).

-export([init/1, handle_call/3, handle_cast/2]).

-behaviour(gen_server).

start_link(Name, Filename) ->
    gen_server:start_link({local, Name}, ?MODULE, Filename, []).

init(Filename) ->
    {ok, Filename}.

log_to_file(File, Data) ->
    Msg = io_lib:format("~p.~n", [Data]),
    %% Convert PIDs and Refs to strings...
    %%MidMsg = re:replace(Msg, "<[^>]*>", "\"&\"", [{return,list},global]),
    MidMsg = re:replace(Msg, "<[^>]*>", "\"<PID>\"", [{return,list},global]),
    ProcMsg = re:replace(MidMsg, "#Ref\"", "\"Ref", [{return,list},global]),
    ProcProcMsg = re:replace(ProcMsg, "#Fun\"", "\"Fun", [{return,list},global]),
    file:write_file(File, ProcProcMsg, [append]).

handle_call({F,A,Result,Ns}, _From, File) ->
    FName = lists:flatten(io_lib:format("~p/~p", [F, length(A)])),
    log_to_file(File, {FName,A,Result,Ns}),
    {noreply, File};
handle_call(stop, _From, File) ->
    %%close_file(File),
    {stop,normal,File}.

handle_cast({M,F,A,Result,Ns}, File) ->
    FName = lists:flatten(io_lib:format("~p/~p", [F, length(A)])),
    log_to_file(File, {M,FName,A,Result,Ns}),
    {noreply, File};
handle_cast(stop, File) ->
    %%close_file(File),
    {stop,normal,File}.

make_name(LogID) ->
    list_to_atom(atom_to_list(LogID) ++ "_logger").

log(M, F, A, Result, Ns) ->
    log(M, F, A, Ns, Result, M).

log(M, F, A, Ns, Result, LogID) ->
    Name = make_name(LogID),
    case whereis(Name) of
	undefined ->
	    Filename = atom_to_list(LogID) ++ ".log",
	    start_link(Name, Filename);
	_ ->
	    ok
    end,
    NPairs = case Ns of 
		 [[]] ->
		     [{"Result", get_type(Result)}];
		 _ ->
		     lists:zip(Ns ++ ["Result"],lists:map(fun(Arg) -> get_type(Arg) end, A ++ [Result]))
	     end,
    gen_server:cast(Name, {M,F,A,Result,NPairs}).

get_type(A) ->
    AString = lists:flatten(io_lib:format("~p", [A])),
    case re:replace(AString, "[0-9.]*", "", [{return,list}]) of
	[] ->
	    "N";
	_ ->
	    "S"
    end.

parse_log(File, Module) ->
    {ok, Content} = file:consult(File),
    split_for_module(Module, Content).

split_for_module(_Module, []) ->
    [[]];
split_for_module(Module, [{Module, F, A, Result, Ns}| More]) ->
    Sub = split_for_module(Module, More),
    [[{Module,F,A,Result,Ns} | hd(Sub)] | tl(Sub)];
split_for_module(Module, [{_NotModule, _F, _A, _Result, _Ns} | More]) ->
    Sub = split_for_module(Module, More),
    case hd(Sub) of
	[] ->
	    Sub;
	_List ->
	    [[] | Sub]	
    end.

logfile_to_tracefile([FileNameAtom, Module]) ->
    FileName = atom_to_list(FileNameAtom),
    TraceName = re:replace(FileName, ".log", ".traces", [{return,list}]),
    Log = parse_log(FileName, Module),
    log_to_trace_file(Log, TraceName).

logfile_to_statechum([FileNameAtom, Module]) ->
    FileName = atom_to_list(FileNameAtom),
    TraceName = re:replace(FileName, ".log", ".traces", [{return,list}]),
    Log = parse_log(FileName, Module),
    log_to_statechumfile(Log, TraceName).

logfile_to_efsmfile([FileNameAtom, Module]) ->
    FileName = atom_to_list(FileNameAtom),
    TraceName = re:replace(FileName, ".log", ".traces", [{return,list}]),
    Log = parse_log(FileName, Module),
    Types = get_efsm_names(Log),
    Msg = io_lib:format("types~n~s~n", [Types]),
    file:write_file(TraceName, Msg, [append]),
    log_to_efsmfile(Log, TraceName).
    
log_to_trace_file([], _File) ->
    ok;
log_to_trace_file([Trace | Ts], File) ->
    Rev = init:get_argument(rev),
    T = if Rev == error ->
		Trace;
	   true ->
		lists:reverse(Trace)
	end,
    Msg = io_lib:format("+ ~s~n", [trace_to_string(T)]),
    file:write_file(File, Msg, [append]),
    log_to_trace_file(Ts, File).

log_to_statechumfile([], _File) ->
    ok;
log_to_statechumfile([[] | Ts], File) ->
    log_to_statechumfile(Ts, File);
log_to_statechumfile([Trace |Ts], File) ->
    Rev = init:get_argument(rev),
    T = if Rev == error ->
		Trace;
	   true ->
		lists:reverse(Trace)
	end,
    Msg = io_lib:format("+[[~s]]~n", [trace_to_statechum_string(T)]),
    file:write_file(File, Msg, [append]),
    log_to_statechumfile(Ts, File).

log_to_efsmfile([], _File) ->
    ok;
log_to_efsmfile([Trace | Ts], File) ->
    Rev = init:get_argument(rev),
    T = if Rev == error ->
		Trace;
	   true ->
		lists:reverse(Trace)
	end,
    Msg = io_lib:format("trace~n~s~n", [trace_to_efsm(T)]),
    file:write_file(File, Msg, [append]),
    log_to_efsmfile(Ts, File).

strip_handlers(F,A) ->
    Fs = if is_list(F) ->
		 F;
	    true ->
		 io_lib:format("~p",[F])
	 end,
    case string:substr(Fs,1,6) of
	"handle" ->
	    %% This is a handler, so lets use the first param as the event
	    Event = lists:flatten(io_lib:format("~s_~p",[Fs,hd(A)])),
	    {Event,tl(A)};
	_ ->
	    {Fs,A}
    end.


log_to_traces([]) ->
    [];
log_to_traces([Trace | Ts]) ->
    [io_lib:format("+ ~s", [trace_to_string(Trace)]) | log_to_traces(Ts)].

trace_to_efsm([]) ->
    "";
trace_to_efsm([{_M, F, A, Result, _Names} | Ts]) ->
    {Fs,As} = strip_handlers(F,A),
    lists:flatten(io_lib:format("~p ~s~n", [Fs, make_efsm_args(As ++ [Result])]) ++ trace_to_efsm(Ts)).

make_efsm_args([]) ->
    "";
make_efsm_args([A | As]) ->
    lists:flatten(io_lib:format("~900000000p ", [A]) ++ make_efsm_args(As)).

trace_to_string([]) ->
    "";
trace_to_string([{M,F,A,Result,_Names} | Ts]) ->
    lists:flatten(io_lib:format("~900000000p ", [{M,F,A,Result}]) ++ trace_to_string(Ts)).

trace_to_statechum_string([]) ->
    "";
trace_to_statechum_string([{M,F,A,Result,_Names} | Ts]) ->
    lists:flatten(io_lib:format("~900000000p,", [{M,F,A,Result}]) ++ trace_to_statechum_string(Ts)).

get_efsm_names(Traces) ->
    TypeSet = lists:foldl(fun({_M,F,A,_Result,Ns}, S) -> 
				  {Fs,As} = strip_handlers(F,A),
				  TString = if length(As) == length(A) ->
						    io_lib:format("~s", [make_type_string(Ns)]);
					       true ->
						    io_lib:format("~s", [make_type_string(tl(Ns))])
					    end,
				  case lists:keyfind(Fs,1,S) of
				      false ->
					  [{Fs, TString} | S];
				      _ ->
					  S
				  end
			  end, [], lists:flatten(Traces)),
    lists:flatten(lists:map(fun({F,Ts}) -> io_lib:format("~p ~s~n", [F,Ts]) end, TypeSet)).

make_type_string([]) ->
    "";
make_type_string([{N,T} | Ns]) ->
    lists:flatten(io_lib:format("~p:~s ", [N,T])) ++ make_type_string(Ns).
