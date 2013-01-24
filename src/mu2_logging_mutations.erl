-module(mu2_logging_mutations).
-export([apply_logging/1, apply_relevant_logging/1]).

-include("../include/mutations.hrl").

log_mutation(File, LogID) ->
    ?FULL_TD_TP([?RULE(?T("f@(Args@@) when Guard@@-> Body@@;"), 
		       begin
			   {NewArgs@@, LogArgs@@, _} = convert_args(Args@@, 1),
			   ?TO_AST("f@(NewArgs@@) when Guard@@-> mu2_logger:log(?MODULE, f@, [LogArgs@@], " ++ LogID ++ "), Body@@;")
		       end,
		       true)], 
		[File]).

specific_log_mutation(File, FunctionNames, LogID) ->
    ?FULL_TD_TP([?RULE(?T("f@(Args@@) when Guard@@-> Body@@;"), 
		       begin
			   {NewArgs@@, LogArgs@@, _} = convert_args(Args@@, 1),
			   ?TO_AST("f@(NewArgs@@) when Guard@@-> mu2_logger:log(?MODULE, f@, [LogArgs@@], " ++ LogID ++ "), Body@@;")
		       end,
		       contains(FunctionNames, {list_to_atom(?PP(f@)), length(Args@@)}))], 
		[File]).

apply_logging([FileAtom, OutFileAtom]) ->
    io:format("Applying logging to all functions.~n"),
    apply_logging(FileAtom, OutFileAtom, fun(File, LogID) -> log_mutation(File, LogID) end);

apply_logging([FileAtom, OutFileAtom | FunctionNames]) ->
    io:format("Applying logging to ~p.~n", [FunctionNames]),
    apply_logging(FileAtom, OutFileAtom, fun(File, LogID) -> specific_log_mutation(File, FunctionNames, LogID) end).

%% Attempts to determine a suitable external alphabet and instrument only those functions
apply_relevant_logging([FileAtom, OutFileAtom]) ->
    Module = mu2_output:get_module_name_from_filename(atom_to_list(FileAtom)),
    FunctionNames = mu2_module_explorer:get_alphabet(list_to_atom(Module)),
    io:format("Applying logging to ~p.~n", [FunctionNames]),
    apply_logging(FileAtom, OutFileAtom, fun(File, LogID) -> specific_log_mutation(File, FunctionNames, LogID) end).

apply_logging(FileAtom, OutFileAtom, Mutation) ->
    File = atom_to_list(FileAtom),
    OutFile = atom_to_list(OutFileAtom),
    LogID = case init:get_argument(log_id) of
		{ok, Val} ->
		    io:format("Will write log to ~p~n", [hd(hd(Val))]),
		    hd(hd(Val));
		_NotPresent -> 
		    mu2_output:get_module_name_from_filename(File)
	    end,
    io:format("Applying logging mutations to ~p and writing to ~p...~n", [File, OutFile]),
    wrangler_ast_server:start_ast_server(),
    {ok, [{{File, File}, ST}]} = Mutation(File, LogID),
    mu2_output:write_mutant([], OutFile, {File, "Logging", "all", "all", ST}).
    
convert_args([], Next) ->
    {[], [], Next};
convert_args([A | AS], Next) ->
    {Arg, LogArg, NewNext} = convert_if_nec(A, Next),
    {NewArgs, NewLogArgs, NewNewNext} = convert_args(AS, NewNext),
    {[Arg | NewArgs], [LogArg | NewLogArgs], NewNewNext};
convert_args(Wibble, Next) ->
    io:format("Can't convert this list of args: ~p~n", [Wibble]),
    {Wibble, Wibble, Next}.

convert_if_nec({wrapper,underscore,{attr, X, Attrs, Y}, {var,Loc,'_'}}, Next)  ->
    NewName = list_to_atom("MU2_LOG_" ++ lists:flatten(io_lib:format("~p", [Next]))),
    {{wrapper,variable,{attr, X, Attrs, Y}, {var,Loc,NewName}},
     {wrapper,variable,{attr, X, Attrs, Y}, {var,Loc,NewName}},
     Next+1};
convert_if_nec({wrapper,variable,{attr, X, Attrs, Y}, {var,Loc,Name}}, Next)  ->
    NameString = atom_to_list(Name),
    case hd(NameString) of
	95 ->
	    NewName = list_to_atom("MU2_LOG_" ++ lists:flatten(io_lib:format("~p", [Next])) ++ NameString),
	    {{wrapper,variable,{attr, X, Attrs, Y}, {var,Loc,NewName}}, 
	     {wrapper,variable,{attr, X, Attrs, Y}, {var,Loc,NewName}}, 
	     Next+1};
	_ ->
	    {{wrapper,variable,{attr, X, Attrs, Y}, {var,Loc,Name}},
	     {wrapper,variable,{attr, X, Attrs, Y}, {var,Loc,Name}},
	      Next}
    end;
convert_if_nec({wrapper, atom, Attrs, Content}, Next) ->
    %% nothing to do to atoms...
    {{wrapper, atom, Attrs, Content}, 
     {wrapper, atom, Attrs, Content}, 
     Next};
convert_if_nec({_Name, _Type, {attr, Loc, _AttrContent, _Something} = Attr, _Content} = Arg, Next) ->
    Right = {wrapper, variable, Attr, {var,Loc,list_to_atom("MU2_LOG_" ++ lists:flatten(io_lib:format("~p", [Next])))}},
    {{tree, match_expr, Attr, {match_expr, Arg, Right}}, Right, Next+1};
convert_if_nec(Arg, _Next) ->
    io:format("Failed to handle:~n~p~n", [Arg]),
    erlang:error("Failed to handle an arg.").

contains([], _L) ->
    false;
contains([L |_], L) ->
    true;
contains([_ | Ls], L) ->
    contains(Ls, L).

