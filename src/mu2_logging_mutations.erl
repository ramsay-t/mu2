-module(mu2_logging_mutations).
-export([log_mutation/1, apply_logging/1, apply_relevant_logging/1, log/3]).

-include("../include/mutations.hrl").

log_mutation(File) ->
    ?FULL_TD_TP([?RULE(?T("f@(Args@@) when Guard@@-> Body@@;"), 
		       begin
			   {NewArgs@@, LogArgs@@, _} = convert_args(Args@@, 1),
			   ?TO_AST("f@(NewArgs@@) when Guard@@-> mu2_logging_mutations:log(?MODULE, f@, [LogArgs@@]), Body@@;")
		       end,
		       true)], 
		[File]).

specific_log_mutation(File, FunctionNames) ->
    ?FULL_TD_TP([?RULE(?T("f@(Args@@) when Guard@@-> Body@@;"), 
		       begin
			   {NewArgs@@, LogArgs@@, _} = convert_args(Args@@, 1),
			   ?TO_AST("f@(NewArgs@@) when Guard@@-> mu2_logging_mutations:log(?MODULE, f@, [LogArgs@@]), Body@@;")
		       end,
		       contains(FunctionNames, {list_to_atom(?PP(f@)), length(Args@@)}))], 
		[File]).

apply_logging([FileAtom, OutFileAtom]) ->
    File = atom_to_list(FileAtom),
    OutFile = atom_to_list(OutFileAtom),
    io:format("Applying all mutations to ~p and writing to ~p...~n", [File, OutFile]),
    wrangler_ast_server:start_ast_server(),
    {ok, [{{File, File}, ST}]} = log_mutation(File),
    mu2_output:write_mutant([], OutFile, {File, "Logging", "all", "all", ST});

apply_logging([FileAtom, OutFileAtom | FunctionNames]) ->
    File = atom_to_list(FileAtom),
    OutFile = atom_to_list(OutFileAtom),
    io:format("Applying mutations to ~p in ~p and writing to ~p...~n", [FunctionNames, File, OutFile]),
    wrangler_ast_server:start_ast_server(),
    {ok, [{{File, File}, ST}]} = specific_log_mutation(File, FunctionNames),
    mu2_output:write_mutant([], OutFile, {File, "Logging", "all", "all", ST}).

%% Attempts to determine a suitable external alphabet and instrument only those functions
apply_relevant_logging([FileAtom, OutFileAtom]) ->
    File = atom_to_list(FileAtom),
    OutFile = atom_to_list(OutFileAtom),
    Module = mu2_output:get_module_name_from_filename(File),
    FunctionNames = mu2_module_explorer:get_alphabet(list_to_atom(Module)),
    io:format("Applying mutations to ~p in ~p and writing to ~p...~n", [FunctionNames, File, OutFile]),
    wrangler_ast_server:start_ast_server(),
    {ok, [{{File, File}, ST}]} = specific_log_mutation(File, FunctionNames),
    mu2_output:write_mutant([], OutFile, {File, "Logging", "all", "all", ST}).
    
log(M, F, A) ->
    mu2_logger:log(M,F,A).

convert_args([], Next) ->
    {[], [], Next};
convert_args([A | AS], Next) ->
    {Arg, LogArg, NewNext} = convert_if_nec(A, Next),
    {NewArgs, NewLogArgs, NewNewNext} = convert_args(AS, NewNext),
    %%io:format("Converting~n~p~n---------~n~p~n>>>>>>>>>>>~n~p~n~n", [A, [Arg|NewArgs], [LogArg|NewLogArgs]]),
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
convert_if_nec({tree, list, Attrs, {list, Content, Last}}, Next) ->
    {NewContent, NewLogContent, NewNext} = convert_args(Content, Next),
    {NewLast, NewNewNext} = convert_if_nec(Last, NewNext),
    {{tree, list, Attrs, {list, NewContent, NewLast}}, 
     {tree, list, Attrs, {list, NewLogContent, NewLast}}, 
     NewNewNext};
convert_if_nec({tree, match_expr, Attrs, {match_expr, Left, Right}}, Next) ->
%%    io:format(">>>>>>>>>>>>>>>>>>~n~p~n~n~p~n<<<<<<<<<<<<<<<<<<<<<<~n", [Left,Right]),
%%erlang:error("Arg...."),
    {NewRight, NewLogRight, NewNext} = convert_if_nec(Right, Next),
    {{tree, match_expr, Attrs, {match_expr, Left, NewRight}}, 
     NewLogRight, 
     NewNext};
convert_if_nec({tree, Type, Attrs, Content}, Next) ->
    {NewContent, NewLogContent, NewNext} = convert_args(Content, Next),
    {{tree, Type, Attrs, NewContent}, 
     {tree, Type, Attrs, NewLogContent}, 
     NewNext};
convert_if_nec({wrapper, atom, Attrs, Content}, Next) ->
    %% nothing to do to atoms...
    {{wrapper, atom, Attrs, Content}, 
     {wrapper, atom, Attrs, Content}, 
     Next};
convert_if_nec(Arg, Next) ->
    %% Some other type of argument - maybe an atom?
    io:format("Unhandled arg... ~p~n", [Arg]),
    {Arg, Arg, Next}.

contains([], L) ->
    io:format("Doesn't contain ~p~n", [L]),
    false;
contains([L |_], L) ->
    io:format("Contains ~p~n", [L]),
    true;
contains([_ | Ls], L) ->
    contains(Ls, L).

