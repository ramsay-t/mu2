-module(mu2_logging_mutations).
-export([log_mutation/1, apply_logging/1, log/3]).

-include("../include/mutations.hrl").

log_mutation(File) ->
    ?FULL_TD_TP([?RULE(?T("f@(Args@@) when Guard@@-> Body@@;"), 
		       begin
			   {NewArgs@@, _} = convert_args(Args@@, 1),
			   ?TO_AST("f@(NewArgs@@) when Guard@@-> mu2_logging_mutations:log(?MODULE, f@, [NewArgs@@]), Body@@;")
		       end,
		       true)], 
		[File]).

specific_log_mutation(File, FunctionNames) ->
    ?FULL_TD_TP([?RULE(?T("f@(Args@@) when Guard@@-> Body@@;"), 
		       begin
			   {NewArgs@@, _} = convert_args(Args@@, 1),
			   ?TO_AST("f@(NewArgs@@) when Guard@@-> mu2_logging_mutations:log(?MODULE, f@, [NewArgs@@]), Body@@;")
		       end,
		       contains(FunctionNames, list_to_atom(?PP(f@))))], 
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



log(M, F, A) ->
    mu2_logger:log(M,F,A).

convert_args([], Next) ->
    {[], Next};
convert_args([A | AS], Next) ->
    {Arg, NewNext} = convert_if_nec(A, Next),
    {NewArgs, NewNewNext} = convert_args(AS, NewNext),
    {[Arg | NewArgs], NewNewNext};
convert_args(Wibble, _Next) ->
    io:format("Cant convert this list of args: ~p~n", [Wibble]).

convert_if_nec({wrapper,underscore,{attr, X, Attrs, Y}, {var,Loc,'_'}}, Next)  ->
    NewName = list_to_atom("MU2_LOG_" ++ lists:flatten(io_lib:format("~p", [Next]))),
    {{wrapper,variable,{attr, X, Attrs, Y}, {var,Loc,NewName}}, Next+1};
convert_if_nec({wrapper,variable,{attr, X, Attrs, Y}, {var,Loc,Name}}, Next)  ->
    NameString = atom_to_list(Name),
    case hd(NameString) of
	95 ->
	    NewName = list_to_atom("MU2_LOG_" ++ lists:flatten(io_lib:format("~p", [Next])) ++ NameString),
	    {{wrapper,variable,{attr, X, Attrs, Y}, {var,Loc,NewName}}, Next+1};
	_ ->
	    {{wrapper,variable,{attr, X, Attrs, Y}, {var,Loc,Name}}, Next}
    end;
convert_if_nec({tree, list, Attrs, {list, Content, Last}}, Next) ->
    {NewContent, NewNext} = convert_args(Content, Next),
    {NewLast, NewNewNext} = convert_if_nec(Last, NewNext),
    {{tree, list, Attrs, {list, NewContent, NewLast}}, NewNewNext};
convert_if_nec({tree, Type, Attrs, Content}, Next) ->
    {NewContent, NewNext} = convert_args(Content, Next),
    {{tree, Type, Attrs, NewContent}, NewNext};
convert_if_nec({wrapper, atom, Attrs, Content}, Next) ->
    %% nothing to do to atoms...
    {{wrapper, atom, Attrs, Content}, Next};
convert_if_nec(Arg, Next) ->
    %% Some other type of argument - maybe an atom?
    io:format("Unhandled arg... ~p~n", [Arg]),
    {Arg, Next}.

contains([], _) ->
    false;
contains([L |_], L) ->
    true;
contains([_ | Ls], L) ->
    contains(Ls, L).

