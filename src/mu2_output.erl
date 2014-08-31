-module(mu2_output).

-export([write_mutant/3, make_mutant_name/3, get_module_name_from_filename/1]).

-compile(export_all).

-include("../include/mutations.hrl").

write_mutant(Folder, MutantName, {File, Name, _Item, Loc, ST}) ->
    Text = make_file_content(File, Name, Loc, MutantName, ST),
    case Folder of
	[] ->
	    FullName = MutantName;
	_ ->
	    FullName = Folder ++ "/" ++ MutantName
    end,
    io:format("Writing ~p...~n", [FullName]),
    {ok, IODevice} = file:open(FullName, [write]),
    file:write(IODevice, Text),
    file:close(IODevice).

make_mutant_name(File, MuName, {{SL,SC},{EL,EC}}) ->
    filename:basename(File, ".erl") ++ lists:flatten(io_lib:format("_~p_~p_~p_~p_~p", [MuName,SL,SC,EL,EC])) ++ ".erl".


%% Internal functions

rename_module(ST, NewName) ->
    io:format("Renaming to ~p~n", [NewName]),
    ?FULL_TD_TP([?RULE(?T("-module(name@)"), 
		       ?TO_AST("-module(" ++ NewName ++ ")"),
		       true)], 
		ST).
make_file_content(File, Name, Loc, MutantName, ST) ->
    case lists:suffix(".erl", MutantName) of
	true ->
	    NewMName = get_module_name_from_filename(MutantName);
	false ->
	    NewMName = MutantName
    end,
    {ok, NST} = rename_module(ST, NewMName),
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n" ++
	lists:flatten(io_lib:format("%% Generated from ~p~n%% Applied rule ~p at location ~p~n", [File, Name, Loc])) ++
	"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n" ++
    wrangler_prettypr:print_ast('unix',NST) ++ "\n".
    
get_module_name_from_filename(MutantName) ->
    lists:reverse(lists:nthtail(4, lists:reverse(filename:basename(MutantName)))).
