-module(mu2_output).

-export([write_mutant/3, make_mutant_name/2]).

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

make_mutant_name(File, Number) ->
    filename:basename(File, ".erl") ++ "_" ++ lists:flatten(io_lib:format("~p", [Number])) ++ ".erl".


%% Internal functions

rename_module(ST, NewName) ->
    io:format("Renaming to ~p~n", [NewName]),
    ?FULL_TD_TP([?RULE(?T("-module(name@)"), 
		       ?TO_AST("-module(" ++ NewName ++ ")"),
		       true)], 
		[ST]).

make_file_content(File, Name, Loc, MutantName, ST) ->
    case lists:suffix(".erl", MutantName) of
	true ->
	    NewMName = lists:reverse(lists:nthtail(4, lists:reverse(filename:basename(MutantName))));
	false ->
	    NewMName = MutantName
    end,
    {ok, NST} = rename_module(ST, NewMName),
    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n" ++
	lists:flatten(io_lib:format("%% Generated from ~p~n%% Applied rule ~p at location ~p~n", [File, Name, Loc])) ++
	"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n" ++
	?PP(NST) ++ "\n".
    
