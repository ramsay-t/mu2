-module(mu2_mutation).
-export([generate_mutants/4, random_mutation/2, all_mutations/0, generate_mutants/1, test/1, mutate/1]).

-include("../include/mutations.hrl").

test(File) ->
    io:format("GOT: ~p~n", [File]),
    generate_mutants(File, all_mutations(), 1000, "mutants/").
%    generate_mutants(File, mu2_datatype_mutations:all(), 1000, "mutants/").

all_mutations() ->
    mu2_exchange_mutations:all() ++ mu2_case_mutations:all() ++ mu2_if_mutations:all() ++ mu2_datatype_mutations:all().

%% This is for use on the command line via -s 
mutate([File, OutputFolder, CountString]) ->
    {Count, _Rem} = string:to_integer(atom_to_list(CountString)),
    io:format("Mutating ~p, generating ~p mutants in ~p~n", [File, Count, OutputFolder]),
    generate_mutants(atom_to_list(File), all_mutations(), Count, atom_to_list(OutputFolder)). 

generate_mutants(File, Mutations, Number, OutputFolder) ->
    wrangler_ast_server:start_ast_server(),
    case possible_mutations(File, Mutations) of
	[] ->
	    {error, "No possible mutations available for this file."};
	PosMuts ->
	    more_mutants(File, PosMuts, Number, OutputFolder)
    end.

generate_mutants([File, Mutations, Number, OutputFolder]) ->
    generate_mutants(File, Mutations, Number, OutputFolder).

random_mutation(File, PosMuts) ->
    Item = random:uniform(length(PosMuts)),
    {{Name, _Match, Mutation}, Loc} = lists:nth(Item, PosMuts),
    io:format("Applying ~p at ~p...~n", [Name, Loc]),
    {ok, [{{File, File}, ST}]} = erlang:apply(Mutation, [File, Loc]),
    {File, Name, Item, Loc, ST}.

%% Internal functions ----------------------------

more_mutants(_File, [], _Number, _OutputFolder) ->
    io:format("No more mutations possible.~n"),
    [];
%% Checks Number exactly 0; this allows you to use -1 to generate ALL possible mutants
more_mutants(_File, _PosMuts, Number, _OutputFolder) when (Number == 0) ->
    [];
more_mutants(File, PosMuts, Number, OutputFolder) ->
    {File, Name, Item, Loc, ST} = random_mutation(File, PosMuts),
    MutantName = mu2_output:make_mutant_name(File, Number),
    mu2_output:write_mutant(OutputFolder, MutantName, {File, Name, Item, Loc, ST}),
    {Pre, Post} = lists:split(Item, PosMuts),
    OtherPosMuts = lists:sublist(Pre, Item-1) ++ Post,
    io:format("Choosing ~p more mutants from ~p possiblities...~n", [Number-1, length(OtherPosMuts)]),
    [{MutantName, Name, Loc} | more_mutants(File, OtherPosMuts, Number-1, OutputFolder)].

possible_mutations(_File, []) ->
    [];
possible_mutations(File, [{Name, Match, Mutation} | Ms]) ->
    io:format("Checking applicability of ~p, ~p more to try...~n", [Name, length(Ms)]),
    lists:map(fun({_File, Loc}) -> {{Name, Match, Mutation}, Loc} end, erlang:apply(Match, [File])) 
	++ possible_mutations(File, Ms).

