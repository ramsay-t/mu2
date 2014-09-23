-module(mu2).
-export([generate/2,generate/3,generate/4,test/4]).

generate(File,OutputFolder) ->
    generate(File, -1, OutputFolder).

generate(File,Number,OutputFolder) ->
    generate(File, mu2_mutation:all_mutations(), Number, OutputFolder).

generate(File,Mutations,Number,OutputFolder) ->
    mu2_mutation:generate_mutants(File, Mutations, Number, OutputFolder).

%% Loads all the mutants in MutantFolder, renames them to replace Module in Dir, and then runs TestFun() (which should run some tests on Module...)
test(Dir,Module,MutantFolder,TestFun) ->
    %% List the mutants
    {ok, Ms} = file:list_dir(MutantFolder),
    lists:map(fun(M) -> one_test(Dir,Module, filename:join(MutantFolder,M), TestFun) end, Ms).

one_test(Dir,Module,MutantFile,TestFun) ->
    %% Rename the mutant
    io:format("Testing ~p~n",[MutantFile]),
    {ok,MuST} = api_refac:get_ast(MutantFile),
    ModString = case is_atom(Module) of
		    true ->
			atom_to_list(Module);
		    _ ->
			Module
		end,
    mu2_output:write_mutant(Dir,ModString,MuST),
    TestFun().
