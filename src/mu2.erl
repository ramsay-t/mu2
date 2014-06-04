-module(mu2).
-export([generate/2,generate/3,generate/4,test/3]).

generate(File,OutputFolder) ->
    generate(File, 1000, OutputFolder).

generate(File,Number,OutputFolder) ->
    generate(File, mu2_mutation:all_mutations(), Number, OutputFolder).

generate(File,Mutations,Number,OutputFolder) ->
    mu2_mutation:generate_mutants(File, Mutations, Number, OutputFolder).

test(Module,MutantFolder,TestFun) ->
    %% List the mutants
    Ms = file:list_dir(MutantFolder),
    lists:map(fun(M) -> one_test(Module, M, TestFun) end, Ms).

one_test(Module,MutantFile,TestFun) ->
    %% Rename the mutant
    io:format("Should load ~p and turn it into ~p~n",[MutantFile,Module]),
    TestFun().
