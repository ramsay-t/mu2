-module(mu2_if_mutations).

-export([all/0, if_mutations/0]).

-include("../include/mutations.hrl").

all() ->
    if_mutations().

if_mutations() ->
    [{swap_if_order,?MUTATION_MATCH("if Guards@@@ -> Body@@@ end"),
      ?MUTATION("if Guards@@@ -> Body@@@ end",
		begin
		    A = random:uniform(length(Guards@@@)),
		    B = mu2_case_mutations:random_not_n(length(Guards@@@), A),
		    NewGuards@@@ = mu2_case_mutations:swap(Guards@@@, A, B),
		    NewBody@@@ = mu2_case_mutations:swap(Body@@@, A, B),
		    ?TO_AST("if NewGuards@@@-> NewBody@@@ end")
		end)},
     {exchange_if_guard,?MUTATION_MATCH("if Guards@@@ -> Body@@@ end"),
      ?MUTATION("if Guards@@@ -> Body@@@ end",
		begin
		    A = random:uniform(length(Guards@@@)),
		    B = mu2_case_mutations:random_not_n(length(Guards@@@), A),
		    NewGuards@@@ = mu2_case_mutations:swap(Guards@@@, A, B),
		    ?TO_AST("if NewGuards@@@-> Body@@@ end")
		end)},
     {exchange_if_pattern,?MUTATION_MATCH("if Guards@@@ -> Body@@@ end"),
      ?MUTATION("if Guards@@@ -> Body@@@ end",
		begin
		    A = random:uniform(length(Guards@@@)),
		    B = mu2_case_mutations:random_not_n(length(Guards@@@), A),
		    NewGuards@@@ = mu2_case_mutations:swap(Guards@@@, A, B),
		    ?TO_AST("if NewGuards@@@-> Body@@@ end")
		end)},
     {remove_last_if,?MUTATION_MATCH("if  Guards@@@ -> Body@@@ end"),
      ?MUTATION("if Guards@@@ -> Body@@@ end",
		begin
		    NewGuards@@@ = lists:sublist(Guards@@@, length(Guards@@@) -1),
		    NewBody@@@ = lists:sublist(Body@@@, length(Guards@@@) -1),
		    ?TO_AST("if NewGuards@@@-> NewBody@@@ end")
		end)}
    ].
