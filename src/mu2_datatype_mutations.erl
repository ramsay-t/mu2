-module(mu2_datatype_mutations).

-export([all/0]).

-include("../include/mutations.hrl").

all() ->
    [].
%%    atom_mutations() ++ tuple_mutations().

%% atom_mutations() ->
%%     [{fun_atom2var, ?MUTATION_MATCH("f@(Args@@) when Guard@@ -> Body@@."), 
%%       ?MUTATION("function@(Args@@) when Guard@@ -> Body@@.", 
%% 		begin
%% 		    Arg = random:uniform(length(Args@@)),
%% 		    %% Fixme change one...
%% 		    NewArgs@@ = Args@@,
%% 		    ?TO_AST("f@(NewArgs@@) when Guard@@ -> Body@@.")
%% 		end)}
%%     ].



%% Atoms...
%%		    case ?PP(_W_atom@) of
%%			[H|T] when H >= $a, H =< $z ->
%%			    AName = [H + ($A - $a) | T];
%%			Other ->
%%			    AName = Other
%%		    end,


%% tuple_mutations() ->
%%     [].
