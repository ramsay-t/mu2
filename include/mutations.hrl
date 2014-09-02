-include("include/install.hrl").

-define(MUTATION_MATCH(From), fun(File) ->
						?FULL_TD_TU([?COLLECT(?T(From),
								      {File, api_refac:start_end_loc(_This@)}, 
								      true)], 
							    [File]) 
					end).

-define(MUTATION_RESTRICT(From,Fun), fun(File) ->
						?FULL_TD_TU([?COLLECT(?T(From),
								      {File, api_refac:start_end_loc(_This@)}, 
								      Fun)], 
							    [File]) 
					end).

-define(MUTATION_EXCHANGE(From, To), fun(AST, Loc) ->
					  ?FULL_TD_TP([?RULE(?T(From), 
							     ?TO_AST(To), 
							     api_refac:start_end_loc(_This@)==Loc)], 
						      AST) 
				  end).

-define(MUTATION(From, Func), fun(AST, Loc) ->
				      ?FULL_TD_TP([?RULE(?T(From), 
							 Func, 
							 api_refac:start_end_loc(_This@)==Loc)], 
						  AST)
			      end).

-define(GLOBAL_MUTATION(From, Restrict, Func), fun(AST, Loc) ->
						       ?FULL_TD_TP([?RULE(?T(From), 
									  Func, 
									  Restrict)], 
								   AST) 
					       end).
