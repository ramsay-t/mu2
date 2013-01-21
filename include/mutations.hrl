-include("include/install.hrl").

-define(MUTATION_MATCH(From), fun(File) ->
						?FULL_TD_TU([?COLLECT(?T(From),
								      {File, api_refac:start_end_loc(_This@)}, 
								      true)], 
							    [File]) 
					end).

-define(MUTATION_EXCHANGE(From, To), fun(File, Loc) ->
					  ?FULL_TD_TP([?RULE(?T(From), 
							     ?TO_AST(To), 
							     api_refac:start_end_loc(_This@)==Loc)], 
						      [File]) 
				  end).

-define(MUTATION(From, Func), fun(File, Loc) ->
				      ?FULL_TD_TP([?RULE(?T(From), 
							 Func, 
							 api_refac:start_end_loc(_This@)==Loc)], 
						  [File]) 
			      end).
