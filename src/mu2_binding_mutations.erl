-module(mu2_binding_mutations).

-export([all/0, binding_mutations/0]).
-include("../include/mutations.hrl").

all() ->
    binding_mutations().

-compile(nowarn_shadow_vars).

binding_mutations() ->
    [{reuse_bound_var,
      ?MUTATION_RESTRICT("V@ = Expr@",
			 begin
			     {wrapper,variable,{attr,_Loc,Attrs,_},_Image} = V@,
				 {env,Env} = lists:keyfind(env,1,Attrs),
			     length(Env) > 0
			 end),
      ?MUTATION("V@ = Expr@",
	 begin
	     {wrapper,variable,{attr,_Loc,Attrs,_},_Image} = V@,
		 {env,Env} = lists:keyfind(env,1,Attrs),
	     {NewV,_NewVLoc} = lists:nth(random:uniform(length(Env)),Env),
	     NewVList = atom_to_list(NewV),
	     %% FIXME doesn't work - suspect nested definitions are broken...
	     mu2_extras_server:add(?GLOBAL_MUTATION("OtherV@",
						    begin
							io:format("~p: ~p~n",[?PP(OtherV@),api_refac:type(OtherV@)]),
							%%api_refac:type(OtherV@) == variable
							false
						    end,
						    begin
							io:format("Being applied to ~p~n",[Loc]),
							%%io:format("Making ~p~n",[NewVList]),
							%%?TO_AST(NewVList)
							?TO_AST("OtherV@")
						    end
						   )
				  ),
	     
	     ?TO_AST(NewVList ++ " = Expr@")
	 end)}
    ].
