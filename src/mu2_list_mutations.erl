-module(mu2_list_mutations).

-export([all/0, missing/0]).

-include("../include/mutations.hrl").

all() ->
    missing().

missing() ->
    [
     {list_to_emptylist
      ,?MUTATION_RESTRICT("V@",
			  begin
			      case api_refac:type(V@) of
				  list ->
				      ?PP(V@) /= "[]";
				  _ ->
				      false
			      end
			  end)
      ,?MUTATION("V@",
		begin
		    ?TO_AST("[]")
		end)}
     ,{list_to_tail
      ,?MUTATION_RESTRICT("V@",
			  begin
			      case api_refac:type(V@) of
				  list ->
				      ?PP(V@) /= "[]";
				  _ ->
				      false
			      end
			  end)
      ,?MUTATION("V@",
		begin
		    ?TO_AST("tl(V@)")
		end)}
     ,{list_to_head
      ,?MUTATION_RESTRICT("V@",
			  begin
			      case api_refac:type(V@) of
				  list ->
				      ?PP(V@) /= "[]";
				  _ ->
				      false
			      end
			  end)
      ,?MUTATION("V@",
		begin
		    ?TO_AST("hd(V@)")
		end)}

    ].
