-module(mu2_module_explorer).
-export([get_alphabet/1,print_alphabet/1]).

get_alphabet(Module) ->
    MI = Module:module_info(),
    A = get_comp(attributes, MI),
    B = get_comp(behaviour, A),
    case B of
	{error,not_found,behaviour} ->
	    get_exports(MI);
	_List ->
	    case get_behaviour_alphabet(B) of
		{error, no_alphabet_found} ->
		    get_exports(MI);
		Aleph ->
		    Aleph
	    end
    end.

print_alphabet(Module) ->
    A = get_alphabet(Module),
    io:format("Alphabet: ~p~n", [A]).

get_exports(MI) ->
    lists:usort(get_comp(exports, MI)).

get_comp(C, []) ->
    {error, not_found, C};
get_comp(C, [{C,Content} | _]) ->
    Content;
get_comp(C, [_ | More]) ->
    get_comp(C, More).

get_behaviour_alphabet([]) ->
    {error, no_alphabet_found};
get_behaviour_alphabet([gen_server | _]) ->
    [{init,1},{handle_call,2},{handle_cast,2},{handle_info,2}];
get_behaviour_alphabet([_ | More]) ->
    %% Don't know what to do with the first behaviour? Try some of the others...
    get_behaviour_alphabet(More).


