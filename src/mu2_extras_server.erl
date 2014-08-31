-module(mu2_extras_server).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3]).
-export([handle_cast/2,handle_cast/3,code_change/3,terminate/2,handle_info/2]).
-export([start_if_needed/0,get_all/0,add/1,pop_first/0]).

start_link() ->
    gen_server:start_link({local, mu2_extras}, mu2_extras_server, [], []).

init(_Args) ->
    {ok, []}.

handle_call({add,F}, _From, Fs) ->
    {reply, ok, [F | Fs]};
handle_call(clear, _From, _Fs) ->
    {reply, ok, []};
handle_call(pop_first, _From, Fs) ->
    case Fs of
	[] ->
	    {reply, {error, no_mutations}, []};
	_ ->
	    Backwards = lists:reverse(Fs),
	    {reply, hd(Backwards), lists:reverse(tl(Backwards))}
    end;
handle_call(get_all, _From, Fs) ->
    {reply, {ok, Fs}, Fs}.

terminate(_,_) ->
    ok.

start_if_needed() ->
    case global:whereis_name(mu2_extras) of
	undefined ->
	    start_link();
	_ ->
	    ok
    end.

get_all() ->
    start_if_needed(),
    gen_server:call(mu2_extras,get_all).

pop_first() ->
    start_if_needed(),
    gen_server:call(mu2_extras,pop_first).
    

add(F) ->
    start_if_needed(),
    gen_server:call(mu2_extras,{add,F}).
    

%% Unimplemented deliberately
handle_cast(_,_) ->
    exit("Unimplemented").
handle_cast(_,_,_) ->
    exit("Unimplemented").
code_change(_,_,_) ->
    exit("Unimplemented").
handle_info(_,_) ->
    exit("Unimplemented").
