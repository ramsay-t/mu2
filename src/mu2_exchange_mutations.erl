-module(mu2_exchange_mutations).

-export([all/0, arithmetic/0, logic/0, relational/0]).

-include("../include/mutations.hrl").

all() ->
    arithmetic() ++ logic() ++ relational().

%% {'+', '-', '*', '/', 'rem'}
arithmetic() ->
    [{plus_to_minus, ?MUTATION_MATCH("X@ + Y@"), ?MUTATION_EXCHANGE("X@ + Y@", "X@ - Y@")}
     ,{plus_to_mul, ?MUTATION_MATCH("X@ + Y@"), ?MUTATION_EXCHANGE("X@ + Y@", "X@ * Y@")}
     ,{plus_to_div, ?MUTATION_MATCH("X@ + Y@"), ?MUTATION_EXCHANGE("X@ + Y@", "X@ / Y@")}
     ,{plus_to_rem, ?MUTATION_MATCH("X@ + Y@"), ?MUTATION_EXCHANGE("X@ + Y@", "X@ rem Y@")}
     
     ,{minus_to_plus, ?MUTATION_MATCH("X@ - Y@"), ?MUTATION_EXCHANGE("X@ - Y@", "X@ + Y@")}
     ,{minus_to_mul, ?MUTATION_MATCH("X@ - Y@"), ?MUTATION_EXCHANGE("X@ - Y@", "X@ * Y@")}
     ,{minus_to_div, ?MUTATION_MATCH("X@ - Y@"), ?MUTATION_EXCHANGE("X@ - Y@", "X@ / Y@")}
     ,{minus_to_rem, ?MUTATION_MATCH("X@ - Y@"), ?MUTATION_EXCHANGE("X@ - Y@", "X@ rem Y@")}
     
     ,{mul_to_plus, ?MUTATION_MATCH("X@ * Y@"), ?MUTATION_EXCHANGE("X@ * Y@", "X@ + Y@")}
     ,{mul_to_minus, ?MUTATION_MATCH("X@ * Y@"), ?MUTATION_EXCHANGE("X@ * Y@", "X@ - Y@")}
     ,{mul_to_div, ?MUTATION_MATCH("X@ * Y@"), ?MUTATION_EXCHANGE("X@ * Y@", "X@ / Y@")}
     ,{mul_to_rem, ?MUTATION_MATCH("X@ * Y@"), ?MUTATION_EXCHANGE("X@ * Y@", "X@ rem Y@")}
     
     ,{div_to_plus, ?MUTATION_MATCH("X@ / Y@"), ?MUTATION_EXCHANGE("X@ / Y@", "X@ + Y@")}
     ,{div_to_minus, ?MUTATION_MATCH("X@ / Y@"), ?MUTATION_EXCHANGE("X@ / Y@", "X@ - Y@")}
     ,{div_to_mul, ?MUTATION_MATCH("X@ / Y@"), ?MUTATION_EXCHANGE("X@ / Y@", "X@ * Y@")}
     ,{div_to_rem, ?MUTATION_MATCH("X@ / Y@"), ?MUTATION_EXCHANGE("X@ / Y@", "X@ rem Y@")}
     
     ,{rem_to_plus, ?MUTATION_MATCH("X@ rem Y@"), ?MUTATION_EXCHANGE("X@ rem Y@", "X@ + Y@")}
     ,{rem_to_minus, ?MUTATION_MATCH("X@ rem Y@"), ?MUTATION_EXCHANGE("X@ rem Y@", "X@ - Y@")}
     ,{rem_to_mul, ?MUTATION_MATCH("X@ rem Y@"), ?MUTATION_EXCHANGE("X@ rem Y@", "X@ * Y@")}
     ,{rem_to_div, ?MUTATION_MATCH("X@ rem Y@"), ?MUTATION_EXCHANGE("X@ rem Y@", "X@ / Y@")}
    ].

%% {'and', 'or', 'xor', 'true', 'false'}
logic() ->
    [{and_to_or, ?MUTATION_MATCH("X@ and Y@"), ?MUTATION_EXCHANGE("X@ and Y@", "X@ or Y@")}
     ,{and_to_xor, ?MUTATION_MATCH("X@ and Y@"), ?MUTATION_EXCHANGE("X@ and Y@", "X@ xor Y@")}

     ,{or_to_and, ?MUTATION_MATCH("X@ or Y@"), ?MUTATION_EXCHANGE("X@ or Y@", "X@ and Y@")}
     ,{or_to_xor, ?MUTATION_MATCH("X@ or Y@"), ?MUTATION_EXCHANGE("X@ or Y@", "X@ xor Y@")}

     ,{xor_to_and, ?MUTATION_MATCH("X@ xor Y@"), ?MUTATION_EXCHANGE("X@ xor Y@", "X@ and Y@")}
     ,{xor_to_or, ?MUTATION_MATCH("X@ xor Y@"), ?MUTATION_EXCHANGE("X@ xor Y@", "X@ or Y@")}

     ,{true_to_false, ?MUTATION_MATCH("true"), ?MUTATION_EXCHANGE("true", "false")}
     ,{false_to_true, ?MUTATION_MATCH("false"), ?MUTATION_EXCHANGE("false", "true")}

     ,{remove_negation, ?MUTATION_MATCH("not F@"), ?MUTATION_EXCHANGE("not F@", "F@")}
    ].

%% {'<', '==', '>', '>=', '=<', '/=', '=:=', '=/='}
relational() ->
    [{lt_to_eq, ?MUTATION_MATCH("X@ < Y@"), ?MUTATION_EXCHANGE("X@ < Y@", "X@ == Y@")}
     ,{lt_to_gt, ?MUTATION_MATCH("X@ < Y@"), ?MUTATION_EXCHANGE("X@ < Y@", "X@ > Y@")}
     ,{lt_to_ge, ?MUTATION_MATCH("X@ < Y@"), ?MUTATION_EXCHANGE("X@ < Y@", "X@ >= Y@")}
     ,{lt_to_le, ?MUTATION_MATCH("X@ < Y@"), ?MUTATION_EXCHANGE("X@ < Y@", "X@ =< Y@")}
     ,{lt_to_ne, ?MUTATION_MATCH("X@ < Y@"), ?MUTATION_EXCHANGE("X@ < Y@", "X@ /= Y@")}
     ,{lt_to_te, ?MUTATION_MATCH("X@ < Y@"), ?MUTATION_EXCHANGE("X@ < Y@", "X@ =:= Y@")}
     ,{lt_to_nte, ?MUTATION_MATCH("X@ < Y@"), ?MUTATION_EXCHANGE("X@ < Y@", "X@ =/= Y@")}

     ,{eq_to_lt, ?MUTATION_MATCH("X@ == Y@"), ?MUTATION_EXCHANGE("X@ == Y@", "X@ < Y@")}
     ,{eq_to_gt, ?MUTATION_MATCH("X@ == Y@"), ?MUTATION_EXCHANGE("X@ == Y@", "X@ > Y@")}
     ,{eq_to_ge, ?MUTATION_MATCH("X@ == Y@"), ?MUTATION_EXCHANGE("X@ == Y@", "X@ >= Y@")}
     ,{eq_to_le, ?MUTATION_MATCH("X@ == Y@"), ?MUTATION_EXCHANGE("X@ == Y@", "X@ =< Y@")}
     ,{eq_to_ne, ?MUTATION_MATCH("X@ == Y@"), ?MUTATION_EXCHANGE("X@ == Y@", "X@ /= Y@")}
     ,{eq_to_te, ?MUTATION_MATCH("X@ == Y@"), ?MUTATION_EXCHANGE("X@ == Y@", "X@ =:= Y@")}
     ,{eq_to_nte, ?MUTATION_MATCH("X@ == Y@"), ?MUTATION_EXCHANGE("X@ == Y@", "X@ =/= Y@")}

     ,{gt_to_lt, ?MUTATION_MATCH("X@ > Y@"), ?MUTATION_EXCHANGE("X@ > Y@", "X@ < Y@")}
     ,{gt_to_eq, ?MUTATION_MATCH("X@ > Y@"), ?MUTATION_EXCHANGE("X@ > Y@", "X@ == Y@")}
     ,{gt_to_ge, ?MUTATION_MATCH("X@ > Y@"), ?MUTATION_EXCHANGE("X@ > Y@", "X@ >= Y@")}
     ,{gt_to_le, ?MUTATION_MATCH("X@ > Y@"), ?MUTATION_EXCHANGE("X@ > Y@", "X@ =< Y@")}
     ,{gt_to_ne, ?MUTATION_MATCH("X@ > Y@"), ?MUTATION_EXCHANGE("X@ > Y@", "X@ /= Y@")}
     ,{gt_to_te, ?MUTATION_MATCH("X@ > Y@"), ?MUTATION_EXCHANGE("X@ > Y@", "X@ =:= Y@")}
     ,{gt_to_nte, ?MUTATION_MATCH("X@ > Y@"), ?MUTATION_EXCHANGE("X@ > Y@", "X@ =/= Y@")}

    ,{ge_to_lt, ?MUTATION_MATCH("X@ >= Y@"), ?MUTATION_EXCHANGE("X@ >= Y@", "X@ < Y@")}
    ,{ge_to_eq, ?MUTATION_MATCH("X@ >= Y@"), ?MUTATION_EXCHANGE("X@ >= Y@", "X@ == Y@")}
    ,{ge_to_gt, ?MUTATION_MATCH("X@ >= Y@"), ?MUTATION_EXCHANGE("X@ >= Y@", "X@ > Y@")}
    ,{ge_to_le, ?MUTATION_MATCH("X@ >= Y@"), ?MUTATION_EXCHANGE("X@ >= Y@", "X@ =< Y@")}
    ,{ge_to_ne, ?MUTATION_MATCH("X@ >= Y@"), ?MUTATION_EXCHANGE("X@ >= Y@", "X@ /= Y@")}
    ,{ge_to_te, ?MUTATION_MATCH("X@ >= Y@"), ?MUTATION_EXCHANGE("X@ >= Y@", "X@ =:= Y@")}
    ,{ge_to_nte, ?MUTATION_MATCH("X@ >= Y@"), ?MUTATION_EXCHANGE("X@ >= Y@", "X@ =/= Y@")}

    ,{le_to_lt, ?MUTATION_MATCH("X@ =< Y@"), ?MUTATION_EXCHANGE("X@ =< Y@", "X@ < Y@")}
    ,{le_to_eq, ?MUTATION_MATCH("X@ =< Y@"), ?MUTATION_EXCHANGE("X@ =< Y@", "X@ == Y@")}
    ,{le_to_gt, ?MUTATION_MATCH("X@ =< Y@"), ?MUTATION_EXCHANGE("X@ =< Y@", "X@ > Y@")}
    ,{le_to_ge, ?MUTATION_MATCH("X@ =< Y@"), ?MUTATION_EXCHANGE("X@ =< Y@", "X@ =< Y@")}
    ,{le_to_ne, ?MUTATION_MATCH("X@ =< Y@"), ?MUTATION_EXCHANGE("X@ =< Y@", "X@ /= Y@")}
    ,{le_to_te, ?MUTATION_MATCH("X@ =< Y@"), ?MUTATION_EXCHANGE("X@ =< Y@", "X@ =:= Y@")}
    ,{le_to_nte, ?MUTATION_MATCH("X@ =< Y@"), ?MUTATION_EXCHANGE("X@ =< Y@", "X@ =/= Y@")}

    ,{ne_to_lt, ?MUTATION_MATCH("X@ /= Y@"), ?MUTATION_EXCHANGE("X@ /= Y@", "X@ < Y@")}
    ,{ne_to_eq, ?MUTATION_MATCH("X@ /= Y@"), ?MUTATION_EXCHANGE("X@ /= Y@", "X@ == Y@")}
    ,{ne_to_gt, ?MUTATION_MATCH("X@ /= Y@"), ?MUTATION_EXCHANGE("X@ /= Y@", "X@ > Y@")}
    ,{ne_to_ge, ?MUTATION_MATCH("X@ /= Y@"), ?MUTATION_EXCHANGE("X@ /= Y@", "X@ >= Y@")}
    ,{ne_to_le, ?MUTATION_MATCH("X@ /= Y@"), ?MUTATION_EXCHANGE("X@ /= Y@", "X@ =< Y@")}
    ,{ne_to_te, ?MUTATION_MATCH("X@ /= Y@"), ?MUTATION_EXCHANGE("X@ /= Y@", "X@ =:= Y@")}
    ,{ne_to_nte, ?MUTATION_MATCH("X@ /= Y@"), ?MUTATION_EXCHANGE("X@ /= Y@", "X@ =/= Y@")}

    ,{te_to_lt, ?MUTATION_MATCH("X@ =:= Y@"), ?MUTATION_EXCHANGE("X@ =:= Y@", "X@ < Y@")}
    ,{te_to_eq, ?MUTATION_MATCH("X@ =:= Y@"), ?MUTATION_EXCHANGE("X@ =:= Y@", "X@ == Y@")}
    ,{te_to_gt, ?MUTATION_MATCH("X@ =:= Y@"), ?MUTATION_EXCHANGE("X@ =:= Y@", "X@ > Y@")}
    ,{te_to_ge, ?MUTATION_MATCH("X@ =:= Y@"), ?MUTATION_EXCHANGE("X@ =:= Y@", "X@ >= Y@")}
    ,{te_to_le, ?MUTATION_MATCH("X@ =:= Y@"), ?MUTATION_EXCHANGE("X@ =:= Y@", "X@ =< Y@")}
    ,{te_to_ne, ?MUTATION_MATCH("X@ =:= Y@"), ?MUTATION_EXCHANGE("X@ =:= Y@", "X@ /= Y@")}
    ,{te_to_nte, ?MUTATION_MATCH("X@ =:= Y@"), ?MUTATION_EXCHANGE("X@ =:= Y@", "X@ =/= Y@")}

    ,{nte_to_lt, ?MUTATION_MATCH("X@ =/= Y@"), ?MUTATION_EXCHANGE("X@ =/= Y@", "X@ < Y@")}
    ,{nte_to_eq, ?MUTATION_MATCH("X@ =/= Y@"), ?MUTATION_EXCHANGE("X@ =/= Y@", "X@ == Y@")}
    ,{nte_to_gt, ?MUTATION_MATCH("X@ =/= Y@"), ?MUTATION_EXCHANGE("X@ =/= Y@", "X@ > Y@")}
    ,{nte_to_ge, ?MUTATION_MATCH("X@ =/= Y@"), ?MUTATION_EXCHANGE("X@ =/= Y@", "X@ >= Y@")}
    ,{nte_to_le, ?MUTATION_MATCH("X@ =/= Y@"), ?MUTATION_EXCHANGE("X@ =/= Y@", "X@ =< Y@")}
    ,{nte_to_ne, ?MUTATION_MATCH("X@ =/= Y@"), ?MUTATION_EXCHANGE("X@ =/= Y@", "X@ /= Y@")}
    ,{nte_to_te, ?MUTATION_MATCH("X@ =/= Y@"), ?MUTATION_EXCHANGE("X@ =/= Y@", "X@ =:= Y@")}
    ].
