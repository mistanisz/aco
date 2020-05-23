%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2020
%%%-------------------------------------------------------------------
-module(mrowka_behaviour).
-author("Michal Stanisz").

-callback select_next(#{Elem => {Cost, non_neg_integer()}}) -> {Elem, Cost}.

