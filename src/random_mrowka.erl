%%%-------------------------------------------------------------------
%%% @author Michal Stanisz
%%% @copyright (C) 2020
%%%-------------------------------------------------------------------
-module(random_mrowka).
-author("Michal Stanisz").

-behaviour(mrowka_behaviour).

%% API
-export([select_next/1]).

select_next(Map) ->
    List = maps:keys(Map),
    A = lists:nth(rand:uniform(length(List)), List),
    {Cost, _} = maps:get(A, Map),
    {A, Cost}.
