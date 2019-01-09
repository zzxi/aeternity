-module(aestratum_target).


%recalculate(TargetQueue, MineRate, MinTargetCount, MaxTarget, MaxSolveTime) ->
%    K = MaxTarget * (1 bsl 32),
%    SumDivTargets = lists:sum([K div Target || {_Time, Target} <- PrevTargets]),
%    TotalSolveTime = lists:map(fun({Time, _}) when is_integer(Time) -> Time;
%                                  ({max, ,}) -> MaxSolveTimer
%                               end,
%
%
%
%
%
%
%
%recalculate2(PrevHeaders0) ->
%    N                        = aec_governance:key_blocks_to_check_difficulty_count(),
%    N                        = length(PrevHeaders0) - 1, %% Sanity check.
%    %% Ensure the list of previous headers are in order - oldest first.
%    SortFun                  = fun(H1, H2) -> aec_headers:height(H1) =< aec_headers:height(H2) end,
%    PrevHeaders              = lists:sort(SortFun, PrevHeaders0),
%    K                        = aec_pow:scientific_to_integer(?HIGHEST_TARGET_SCI) * (1 bsl 32),
%    SumKDivTargets           = lists:sum([ K div aec_pow:scientific_to_integer(aec_headers:target(Hd))
%                                           || Hd <- tl(PrevHeaders) ]),
%    DesiredTimeBetweenBlocks = aec_governance:expected_block_mine_rate(),
%    TotalSolveTime           = total_solve_time(PrevHeaders),
%    TemperedTST              = (3 * N * DesiredTimeBetweenBlocks) div 4 + (2523 * TotalSolveTime) div 10000,
%    NewTargetInt             = TemperedTST * K div (DesiredTimeBetweenBlocks * SumKDivTargets),
%    min(?HIGHEST_TARGET_SCI, aec_pow:integer_to_scientific(NewTargetInt)).
%
%total_solve_time(Headers) ->
%    Min = -aec_governance:accepted_future_block_time_shift(),
%    Max = 6 * aec_governance:expected_block_mine_rate(),
%    total_solve_time(Headers, {Min, Max}, 0).
%
%total_solve_time([_], _MinMax, Acc) -> Acc;
%total_solve_time([Hdr2 | [Hdr1 | _] = Hdrs], MinMax = {Min, Max}, Acc) ->
%    SolveTime0 = aec_headers:time_in_msecs(Hdr1) - aec_headers:time_in_msecs(Hdr2),
%    SolveTime =
%        if SolveTime0 < Min -> Min;
%           SolveTime0 > Max -> Max;
%           true             -> SolveTime0
%        end,
%    total_solve_time(Hdrs, MinMax, Acc + SolveTime).
%
