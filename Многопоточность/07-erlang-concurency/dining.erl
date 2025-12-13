-module(dining).
-export([main/0]).

%%% ==================================================
%%% Entry point
%%% ==================================================

main() ->
    case file:read_file("input.txt") of
        {ok, Bin} ->
            Tokens = string:tokens(binary_to_list(Bin), " \n\t"),
            case Tokens of
                [Nstr, Kstr] ->
                    N = list_to_integer(Nstr),
                    K = list_to_integer(Kstr),
                    io:format("Dining Philosophers: N=~p, K=~p~n", [N, K]),
                    start(N, K);
                _ ->
                    io:format("Invalid input format. Use: N K~n")
            end;
        {error, Reason} ->
            io:format("Cannot read input.txt: ~p~n", [Reason])
    end.

%%% ==================================================
%%% Start system
%%% ==================================================

start(N, K) when N > 1, K > 0 ->
    rand:seed(exsplus, erlang:monotonic_time()),

    Forks = [spawn(fun fork/0) || _ <- lists:seq(1, N)],

    Parent = self(),

    lists:foreach(
        fun(I) ->
            Left  = lists:nth(I, Forks),
            Right = lists:nth((I rem N) + 1, Forks),
            spawn(fun() ->
                philosopher(I, Left, Right, K, Parent)
            end)
        end,
        lists:seq(1, N)
    ),

    wait(N),
    io:format("All philosophers finished. Program ends.~n");

start(_, _) ->
    io:format("N must be > 1 and K > 0~n").

%%% ==================================================
%%% Fork process
%%% ==================================================

fork() ->
    free().

free() ->
    receive
        {request, Pid} ->
            Pid ! {granted, self()},
            busy(Pid)
    end.

busy(Owner) ->
    receive
        {release, Owner} ->
            free()
    end.

%%% ==================================================
%%% Philosopher process with TIMEOUT
%%% ==================================================

philosopher(Id, ForkA, ForkB, K, Parent) ->
    {First, Second} =
        case ForkA < ForkB of
            true  -> {ForkA, ForkB};
            false -> {ForkB, ForkA}
        end,

    philosopher_loop(Id, First, Second, K),
    Parent ! {done, Id}.

philosopher_loop(_Id, _F1, _F2, 0) ->
    ok;

philosopher_loop(Id, F1, F2, K) ->
    think(Id),

    %% Try to take first fork
    F1 ! {request, self()},
    case receive_with_timeout(F1, 200) of
        ok ->
            %% Try to take second fork
            F2 ! {request, self()},
            case receive_with_timeout(F2, 200) of
                ok ->
                    eat(Id, K),
                    %% Release both forks
                    F2 ! {release, self()},
                    F1 ! {release, self()},
                    philosopher_loop(Id, F1, F2, K - 1);
                timeout ->
                    %% Failed to get second fork -> release first
                    F1 ! {release, self()},
                    retry(Id, F1, F2, K)
            end;
        timeout ->
            retry(Id, F1, F2, K)
    end.

retry(Id, F1, F2, K) ->
    io:format("Philosopher ~p retrying~n", [Id]),
    timer:sleep(rand:uniform(150)),
    philosopher_loop(Id, F1, F2, K).

%%% ==================================================
%%% Helper: receive with timeout
%%% ==================================================

receive_with_timeout(Fork, Timeout) ->
    receive
        {granted, Fork} ->
            ok
    after Timeout ->
            timeout
    end.

%%% ==================================================
%%% Wait for completion
%%% ==================================================

wait(0) -> ok;
wait(N) ->
    receive
        {done, Id} ->
            io:format("Philosopher ~p finished~n", [Id]),
            wait(N - 1)
    end.

%%% ==================================================
%%% Actions
%%% ==================================================

think(Id) ->
    io:format("Philosopher ~p is thinking~n", [Id]),
    timer:sleep(rand:uniform(300)).

eat(Id, K) ->
    io:format("Philosopher ~p is EATING (remaining ~p)~n", [Id, K]),
    timer:sleep(rand:uniform(300)).
