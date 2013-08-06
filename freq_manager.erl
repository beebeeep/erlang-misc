-module(freq_manager).
-export([start/1, stop/0, allocate/1, deallocate/1, list/0]).
-export([init/1]).

start(FreqList) -> 
    Pid = spawn(freq_manager, init, [FreqList]),
    register(freq_manager, Pid),
    ok.

stop() -> 
    freq_manager ! {stop, self()},
    receive {reply, Reply} -> Reply end.

allocate(Count) -> call({allocate, Count}).
deallocate(Freq) -> call({deallocate, Freq}).
list() -> call(list).

call(Msg) ->
    freq_manager ! {request, self(), Msg},
    receive {reply, Reply} -> Reply end.

reply(To, Reply) ->
    To ! {reply, Reply}.

init(FreqList) -> 
    process_flag(trap_exit, true),
    io:format("Frequency manager: started at ~p, trapping exits~n", [self()]),
    loop({[],FreqList}).

loop(Freqs) ->
    receive 
        {request, From, {allocate, FreqCount}} -> 
            {Reply, NewFreqs} = manager_allocate(FreqCount, Freqs, From),
            reply(From, Reply),
            loop(NewFreqs);
        {request, From, {deallocate, Freq}} ->
            {Reply, NewFreqs} = manager_deallocate(Freq, Freqs, From),
            reply(From, Reply),
            loop(NewFreqs);
        {request, From, list} -> 
            reply(From, {ok, Freqs}),
            loop(Freqs);
        {'EXIT', Pid, Reason} -> 
            io:format("Frequency manager: recieved EXIT signal from linked process ~p: ~p, deallocating its frequency~n", [Pid, Reason]),
            NewFreqs = process_exited(Freqs, Pid),
            loop(NewFreqs);
        {stop, From} -> 
            reply(From, ok)
    end.

freq_allocator(0, Freqs, _Pid, Result) -> 
    {Result, Freqs};
freq_allocator(Count, {AllocatedFreqs, [Freq|RestFreqs]}, Pid, Result) ->
    freq_allocator(Count-1, { [{Freq, Pid}|AllocatedFreqs], RestFreqs }, Pid, [Freq|Result]).


manager_allocate(FreqCount, {AllocatedFreqs, FreeFreqs}, Pid) ->
    case FreqCount < length(FreeFreqs) of 
        true -> 
            io:format("Frequency manager: allocating frequency and linking to ~p~n", [Pid]),
            link(Pid),
            freq_allocator(FreqCount, {AllocatedFreqs, FreeFreqs}, Pid, []);
        false -> 
            {
                {error, no_frequencies},
                {AllocatedFreqs, FreeFreqs}
            }
    end.

manager_deallocate(Freq, {AllocatedFreqs, FreeFreqs}, Pid) -> 
    Deallocated = lists:keyfind(Freq, 1, AllocatedFreqs),
    case Deallocated of
        false ->
            { 
                {error, frequency_not_allocated}, 
                {AllocatedFreqs, FreeFreqs} 
            };
        {Freq, Pid} ->
            io:format("Frequency manager: deallocating frequency and unlinking from ~p~n", [Pid]),
            unlink(Pid),
            {
                ok, 
                {lists:keydelete(Freq, 1, AllocatedFreqs), [Freq|FreeFreqs]}
            };
        {Freq, _OtherPid} -> 
            { 
                {error, not_your_frequency}, 
                {AllocatedFreqs, FreeFreqs} 
            } 
    end.

process_exited({AllocatedFreqs, FreeFreqs}, Pid) -> 
    case lists:keyfind(Pid, 2, AllocatedFreqs) of 
        {Freq, Pid} ->
            io:format("Frequency manager: deallocating frequency ~p from dead process ~p~n", [Freq, Pid]),
            {lists:keydelete(Freq, 1, AllocatedFreqs), [Freq|FreeFreqs]};
        false ->
            io:format("Frequency manager: hmmm, there was no allocated frequency for process ~p~n", [Pid]),
            {AllocatedFreqs, FreeFreqs}
    end.
