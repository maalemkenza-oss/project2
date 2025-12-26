-module(distributor).
-compile(export_all).
-import(lists,[map/2,sum/1,reverse/1,filter/2,append/2]).
-import(codinglist,[coding_list/1]).
-import(library,[split/2,is_Terminal/2,getInitialConf/1,displayOfConf/2,countSetBits/1,second/2]).
-import(testhash,[h/3]).

-define(N,4).
-define(M,10).
-define(solution_flag, solution_found_flag).

%%% --- Machines and references ---
workstation() -> [w0,w1,w2,w3,w4,w5,w6,w7,w8,w9].
procName(I) -> lists:nth(I+1, workstation()).
refvec() -> coding_list(?N).

%%% --- Global solution flag ---
solution_found() ->
    case whereis(?solution_flag) of
        undefined -> false;
        _Pid -> true
    end.

set_solution_found() ->
    register(?solution_flag, self()).

broadcast_stop_to_all() ->
    lists:foreach(fun(M) -> procName(M) ! stop end, lists:seq(0, ?M-1)).

%%% --- Process initialization ---
initiator(I) ->
    Refvec=refvec(),
    S0=getInitialConf(Refvec),
    I0=h(S0, ?M, Refvec),
    I == I0.

start(I) ->
    Refvec=refvec(),
    S0=getInitialConf(Refvec),
    I0=h(S0, ?M, Refvec),
    if
        I == I0 ->
            Pid = spawn(distributor, onReceive, [I,true,false,false,0,0,[S0],[]]),
            register(procName(I), Pid);
        I /= I0 ->
            Pid = spawn(distributor, onReceive, [I,false,false,false,0,0,[],[]]),
            register(procName(I), Pid)
    end.

startAll() -> startAl(?M).
startAl(0) -> true;
startAl(M) -> start(M-1), startAl(M-1).

%%% --- Generation of tasks ---
gen(I) -> spawn(distributor, generate, [I]).
generateAll() -> generateAl(?M).
generateAl(0) -> true;
generateAl(M) -> gen(M-1), generateAl(M-1).

%%% --- Stop processes ---
stopAll() -> stopAl(?M).
stopAl(0) -> true;
stopAl(M) -> sendStop(M-1), stopAl(M-1).

sendStop(I) -> procName(I) ! stop.

%%% --- Sending messages helpers ---
sendConf(Conf,I) -> procName(I) ! {state,Conf}.
sendRec(I,K) -> procName(I) ! {rec,K}.
sendSnd(I,K,Totalrecd) -> procName(I) ! {snd,K,Totalrecd}.
sendTerm(I,K) -> procName(I) ! {term,K}.

%%% --- OnReceive process ---
onReceive(I, Initiator, Terminit, Terminatedi, Nbrecdi, Nbsenti, S, T) ->
    receive
        stop -> ok;

        {state, Conf} ->
            Refvec = refvec(),
            if
                is_Terminal(Conf, Refvec) andalso not solution_found() ->
                    %% Première solution trouvée
                    io:format("Solution found on machine ~w: ~w~n", [I, library:displayOnChess(Conf, Refvec)]),
                    set_solution_found(),
                    broadcast_stop_to_all();
                true ->
                    %% Split la configuration
                    {Rs1, Rs2} = split(Conf, Refvec),
                    io:format("Machine ~w splitting: ~w -> ~w, ~w~n", [I, Conf, Rs1, Rs2]),
                    %% Continue la logique normale de distribution
                    continue_processing(I, [Rs1, Rs2], S, T)
            end,
            onReceive(I, Initiator, Terminit, Terminatedi, Nbrecdi, Nbsenti, S, T);

        %% Ici on peut garder le traitement original des messages rec, snd, term si nécessaire
        Other ->
            io:format("Machine ~w received: ~w~n", [I, Other]),
            onReceive(I, Initiator, Terminit, Terminatedi, Nbrecdi, Nbsenti, S, T)
    end.

%%% --- Traitement des nouvelles configurations ---
continue_processing(_I, [], _S, _T) -> ok;
continue_processing(I, [Conf|Rest], S, T) ->
    %% Ici tu enverras Conf à la machine correspondante via h(Conf, ?M, Refvec)
    Refvec = refvec(),
    Dest = h(Conf, ?M, Refvec),
    if Dest == I -> onReceive(I, false, false, false, 0, 0, [Conf|S], T);
       Dest /= I -> sendConf(Conf, Dest)
    end,
    continue_processing(I, Rest, S, T).

%%% --- Génération séquentielle simple (optionnelle) ---
generate(I) ->
    Pid = procName(I),
    Pid ! {self(), terminatedi}.
