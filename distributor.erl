-module(distributor).
-compile(export_all).
-import(lists,[map/2,sum/1,reverse/1,filter/2,append/2]).
-import(codinglist,[coding_list/1]).
-import(library,[split/2,is_Terminal/2,getInitialConf/1,displayOfConf/2,countSetBits/1,second/2]).
-import(testhash,[h/3,allConfiguration/3,maph/3,numOfConfByMachines/3]).
-define(N,8).   % Problème des 8 Dames
-define(M,10).  % 10 workers

% Variable globale pour coordonner l'arrêt
-define(CONTROLLER, solution_controller).

workstation() -> [w0,w1,w2,w3,w4,w5,w6,w7,w8,w9].

indice(0, [H|_]) -> H;
indice(I, [_|T]) -> indice(I - 1, T).
procName(I) -> indice(I, workstation()).

refvec() -> coding_list(?N).

initiator(I) -> 
    Refvec = refvec(),
    S0 = getInitialConf(Refvec), 
    I0 = h(S0, ?M, Refvec),
    I == I0.

% Contrôleur central pour gérer l'arrêt
start_controller() ->
    case whereis(?CONTROLLER) of
        undefined ->
            Pid = spawn(fun() -> controller_loop(false, undefined, []) end),
            register(?CONTROLLER, Pid),
            Pid;
        Pid -> Pid
    end.

controller_loop(SolutionFound, Solution, Workers) ->
    receive
        {register, WorkerPid} ->
            NewWorkers = [WorkerPid | Workers],
            controller_loop(SolutionFound, Solution, NewWorkers);
        
        {found_solution, WorkerPid, Conf} ->
            case SolutionFound of
                false ->
                    % PREMIÈRE SOLUTION TROUVÉE !
                    io:format("~n" ++ lists:duplicate(70, "*") ++ "~n"),
                    io:format("*** PREMIERE SOLUTION TROUVEE par worker ~p ***~n", [WorkerPid]),
                    
                    % Afficher la solution
                    Refvec = refvec(),
                    ChessPositions = library:displayOnChess(Conf, Refvec),
                    io:format("Positions des dames: ~w~n", [ChessPositions]),
                    display_chess_board(ChessPositions),
                    
                    % Arrêter IMMÉDIATEMENT tous les workers
                    io:format("*** ARRET IMMEDIAT DE TOUS LES WORKERS ***~n"),
                    stop_all_workers(Workers),
                    
                    controller_loop(true, Conf, Workers);
                true ->
                    % Solution déjà trouvée, ignorer
                    controller_loop(SolutionFound, Solution, Workers)
            end;
        
        {get_status, From} ->
            From ! {controller_status, SolutionFound, Solution},
            controller_loop(SolutionFound, Solution, Workers);
        
        stop ->
            ok
    end.

% Arrêter tous les workers immédiatement
stop_all_workers(Workers) ->
    lists:foreach(
        fun(Pid) ->
            try
                Pid ! stop_immediate_solution
            catch
                _:_ -> ok
            end
        end,
        Workers
    ).

% Vérifier si une solution a déjà été trouvée
is_solution_found() ->
    case whereis(?CONTROLLER) of
        undefined -> false;
        Pid ->
            try
                Pid ! {get_status, self()},
                receive
                    {controller_status, Found, _} -> Found
                after 50 -> false
                end
            catch
                _:_ -> false
            end
    end.

% Démarrer un worker
start(I) -> 
    % Démarrer le contrôleur si nécessaire
    start_controller(),
    
    Refvec = refvec(),
    S0 = getInitialConf(Refvec), 
    I0 = h(S0, ?M, Refvec),
    
    if 
        I == I0 -> 
            Pid = spawn(distributor_one_solution, onReceive, 
                       [I, true, false, false, 0, 0, [S0], []]),
            register(procName(I), Pid),
            
            % Enregistrer le worker auprès du contrôleur
            ?CONTROLLER ! {register, Pid},
            Pid;
        true -> 
            Pid = spawn(distributor_one_solution, onReceive, 
                       [I, false, false, false, 0, 0, [], []]), 
            register(procName(I), Pid),
            
            % Enregistrer le worker auprès du contrôleur
            ?CONTROLLER ! {register, Pid},
            Pid
    end.

startAll() -> startAl(?M).
startAl(0) -> true;
startAl(M) -> start(M-1), startAl(M-1).

gen(I) -> spawn(distributor_one_solution, generate, [I]).

generateAll() -> generateAl(?M).
generateAl(0) -> true;
generateAl(M) -> gen(M-1), generateAl(M-1).

stopAll() -> 
    case whereis(?CONTROLLER) of
        undefined -> ok;
        Pid -> Pid ! stop
    end,
    stopAl(?M).
stopAl(0) -> true;
stopAl(M) -> sendStop(M-1), stopAl(M-1).

% Envoyer une configuration (vérifier d'abord si solution trouvée)
sendConf(Conf, I) -> 
    case is_solution_found() of
        false ->
            case whereis(procName(I)) of
                undefined -> ok;
                Pid -> 
                    try
                        Pid ! {state, Conf}
                    catch
                        _:_ -> ok
                    end
            end;
        true -> ok
    end.

sendRec(I, K) -> 
    case is_solution_found() of
        false ->
            case whereis(procName(I)) of
                undefined -> ok;
                Pid -> 
                    try
                        Pid ! {rec, K}
                    catch
                        _:_ -> ok
                    end
            end;
        true -> ok
    end.

sendSnd(I, K, Totalrecd) -> 
    case is_solution_found() of
        false ->
            case whereis(procName(I)) of
                undefined -> ok;
                Pid -> 
                    try
                        Pid ! {snd, K, Totalrecd}
                    catch
                        _:_ -> ok
                    end
            end;
        true -> ok
    end.

sendTerm(I, K) -> 
    case is_solution_found() of
        false ->
            case whereis(procName(I)) of
                undefined -> ok;
                Pid -> 
                    try
                        Pid ! {term, K}
                    catch
                        _:_ -> ok
                    end
            end;
        true -> ok
    end.

sendStop(I) -> 
    case whereis(procName(I)) of
        undefined -> ok;
        Pid -> 
            try
                Pid ! stop
            catch
                _:_ -> ok
            end
    end.

% Générateur de travail
generate(I) -> 
    case is_solution_found() of
        false ->
            W = procName(I),
            case whereis(W) of
                undefined -> 
                    io:format("Worker ~w deja arrete~n", [I]);
                _ ->
                    W ! {self(), terminatedi},
                    receive 
                        {W, {I, Initiator, Terminit, Terminatedi, Len, 
                             Nbrecdi, Nbsenti, S, T}} -> 
                            case is_solution_found() of
                                false ->
                                    if 
                                        ((not Terminatedi) and (Len /= 0)) -> 
                                            io:format("Worker ~w: traitement (~w configs)~n", 
                                                     [I, Len]),
                                            W ! {self(), gen},
                                            generate(I);
                                        ((not Terminatedi) and (Len == 0)) -> 
                                            W ! {self(), gen},
                                            io:format("Worker ~w: attente...~n", [I]), 
                                            generate(I);
                                        (Terminatedi and (Len == 0)) ->
                                            io:format("Worker ~w: terminaison detectee~n", [I]),
                                            sendStop(I) 
                                    end;
                                true ->
                                    io:format("Worker ~w: arret (solution trouvee)~n", [I])
                            end
                    after 2000 ->
                        io:format("Worker ~w: timeout~n", [I])
                    end
            end;
        true ->
            io:format("Worker ~w: arret (solution deja trouvee)~n", [I])
    end.

% Processus principal du worker
onReceive(I, Initiator, Terminit, Terminatedi, Nbrecdi, Nbsenti, S, T) -> 
    receive
        {state, Conf} -> 
            case is_solution_found() of
                false ->
                    Refvec = refvec(),
                    case is_Terminal(Conf, Refvec) of
                        true ->
                            Fst = library:first(Conf, Refvec),
                            case countSetBits(Fst) of
                                ?N ->  % SOLUTION TROUVÉE !
                                    % Signaler au contrôleur
                                    ?CONTROLLER ! {found_solution, self(), Conf},
                                    ok;  % Ce worker s'arrête
                                _ ->
                                    Nbr = Nbrecdi + 1,
                                    onReceive(I, Initiator, Terminit, Terminatedi, 
                                             Nbr, Nbsenti, S, [Conf|T])
                            end;
                        false ->
                            Nbr = Nbrecdi + 1,
                            onReceive(I, Initiator, Terminit, Terminatedi, 
                                     Nbr, Nbsenti, [Conf|S], T)
                    end;
                true ->
                    ok  % Solution déjà trouvée, s'arrêter
            end;
            
        {From, gen} -> 
            case is_solution_found() of
                false ->
                    case S of 
                        [] -> 
                            if 
                                (Initiator and not(Terminit)) -> 
                                    J = (I+1) rem ?M,
                                    sendRec(J, Nbrecdi),
                                    onReceive(I, Initiator, true, Terminatedi, 
                                             Nbrecdi, Nbsenti, S, T);
                                not(Initiator) -> 
                                    onReceive(I, Initiator, Terminit, Terminatedi, 
                                             Nbrecdi, Nbsenti, S, T);
                                Initiator and Terminit -> 
                                    J = (I+1) rem ?M,
                                    sendRec(J, Nbrecdi),   
                                    onReceive(I, Initiator, Terminit, Terminatedi, 
                                             Nbrecdi, Nbsenti, S, T)
                            end;
                        [Conf|Tl] -> 
                            Refvec = refvec(), 
                            case not(is_Terminal(Conf, Refvec)) of
                                true -> 
                                    {Rs1, Rs2} = split(Conf, Refvec),
                                    I1 = h(Rs1, ?M, Refvec),
                                    I2 = h(Rs2, ?M, Refvec),
                                    
                                    B1 = (countSetBits(second(Rs1, Refvec)) >= 
                                          math:sqrt(length(Refvec))),
                                    B2 = (countSetBits(second(Rs2, Refvec)) >= 
                                          math:sqrt(length(Refvec))),
                                    
                                    process_split(I, I1, I2, B1, B2, Rs1, Rs2, 
                                                 Refvec, Tl, T, Initiator, Terminit,
                                                 Terminatedi, Nbrecdi, Nbsenti);
                                false -> 
                                    Refvec2 = refvec(),
                                    Fst = library:first(Conf, Refvec2),
                                    case countSetBits(Fst) of
                                        ?N ->  % Solution dans traitement terminal
                                            ?CONTROLLER ! {found_solution, self(), Conf},
                                            ok;
                                        _ ->
                                            onReceive(I, Initiator, false, Terminatedi,
                                                     Nbrecdi, Nbsenti, Tl, [Conf|T])
                                    end
                            end
                    end;
                true ->
                    ok  % Solution déjà trouvée, s'arrêter
            end;
            
        {From, terminatedi} -> 
            Card = length(S),
            From ! {procName(I), {I, Initiator, Terminit, Terminatedi, Card, 
                                 Nbrecdi, Nbsenti, S, T}},
            onReceive(I, Initiator, Terminit, Terminatedi, Nbrecdi, Nbsenti, S, T);
        
        {rec, K} -> 
            case is_solution_found() of
                false ->
                    J = (I+1) rem ?M,
                    if 
                        not(Initiator) and (length(S) == 0) ->
                            sendRec(J, Nbrecdi + K),
                            onReceive(I, Initiator, Terminit, Terminatedi, 
                                     Nbrecdi, Nbsenti, S, T);
                        not(Initiator) and (length(S) /= 0) -> 
                            onReceive(I, Initiator, Terminit, Terminatedi, 
                                     Nbrecdi, Nbsenti, S, T);
                        Initiator and Terminit -> 
                            Totalrecd = K,
                            sendSnd(J, Nbsenti, Totalrecd),
                            onReceive(I, Initiator, Terminit, Terminatedi, 
                                     Nbrecdi, Nbsenti, S, T)
                    end;
                true -> ok
            end;
        
        {snd, K, Totalrecd} -> 
            case is_solution_found() of
                false ->
                    J = (I+1) rem ?M,
                    if 
                        not(Initiator) ->
                            Total = K + Nbsenti,
                            sendSnd(J, Total, Totalrecd),
                            onReceive(I, Initiator, Terminit, Terminatedi, 
                                     Nbrecdi, Nbsenti, S, T);
                        Initiator and (Terminit and (Totalrecd == K)) ->  
                            Nsol = length(T),
                            sendTerm(J, Nsol),
                            onReceive(I, Initiator, Terminit, true, 
                                     Nbrecdi, Nbsenti, S, T);
                        Initiator and ((not Terminit) or (Totalrecd /= K)) ->  
                            onReceive(I, Initiator, false, Terminatedi, 
                                     Nbrecdi, Nbsenti, S, T)
                    end;
                true -> ok
            end;
        
        {term, K} -> 
            case is_solution_found() of
                false ->
                    J = (I+1) rem ?M,
                    if 
                        not(Initiator) ->    
                            Nsol = length(T), 
                            Total = Nsol + K,
                            sendTerm(J, Total),
                            onReceive(I, Initiator, Terminit, true, 
                                     Nbrecdi, Nbsenti, S, T);
                        Initiator -> 
                            io:format("~nTotal des solutions trouvees: ~w~n", [K]), 
                            onReceive(I, Initiator, Terminit, Terminatedi, 
                                     Nbrecdi, Nbsenti, S, T)
                    end;
                true -> ok
            end;
        
        stop_immediate_solution ->  % Arrêt IMMÉDIAT quand solution trouvée
            io:format("Worker ~w: ARRET IMMEDIAT (solution trouvee)~n", [I]),
            ok;
            
        stop -> 
            io:format("Worker ~w arrete normalement~n", [I]),
            ok;
            
        Unexpected ->
            io:format("Worker ~w: message inattendu ~w~n", [I, Unexpected]),
            onReceive(I, Initiator, Terminit, Terminatedi, Nbrecdi, Nbsenti, S, T)
    end.

% Fonction auxiliaire pour traiter le split
process_split(I, I1, I2, B1, B2, Rs1, Rs2, Refvec, Tl, T, 
              Initiator, Terminit, Terminatedi, Nbrecdi, Nbsenti) ->
    if 
        (B1 and B2) and (I1 == I) and (I2 == I) -> 
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbsenti, 
                     [Rs2, Rs1 | Tl], T);
        (B1 and B2) and (I1 /= I) and (I2 == I) -> 
            sendConf(Rs1, I1),
            Nbs = Nbsenti + 1,
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbs, 
                     [Rs2 | Tl], T);
        (B1 and B2) and (I2 /= I) and (I1 == I) -> 
            sendConf(Rs2, I2),
            Nbs = Nbsenti + 1,
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbs, 
                     [Rs1 | Tl], T);
        (B1 and B2) and (I2 /= I) and (I1 /= I) -> 
            sendConf(Rs1, I1),
            sendConf(Rs2, I2),
            Nbs = Nbsenti + 2,
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbs, 
                     Tl, T);
        (not B1 and B2) and (I2 == I) -> 
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbsenti, 
                     [Rs2 | Tl], T);
        (not B1 and B2) and (I2 /= I) -> 
            sendConf(Rs2, I2),
            Nbs = Nbsenti + 1,
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbs, 
                     Tl, T);
        (B1 and not B2) and (I1 == I) -> 
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbsenti, 
                     [Rs1 | Tl], T);
        (B1 and not B2) and (I1 /= I) -> 
            sendConf(Rs1, I1),
            Nbs = Nbsenti + 1,
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbs, 
                     Tl, T);
        (not B1 and not B2) -> 
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbsenti, 
                     Tl, T)
    end.

% Afficher l'échiquier
display_chess_board(Positions) ->
    N = ?N,
    io:format("~n" ++ lists:duplicate(50, "-") ++ "~n"),
    io:format("ECHIQUIER DES 8 DAMES~n"),
    io:format("" ++ lists:duplicate(50, "-") ++ "~n~n"),
    
    io:format("  " ++ lists:concat(lists:duplicate(N*2 + 1, "-")) ++ "~n"),
    lists:foreach(
        fun(Row) ->
            io:format("~w |", [Row]),
            lists:foreach(
                fun(Col) ->
                    case lists:member({Row, Col}, Positions) of
                        true -> io:format(" ♕ ");
                        false -> io:format(" . ")
                    end
                end,
                lists:seq(1, N)
            ),
            io:format("|~n")
        end,
        lists:seq(1, N)
    ),
    io:format("  " ++ lists:concat(lists:duplicate(N*2 + 1, "-")) ++ "~n"),
    io:format("    " ++ string:join([integer_to_list(X) || X <- lists:seq(1, N)], " ") ++ "~n"),
    io:format("~n" ++ lists:duplicate(50, "=") ++ "~n~n").
