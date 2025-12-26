-module(distributor).
-compile(export_all).
-import(lists,[map/2,sum/1,reverse/1,filter/2,append/2]).
-import(codinglist,[coding_list/1]).
-import(library,[split/2,is_Terminal/2,getInitialConf/1,
                 displayOfConf/2,countSetBits/1,second/2]).
-import(testhash,[h/3,allConfiguration/3,maph/3,numOfConfByMachines/3]).
-define(N,4).   % Changez à 8 pour le problème des 8-Dames
-define(M,10).

% Variable globale pour indiquer si une solution a été trouvée
-define(SOLUTION_FOUND, solution_found_flag).

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

start(I) -> 
    % Initialiser le flag global
    case whereis(?SOLUTION_FOUND) of
        undefined -> 
            Pid = spawn(fun() -> solution_flag(false) end),
            register(?SOLUTION_FOUND, Pid);
        _ -> ok
    end,
    
    Refvec = refvec(),
    S0 = getInitialConf(Refvec), 
    I0 = h(S0, ?M, Refvec),
    if 
        I == I0 -> 
            Pid = spawn(distributor, onReceive, 
                       [I, true, false, false, 0, 0, [S0], [], false]),
            register(procName(I), Pid);
        true -> 
            Pid = spawn(distributor, onReceive, 
                       [I, false, false, false, 0, 0, [], [], false]), 
            register(procName(I), Pid)
    end.

% Processus pour maintenir le flag de solution
solution_flag(Found) ->
    receive
        {set, true} -> 
            broadcast_stop(),
            solution_flag(true);
        {get, From} -> 
            From ! {solution_flag, Found},
            solution_flag(Found);
        _ -> solution_flag(Found)
    end.

% Diffuser l'arrêt à tous les workers
broadcast_stop() ->
    lists:foreach(
        fun(I) -> 
            Worker = procName(I),
            case whereis(Worker) of
                undefined -> ok;
                Pid -> 
                    try
                        Pid ! stop_solution_found
                    catch
                        _:_ -> ok
                    end
            end
        end,
        lists:seq(0, ?M-1)
    ).

% Vérifier si solution déjà trouvée
check_solution_found() ->
    case whereis(?SOLUTION_FOUND) of
        undefined -> false;
        Pid -> 
            try
                Pid ! {get, self()},
                receive
                    {solution_flag, Found} -> Found
                after 100 -> false
                end
            catch
                _:_ -> false
            end
    end.

startAll() -> startAl(?M).
startAl(0) -> true;
startAl(M) -> start(M-1), startAl(M-1).

gen(I) -> spawn(distributor, generate, [I]).

generateAll() -> generateAl(?M).
generateAl(0) -> true;
generateAl(M) -> gen(M-1), generateAl(M-1).

stopAll() -> stopAl(?M).
stopAl(0) -> true;
stopAl(M) -> sendStop(M-1), stopAl(M-1).

% Envoyer une configuration (avec vérification)
sendConf(Conf, I) -> 
    case check_solution_found() of
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

% Autres fonctions send protégées
sendRec(I, K) -> 
    case check_solution_found() of
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
    case check_solution_found() of
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
    case check_solution_found() of
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
    W = procName(I),
    case check_solution_found() of
        false ->
            case whereis(W) of
                undefined -> 
                    io:format("Worker ~w deja arrete~n", [I]);
                _ ->
                    W ! {self(), terminatedi},
                    receive 
                        {W, {I, Initiator, Terminit, Terminatedi, Len, 
                             Nbrecdi, Nbsenti, S, T, SolutionFound}} -> 
                            case check_solution_found() of
                                false ->
                                    if 
                                        ((not Terminatedi) and (Len /= 0)) -> 
                                            io:format("Worker ~w: travaille (~w configs)~n", 
                                                     [I, Len]),
                                            W ! {self(), gen},
                                            generate(I);
                                        ((not Terminatedi) and (Len == 0)) -> 
                                            W ! {self(), gen},
                                            io:format("Worker ~w: en attente~n", [I]), 
                                            generate(I);
                                        (Terminatedi and (Len == 0)) ->
                                            io:format("Worker ~w: terminaison~n", [I]),
                                            sendStop(I) 
                                    end;
                                true ->
                                    io:format("Worker ~w: solution trouvee~n", [I])
                            end
                    after 3000 ->
                        io:format("Worker ~w: timeout~n", [I])
                    end
            end;
        true ->
            io:format("Worker ~w: arret (solution deja trouvee)~n", [I])
    end.

% Processus principal du worker
onReceive(I, Initiator, Terminit, Terminatedi, Nbrecdi, Nbsenti, S, T, SolutionFound) -> 
    receive
        {state, Conf} -> 
            case check_solution_found() of
                false ->
                    Refvec = refvec(),
                    case is_Terminal(Conf, Refvec) of
                        true ->
                            Fst = library:first(Conf, Refvec),
                            case countSetBits(Fst) of
                                ?N ->  % SOLUTION TROUVEE!
                                    io:format("~n*** SOLUTION COMPLETE sur worker ~w! ***~n", [I]),
                                    display_solution(Conf, Refvec),
                                    ?SOLUTION_FOUND ! {set, true},
                                    ok;
                                _ ->
                                    Nbr = Nbrecdi + 1,
                                    onReceive(I, Initiator, Terminit, Terminatedi, 
                                             Nbr, Nbsenti, S, [Conf|T], SolutionFound)
                            end;
                        false ->
                            Nbr = Nbrecdi + 1,
                            onReceive(I, Initiator, Terminit, Terminatedi, 
                                     Nbr, Nbsenti, [Conf|S], T, SolutionFound)
                    end;
                true -> ok
            end;
            
        {From, gen} -> 
            case check_solution_found() of
                false ->
                    case S of 
                        [] -> 
                            if 
                                (Initiator and not(Terminit)) -> 
                                    J = (I+1) rem ?M,
                                    sendRec(J, Nbrecdi),
                                    onReceive(I, Initiator, true, Terminatedi, 
                                             Nbrecdi, Nbsenti, S, T, SolutionFound);
                                not(Initiator) -> 
                                    onReceive(I, Initiator, Terminit, Terminatedi, 
                                             Nbrecdi, Nbsenti, S, T, SolutionFound);
                                Initiator and Terminit -> 
                                    J = (I+1) rem ?M,
                                    sendRec(J, Nbrecdi),   
                                    onReceive(I, Initiator, Terminit, Terminatedi, 
                                             Nbrecdi, Nbsenti, S, T, SolutionFound)
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
                                                 Terminatedi, Nbrecdi, Nbsenti,
                                                 SolutionFound);
                                false -> 
                                    Refvec2 = refvec(),
                                    Fst = library:first(Conf, Refvec2),
                                    case countSetBits(Fst) of
                                        ?N ->  % Solution dans traitement terminal
                                            io:format("~n*** SOLUTION TERMINALE sur worker ~w! ***~n", [I]),
                                            display_solution(Conf, Refvec2),
                                            ?SOLUTION_FOUND ! {set, true},
                                            ok;
                                        _ ->
                                            onReceive(I, Initiator, false, Terminatedi,
                                                     Nbrecdi, Nbsenti, Tl, [Conf|T], 
                                                     SolutionFound)
                                    end
                            end
                    end;
                true -> ok
            end;
            
        {From, terminatedi} -> 
            Card = length(S),
            From ! {procName(I), {I, Initiator, Terminit, Terminatedi, Card, 
                                 Nbrecdi, Nbsenti, S, T, SolutionFound}},
            onReceive(I, Initiator, Terminit, Terminatedi, Nbrecdi, Nbsenti, 
                     S, T, SolutionFound);
        
        {rec, K} -> 
            case check_solution_found() of
                false ->
                    J = (I+1) rem ?M,
                    if 
                        not(Initiator) and (length(S) == 0) ->
                            sendRec(J, Nbrecdi + K),
                            onReceive(I, Initiator, Terminit, Terminatedi, 
                                     Nbrecdi, Nbsenti, S, T, SolutionFound);
                        not(Initiator) and (length(S) /= 0) -> 
                            onReceive(I, Initiator, Terminit, Terminatedi, 
                                     Nbrecdi, Nbsenti, S, T, SolutionFound);
                        Initiator and Terminit -> 
                            Totalrecd = K,
                            sendSnd(J, Nbsenti, Totalrecd),
                            onReceive(I, Initiator, Terminit, Terminatedi, 
                                     Nbrecdi, Nbsenti, S, T, SolutionFound)
                    end;
                true -> ok
            end;
        
        {snd, K, Totalrecd} -> 
            case check_solution_found() of
                false ->
                    J = (I+1) rem ?M,
                    if 
                        not(Initiator) ->
                            Total = K + Nbsenti,
                            sendSnd(J, Total, Totalrecd),
                            onReceive(I, Initiator, Terminit, Terminatedi, 
                                     Nbrecdi, Nbsenti, S, T, SolutionFound);
                        Initiator and (Terminit and (Totalrecd == K)) ->  
                            Nsol = length(T),
                            sendTerm(J, Nsol),
                            onReceive(I, Initiator, Terminit, true, 
                                     Nbrecdi, Nbsenti, S, T, SolutionFound);
                        Initiator and ((not Terminit) or (Totalrecd /= K)) ->  
                            onReceive(I, Initiator, false, Terminatedi, 
                                     Nbrecdi, Nbsenti, S, T, SolutionFound)
                    end;
                true -> ok
            end;
        
        {term, K} -> 
            case check_solution_found() of
                false ->
                    J = (I+1) rem ?M,
                    if 
                        not(Initiator) ->    
                            Nsol = length(T), 
                            Total = Nsol + K,
                            sendTerm(J, Total),
                            onReceive(I, Initiator, Terminit, true, 
                                     Nbrecdi, Nbsenti, S, T, SolutionFound);
                        Initiator -> 
                            io:format("~nConfigurations terminales: ~w~n", [K]), 
                            onReceive(I, Initiator, Terminit, Terminatedi, 
                                     Nbrecdi, Nbsenti, S, T, SolutionFound)
                    end;
                true -> ok
            end;
        
        stop_solution_found -> 
            io:format("Worker ~w arrete (solution trouvee)~n", [I]),
            ok;
            
        stop -> 
            io:format("Worker ~w arrete normalement~n", [I]),
            ok;
            
        Unexpected ->
            io:format("Worker ~w: message inattendu ~w~n", [I, Unexpected]),
            onReceive(I, Initiator, Terminit, Terminatedi, Nbrecdi, Nbsenti, 
                     S, T, SolutionFound)
    end.

% Fonction auxiliaire pour traiter le split
process_split(I, I1, I2, B1, B2, Rs1, Rs2, Refvec, Tl, T, 
              Initiator, Terminit, Terminatedi, Nbrecdi, Nbsenti, SolutionFound) ->
    if 
        (B1 and B2) and (I1 == I) and (I2 == I) -> 
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbsenti, 
                     [Rs2, Rs1 | Tl], T, SolutionFound);
        (B1 and B2) and (I1 /= I) and (I2 == I) -> 
            sendConf(Rs1, I1),
            Nbs = Nbsenti + 1,
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbs, 
                     [Rs2 | Tl], T, SolutionFound);
        (B1 and B2) and (I2 /= I) and (I1 == I) -> 
            sendConf(Rs2, I2),
            Nbs = Nbsenti + 1,
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbs, 
                     [Rs1 | Tl], T, SolutionFound);
        (B1 and B2) and (I2 /= I) and (I1 /= I) -> 
            sendConf(Rs1, I1),
            sendConf(Rs2, I2),
            Nbs = Nbsenti + 2,
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbs, 
                     Tl, T, SolutionFound);
        (not B1 and B2) and (I2 == I) -> 
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbsenti, 
                     [Rs2 | Tl], T, SolutionFound);
        (not B1 and B2) and (I2 /= I) -> 
            sendConf(Rs2, I2),
            Nbs = Nbsenti + 1,
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbs, 
                     Tl, T, SolutionFound);
        (B1 and not B2) and (I1 == I) -> 
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbsenti, 
                     [Rs1 | Tl], T, SolutionFound);
        (B1 and not B2) and (I1 /= I) -> 
            sendConf(Rs1, I1),
            Nbs = Nbsenti + 1,
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbs, 
                     Tl, T, SolutionFound);
        (not B1 and not B2) -> 
            onReceive(I, Initiator, false, Terminatedi, Nbrecdi, Nbsenti, 
                     Tl, T, SolutionFound)
    end.

% Afficher une solution
display_solution(Conf, Refvec) ->
    ChessPositions = library:displayOnChess(Conf, Refvec),
    io:format("Positions des dames: ~w~n", [ChessPositions]),
    display_chess_board(ChessPositions).

% Afficher l'échiquier visuellement
display_chess_board(Positions) ->
    N = ?N,
    io:format("~nEchiquier ~wx~w:~n", [N, N]),
    io:format("  " ++ lists:concat(lists:duplicate(N*2 + 1, "-")) ++ "~n"),
    lists:foreach(
        fun(Row) ->
            io:format("~w |", [Row]),
            lists:foreach(
                fun(Col) ->
                    case lists:member({Row, Col}, Positions) of
                        true -> io:format(" Q ");
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
    io:format("    " ++ string:join([integer_to_list(X) || X <- lists:seq(1, N)], " ") ++ "~n~n").
