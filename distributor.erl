-module(distributor).
-compile(export_all).
-import(lists,[map/2,sum/1,reverse/1,filter/2,append/2]).
-import(codinglist,[coding_list/1]).
-import(library,[split/2,is_Terminal/2,getInitialConf/1,displayOfConf/2,countSetBits/1,second/2]).
-import(testhash,[h/3,allConfiguration/3,maph/3,numOfConfByMachines/3]).
-define(N,8).  % CHANGÉ: 4 → 8
-define(M,10).

% Variable globale pour indiquer si une solution a été trouvée
-define(SOLUTION_FOUND_FLAG, solution_found).

workstation()-> [w0,w1,w2,w3,w4,w5,w6,w7,w8,w9].

indice(0, [H|_]) ->
                      H;
indice(I, [_|T]) ->
                indice(I - 1, T).
procName(I)-> indice(I , workstation()).

refvec() -> coding_list(?N).

initiator(I) -> Refvec=refvec(),S0= getInitialConf(Refvec), I0=h(S0,?M,Refvec),
                if 
                      I == I0 -> true;
                      I/=I0 -> false
                end. 

% Initialiser le flag de solution
start(I)-> 
    % Initialiser le flag global si ce n'est pas déjà fait
    case whereis(?SOLUTION_FOUND_FLAG) of
        undefined -> 
            Pid = spawn(fun() -> 
                receive 
                    {set_found} -> 
                        % Diffuser l'arrêt à tous les workers
                        broadcast_stop(),
                        solution_flag(true);
                    {get_flag, From} -> 
                        From ! {flag_status, false},
                        receive 
                            {set_found} -> 
                                broadcast_stop(),
                                solution_flag(true)
                        end
                end 
            end),
            register(?SOLUTION_FOUND_FLAG, Pid);
        _ -> ok
    end,
    
    Refvec=refvec(),S0= getInitialConf(Refvec), I0=h(S0,?M,Refvec),
    if 
        I == I0 -> Pid= spawn(distributor, onReceive, [I,true,false,false,0,0,[S0],[]]),
                       register(procName(I),Pid );
        I /=  I0 -> Pid=spawn(distributor,onReceive, [I,false,false,false,0,0,[],[]]), register(procName(I),Pid )
    end.

% Fonction pour diffuser l'arrêt à tous les workers
broadcast_stop() ->
    [procName(I) ! stop_solution_found || I <- lists:seq(0, ?M-1)].

% Vérifier si une solution a déjà été trouvée
is_solution_found() ->
    case whereis(?SOLUTION_FOUND_FLAG) of
        undefined -> false;
        Pid -> 
            Pid ! {get_flag, self()},
            receive
                {flag_status, Status} -> Status
            after 100 -> false
            end
    end.

% Marquer qu'une solution a été trouvée
mark_solution_found() ->
    case whereis(?SOLUTION_FOUND_FLAG) of
        undefined -> ok;
        Pid -> Pid ! {set_found}
    end.

startAll()-> startAl(?M).
startAl(0)-> true;
startAl(M) -> start(M-1),startAl(M-1).

gen(I)-> Pid = spawn(distributor, generate, [I]),Pid.

generateAll() -> generateAl(?M).
generateAl(0)->  true;
generateAl(M)-> gen(M-1),generateAl(M-1).


stopAll()-> stopAl(?M).
stopAl(0)->true;
stopAl(M)-> sendStop(M-1),stopAl(M-1).



sendConf(Conf,I) -> 
    % Ne pas envoyer si solution déjà trouvée
    case is_solution_found() of
        false -> procName(I) ! {state,Conf};
        true -> ok
    end.

sendRec(I,K) -> 
    case is_solution_found() of
        false -> procName(I) ! {rec,K};
        true -> ok
    end.

sendSnd(I,K,Totalrecd) -> 
    case is_solution_found() of
        false -> procName(I) ! {snd ,K,Totalrecd};
        true -> ok
    end.

sendTerm(I,K) -> 
    case is_solution_found() of
        false -> procName(I) ! {term,K};
        true -> ok
    end.
    
sendStop(I)  -> procName(I) ! stop.


generate(I)-> W=procName(I),
             W!{self(),terminatedi},
  receive 
        {W,{I,Initiator,Terminit,Terminatedi, Len,Nbrecdi,Nbsenti,S,T}} -> 
            % Vérifier si solution déjà trouvée
            case is_solution_found() of
                false ->
                    if 
                        ((not (Terminatedi)) and (Len /= 0))-> 
                            io:format("Worker ~w: length=~w, recus=~w, envoyes=~w~n", 
                                     [I,Len,Nbrecdi,Nbsenti]),
                            W !{self(),gen},
                            generate(I);
                        ((not (Terminatedi)) and (Len == 0))-> 
                            W !{self(),gen},
                            io:format("Worker ~w: pile vide, attente~n", [I]), 
                            generate(I);
                        ((Terminatedi)and(Len == 0))->
                            io:format("Detection de terminaison distribuee~n",[]),
                            sendStop(I) 
                    end;
                true ->
                    io:format("Worker ~w arrete (solution trouvee)~n", [I]),
                    ok
            end
   end.

onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T) -> 
    receive
        {state,Conf} -> 
            % Vérifier d'abord si solution déjà trouvée
            case is_solution_found() of
                false ->
                    Nbr=Nbrecdi+1,
                    Refvec=refvec(),
                    B=not(is_Terminal(Conf,Refvec)),
                    
                    % Vérifier si c'est une solution complète
                    if 
                        B ->  
                            onReceive(I,Initiator,Terminit,Terminatedi,Nbr,Nbsenti,[Conf|S],T);
                        not(B) -> 
                            % Configuration terminale - vérifier si c'est une solution complète
                            Fst = library:first(Conf,Refvec),
                            if 
                                % Si toutes les N dames sont placées (8 pour N=8)
                                countSetBits(Fst) == ?N -> 
                                    io:format("~n*** SOLUTION COMPLETE TROUVEE sur worker ~w! ***~n", [I]),
                                    % Afficher les positions
                                    ChessPositions = library:displayOnChess(Conf,Refvec),
                                    io:format("Positions des dames: ~w~n", [ChessPositions]),
                                    % Afficher l'échiquier
                                    display_chess_board(ChessPositions),
                                    % Marquer que solution trouvée
                                    mark_solution_found(),
                                    ok;
                                true -> 
                                    % Configuration terminale mais pas complète
                                    onReceive(I,Initiator,Terminit,Terminatedi,Nbr,Nbsenti,S,[Conf|T])
                            end
                    end;
                true ->
                    % Solution déjà trouvée, ignorer
                    ok
            end;
            
        {From,gen} -> 
            % Vérifier si solution déjà trouvée
            case is_solution_found() of
                false ->
                    case S of 
                        []-> 
                            if 
                                (Initiator and not(Terminit))-> 
                                    J= (I+1) rem ?M,
                                    sendRec(J,Nbrecdi),
                                    onReceive(I,Initiator,true,Terminatedi,Nbrecdi,Nbsenti,S,T);
                                not(Initiator) -> 
                                    onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T);
                                Initiator and Terminit -> 
                                    J= (I+1) rem ?M,
                                    sendRec(J,Nbrecdi),   
                                    onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T)
                            end;
                        [Conf|Tl]-> 
                            Refvec=refvec(), 
                            B=not(is_Terminal(Conf,Refvec)),
                            
                            if 
                                B -> 
                                    {Rs1,Rs2}= split(Conf,Refvec),
                                    I1=h(Rs1,?M,Refvec),
                                    I2=h(Rs2,?M,Refvec),
                                    io:format("Worker ~w: split -> vers workers ~w et ~w~n", [I, I1, I2]),
                                    % OPTIMISATION POUR N=8: seuil plus élevé
                                    B1= (countSetBits(second(Rs1,Refvec)) >= 8),  % Au moins 8 candidats
                                    B2= (countSetBits(second(Rs2,Refvec)) >= 8),
                                    
                                    if 
                                        (B1 and B2) and (I1==I) and (I2==I) -> 
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbsenti,[Rs2|[Rs1|Tl]],T);
                                        (B1 and B2) and (I1/=I) and (I2==I) -> 
                                            sendConf(Rs1,I1),
                                            Nbs = Nbsenti+1,
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbs,[Rs2|Tl],T);
                                        (B1 and B2) and (I2/=I) and (I1==I) -> 
                                            sendConf(Rs2,I2),
                                            Nbs = Nbsenti+1,
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbs,[Rs1|Tl],T);
                                        (B1 and B2) and (I2/=I) and (I1/=I) -> 
                                            sendConf(Rs1,I1),
                                            sendConf(Rs2,I2),
                                            Nbs = Nbsenti+2,
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbs,Tl,T);
                                        (not(B1) and B2) and (I2==I) -> 
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbsenti,[Rs2|Tl],T);
                                        (not(B1) and B2) and (I2/=I) -> 
                                            sendConf(Rs2,I2),
                                            Nbs = Nbsenti+1,
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbs,Tl,T);
                                        (B1 and not(B2)) and (I1==I) -> 
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbsenti,[Rs1|Tl],T);
                                        (B1 and not(B2)) and (I1/=I) -> 
                                            sendConf(Rs1,I1),
                                            Nbs = Nbsenti+1,
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbs,Tl,T);
                                        (not(B1) and not(B2)) -> 
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbsenti,Tl,T)
                                    end;
                                (not(B)) -> 
                                    % Vérifier si solution complète dans le traitement terminal
                                    Refvec2=refvec(),
                                    Fst = library:first(Conf,Refvec2),
                                    if 
                                        countSetBits(Fst) == ?N -> 
                                            io:format("~n*** SOLUTION dans traitement terminal! Worker ~w ***~n", [I]),
                                            ChessPositions = library:displayOnChess(Conf,Refvec2),
                                            io:format("Positions des dames: ~w~n", [ChessPositions]),
                                            display_chess_board(ChessPositions),
                                            mark_solution_found(),
                                            ok;
                                        true ->
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbsenti,Tl,[Conf|T])
                                    end
                            end
                    end;
                true ->
                    % Solution déjà trouvée, s'arrêter
                    ok
            end;
            
        {From,terminatedi} -> 
            Card=length(S),
            From ! {procName(I),{I,Initiator,Terminit,Terminatedi,Card,Nbrecdi,Nbsenti,S,T}},
            onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T);
        
        {rec,K} -> 
            case is_solution_found() of
                false ->
                    J= (I+1) rem ?M,
                    if 
                        not(Initiator) and (length(S)==0) ->
                            sendRec(J,Nbrecdi+K),
                            onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T);
                        not(Initiator) and (length(S)/=0) -> 
                            onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T);
                        Initiator and (Terminit) -> 
                            Totalrecd = K,
                            sendSnd(J,Nbsenti,Totalrecd),
                            onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T)
                    end;
                true -> ok
            end;
        
        {snd,K,Totalrecd} -> 
            case is_solution_found() of
                false ->
                    J= (I+1) rem ?M,
                    if 
                        not(Initiator) ->
                            Total= K+ Nbsenti,
                            sendSnd(J,Total,Totalrecd),
                            onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T);
                        Initiator and (Terminit and (Totalrecd ==K)) ->  
                            Nsol = length(T),
                            sendTerm(J,Nsol),
                            onReceive(I,Initiator,Terminit,true,Nbrecdi,Nbsenti,S,T);
                        Initiator and ((not(Terminit)) or (not (Totalrecd ==K))) ->  
                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbsenti,S,T)
                    end;
                true -> ok
            end;
        
        {term, K} -> 
            case is_solution_found() of
                false ->
                    J= (I+1) rem ?M,
                    if 
                        not(Initiator) ->    
                            Nsol= length(T), 
                            Total= Nsol+K,
                            sendTerm(J,Total),
                            onReceive(I,Initiator,Terminit,true,Nbrecdi,Nbsenti,S,T);
                        Initiator -> 
                            io:format("~nTotal des configurations terminales avant arret: ~w~n",[K]), 
                            onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T)
                    end;
                true -> ok
            end;
        
        % Nouveau message pour arrêt immédiat quand solution trouvée
        stop_solution_found ->
            io:format("Worker ~w arrete (solution trouvee)~n", [I]),
            ok;
            
        stop -> 
            io:format("Worker ~w arrete normalement~n", [I]),
            ok           
    end.

% Processus auxiliaire pour le flag de solution
solution_flag(Found) ->
    receive
        {set_found} -> 
            broadcast_stop(),
            solution_flag(true);
        {get_flag, From} -> 
            From ! {flag_status, Found},
            solution_flag(Found)
    end.

% Fonction pour afficher l'échiquier
display_chess_board(Positions) ->
    io:format("~nRepresentation de l'echiquier 8x8:~n"),
    io:format("  " ++ lists:concat(lists:duplicate(17, "-")) ++ "~n"),
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
                lists:seq(1, 8)
            ),
            io:format("|~n")
        end,
        lists:seq(1, 8)
    ),
    io:format("  " ++ lists:concat(lists:duplicate(17, "-")) ++ "~n"),
    io:format("    1 2 3 4 5 6 7 8~n~n").
