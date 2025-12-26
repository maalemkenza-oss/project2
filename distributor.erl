-module(distributor).
-compile(export_all).
-import(lists,[map/2,sum/1,reverse/1,filter/2,append/2]).
-import(codinglist,[coding_list/1]).
-import(library,[split/2,is_Terminal/2,getInitialConf/1,displayOfConf/2,countSetBits/1,second/2]).
-import(testhash,[h/3,allConfiguration/3,maph/3,numOfConfByMachines/3]).
-define(N,8).
-define(M,10).

% Nouvelle variable globale pour indiquer si une solution a été trouvée
-define(SOLUTION_FOUND, solution_found_flag).

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

% MODIFICATION: Ajout d'un flag global pour solution trouvée
start(I)-> 
    % Initialiser le flag global
    case whereis(?SOLUTION_FOUND) of
        undefined -> register(?SOLUTION_FOUND, spawn(fun() -> solution_flag(false) end));
        _ -> ok
    end,
    
    Refvec=refvec(),S0= getInitialConf(Refvec), I0=h(S0,?M,Refvec),
    if 
        I == I0 -> Pid= spawn(distributor, onReceive, [I,true,false,false,0,0,[S0],[],false]),
                    register(procName(I),Pid );
        I /=  I0 -> Pid=spawn(distributor,onReceive, [I,false,false,false,0,0,[],[],false]), 
                    register(procName(I),Pid )
    end.

% Processus pour maintenir le flag de solution
solution_flag(Found) ->
    receive
        {set, true} -> 
            % Diffuser l'arrêt à tous les workers
            broadcast_stop(),
            solution_flag(true);
        {get, From} -> 
            From ! {solution_flag, Found},
            solution_flag(Found);
        _ -> solution_flag(Found)
    end.

% Fonction pour diffuser l'arrêt
broadcast_stop() ->
    lists:foreach(
        fun(I) -> 
            case whereis(procName(I)) of
                undefined -> ok;
                _ -> procName(I) ! stop_now
            end
        end,
        lists:seq(0, ?M-1)
    ).

% MODIFICATION: Vérifier si solution déjà trouvée
check_solution_found() ->
    ?SOLUTION_FOUND ! {get, self()},
    receive
        {solution_flag, Found} -> Found
    after 100 -> false
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
    % MODIFICATION: Vérifier avant d'envoyer
    case check_solution_found() of
        false -> procName(I) ! {state,Conf};
        true -> ok  % Ne pas envoyer si solution déjà trouvée
    end.

sendRec(I,K) -> procName(I) ! {rec,K}.
sendSnd(I,K,Totalrecd) -> procName(I) ! {snd ,K,Totalrecd}.
sendTerm(I,K) -> procName(I) ! {term,K}.
sendStop(I)  -> procName(I) ! stop.

% MODIFICATION: Vérifier solution avant de générer
generate(I)-> 
    W=procName(I),
    case check_solution_found() of
        false ->
            W!{self(),terminatedi},
            receive 
                {W,{I,Initiator,Terminit,Terminatedi, Len,Nbrecdi,Nbsenti,S,T,SolutionFound}} -> 
                    case check_solution_found() of
                        false ->
                            if 
                                ((not (Terminatedi)) and (Len /= 0))-> 
                                    io:format("length=~w S=~w T=~w ~n Nbrec=~w Nbsent=~w ~n",[Len,S,T,Nbrecdi,Nbsenti]),
                                    W !{self(),gen},
                                    generate(I);
                                ((not (Terminatedi)) and (Len == 0))-> 
                                    W !{self(),gen},
                                    io:format("I=~w Initiator=~w Terminit=~w length =~w S=~w T=~w Nbrec=~w Nbsent=~w ~n",
                                             [I,Initiator,Terminit,Len,S,T,Nbrecdi,Nbsenti]), 
                                    generate(I);
                                ((Terminatedi)and(Len == 0))->
                                    io:format("Distributed Termination Detection~n",[]) ,
                                    sendStop(I) 
                            end;
                        true ->
                            io:format("Solution deja trouvee, arret worker ~w~n", [I])
                    end
            end;
        true ->
            io:format("Solution deja trouvee, arret generation worker ~w~n", [I])
    end.

% MODIFICATION: Ajout du paramètre SolutionFound et logique d'arrêt précoce
onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T,SolutionFound) -> 
    receive
        {state,Conf} -> 
            % MODIFICATION: Vérifier si c'est une solution COMPLÈTE
            Refvec=refvec(),
            case is_Terminal(Conf,Refvec) of
                true ->
                    % Vérifier si c'est une solution complète (toutes les dames placées)
                    Fst = library:first(Conf,Refvec),
                    Snd = library:second(Conf,Refvec),
                    TotalPositions = length(Refvec),
                    
                   case countSetBits(Fst) of
    ?N ->  % N dames placées
        io:format("*** SOLUTION COMPLETE TROUVEE sur worker ~w! ***~n", [I]),
        io:format("Configuration: ~w~n", [Conf]),

        % Afficher la solution sur l'échiquier
        {_TreatedPos, _CliquePos} = displayOfConf(Conf,Refvec),
        ChessPositions = library:displayOnChess(Conf,Refvec),
        io:format("Positions des dames: ~w~n", [ChessPositions]),

        % Marquer la solution comme trouvée
        ?SOLUTION_FOUND ! {set, true},

        % Arrêter ce worker
        ok;
    _ ->
        % Configuration terminale mais pas complète
        Nbr = Nbrecdi + 1,
        onReceive(I,Initiator,Terminit,Terminatedi,Nbr,Nbsenti,S,[Conf|T],SolutionFound)
end;

                false ->
                    % Configuration non-terminale
                    Nbr=Nbrecdi+1,
                    onReceive(I,Initiator,Terminit,Terminatedi,Nbr,Nbsenti,[Conf|S],T,SolutionFound)
            end;
            
        {From,gen} -> 
            % MODIFICATION: Vérifier si solution déjà trouvée
            case check_solution_found() of
                false ->
                    case S of 
                        []-> 
                            if 
                                (Initiator and not(Terminit))-> 
                                    J= (I+1) rem ?M,
                                    sendRec(J,Nbrecdi),
                                    onReceive(I,Initiator,true,Terminatedi,Nbrecdi,Nbsenti,S,T,SolutionFound);
                                not(Initiator) -> 
                                    onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T,SolutionFound);
                                Initiator and Terminit -> 
                                    J= (I+1) rem ?M,
                                    sendRec(J,Nbrecdi),   
                                    onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T,SolutionFound)
                            end;
                        [Conf|Tl]-> 
                            Refvec=refvec(), 
                            B=not(is_Terminal(Conf,Refvec)),
                            
                            if 
                                B -> 
                                    {Rs1,Rs2}= split(Conf,Refvec),
                                    I1=h(Rs1,?M,Refvec),
                                    I2=h(Rs2,?M,Refvec),
                                    
                                    % MODIFICATION: Heuristique - explorer d'abord la branche la plus prometteuse
                                    % On priorise la branche avec le plus de bits à 1 dans la clique
                                    B1= (countSetBits(second(Rs1,Refvec)) >= math:sqrt(length(Refvec))),
                                    B2= (countSetBits(second(Rs2,Refvec)) >= math:sqrt(length(Refvec))),
                                    
                                    % MODIFICATION: Logique simplifiée pour arrêt rapide
                                    if 
                                        (B1 and B2) and (I1==I) and (I2==I) -> 
                                            % Explorer Rs1 d'abord (plus prometteuse)
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbsenti,[Rs2|[Rs1|Tl]],T,SolutionFound);
                                        (B1 and B2) and (I1/=I) and (I2==I) -> 
                                            sendConf(Rs1,I1),
                                            Nbs = Nbsenti+1,
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbs,[Rs2|Tl],T,SolutionFound);
                                        (B1 and B2) and (I2/=I) and (I1==I) -> 
                                            sendConf(Rs2,I2),
                                            Nbs = Nbsenti+1,
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbs,[Rs1|Tl],T,SolutionFound);
                                        (B1 and B2) and (I2/=I) and (I1/=I) -> 
                                            sendConf(Rs1,I1),
                                            sendConf(Rs2,I2),
                                            Nbs = Nbsenti+2,
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbs,Tl,T,SolutionFound);
                                        (not(B1) and B2) and (I2==I) -> 
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbsenti,[Rs2|Tl],T,SolutionFound);
                                        (not(B1) and B2) and (I2/=I) -> 
                                            sendConf(Rs2,I2),
                                            Nbs = Nbsenti+1,
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbs,Tl,T,SolutionFound);
                                        (B1 and not(B2)) and (I1==I) -> 
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbsenti,[Rs1|Tl],T,SolutionFound);
                                        (B1 and not(B2)) and (I1/=I) -> 
                                            sendConf(Rs1,I1),
                                            Nbs = Nbsenti+1,
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbs,Tl,T,SolutionFound);
                                        (not(B1) and not(B2)) -> 
                                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbsenti,Tl,T,SolutionFound)
                                    end;
                                (not(B)) -> 
                                    % Configuration terminale - vérifier si solution complète
                                    Fst = library:first(Conf,Refvec),
                                   case countSetBits(Fst) of
    ?N ->  % Solution complète!
        io:format("*** SOLUTION dans traitement terminal! ~w ***~n", [I]),
        ?SOLUTION_FOUND ! {set, true},
        ok;
    _ ->
        onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbsenti,Tl,[Conf|T],SolutionFound)
end

                            end
                    end;
                true ->
                    % Solution déjà trouvée, s'arrêter
                    ok
            end;
            
        {From,terminatedi} -> 
            Card=length(S),
            From ! {procName(I),{I,Initiator,Terminit,Terminatedi,Card,Nbrecdi,Nbsenti,S,T,SolutionFound}},
            onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T,SolutionFound);
        
        {rec,K} -> 
            case check_solution_found() of
                false ->
                    J= (I+1) rem ?M,
                    if 
                        not(Initiator) and (length(S)==0) ->
                            sendRec(J,Nbrecdi+K),
                            onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T,SolutionFound);
                        not(Initiator) and (length(S)/=0) -> 
                            onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T,SolutionFound);
                        Initiator and (Terminit) -> 
                            Totalrecd = K,
                            sendSnd(J,Nbsenti,Totalrecd),
                            onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T,SolutionFound)
                    end;
                true -> ok
            end;
        
        {snd,K,Totalrecd} -> 
            case check_solution_found() of
                false ->
                    J= (I+1) rem ?M,
                    if 
                        not(Initiator) ->
                            Total= K+ Nbsenti,
                            sendSnd(J,Total,Totalrecd),
                            onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T,SolutionFound);
                        Initiator and (Terminit and (Totalrecd ==K)) ->  
                            Nsol = length(T),
                            sendTerm(J,Nsol),
                            onReceive(I,Initiator,Terminit,true,Nbrecdi,Nbsenti,S,T,SolutionFound);
                        Initiator and ((not(Terminit)) or (not (Totalrecd ==K))) ->  
                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbsenti,S,T,SolutionFound)
                    end;
                true -> ok
            end;
        
        {term, K} -> 
            case check_solution_found() of
                false ->
                    J= (I+1) rem ?M,
                    if 
                        not(Initiator) ->    
                            Nsol= length(T), 
                            Total= Nsol+K,
                            sendTerm(J,Total),
                            onReceive(I,Initiator,Terminit,true,Nbrecdi,Nbsenti,S,T,SolutionFound);
                        Initiator -> 
                            io:format(" Total Solutions trouvees avant arret: ~w~n",[K]), 
                            onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T,SolutionFound)
                    end;
                true -> ok
            end;
        
        stop_now ->  % MODIFICATION: Nouveau message pour arrêt immédiat
            io:format("Worker ~w arrete (solution trouvee)~n", [I]),
            ok;
            
        stop -> 
            io:format("Worker ~w arrete normalement~n", [I]),
            ok           
    end.



                     



