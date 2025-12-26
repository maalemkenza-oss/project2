-module(distributor).
-compile(export_all).
-import(lists,[map/2,sum/1,reverse/1,filter/2,append/2]).
-import(codinglist,[coding_list/1]).
-import(library,[split/2,is_Terminal/2,getInitialConf/1,displayOfConf/2,countSetBits/1,second/2,first/2,displayOnChess/2]).
-import(testhash,[h/3,allConfiguration/3,maph/3,numOfConfByMachines/3]).

-define(N,8).
-define(M,4).
-define(SOLUTION_FOUND, solution_found_flag).

workstation()-> [w0,w1,w2,w3,w4,w5,w6,w7,w8,w9].

indice(0, [H|_]) -> H;
indice(I, [_|T]) -> indice(I - 1, T).
procName(I)-> indice(I , workstation()).

refvec() -> coding_list(?N).

% --- INITIALISATION ---

start(I)-> 
    case whereis(?SOLUTION_FOUND) of
        undefined -> register(?SOLUTION_FOUND, spawn(fun() -> solution_flag(false) end));
        _ -> ok
    end,
    
    Refvec=refvec(), S0= getInitialConf(Refvec), I0=h(S0,?M,Refvec),
    if 
        I == I0 -> Pid= spawn(distributor, onReceive, [I,true,false,false,0,0,[S0],[],false]),
                   register(procName(I),Pid );
        I /=  I0 -> Pid=spawn(distributor,onReceive, [I,false,false,false,0,0,[],[],false]), 
                   register(procName(I),Pid )
    end.

% --- GESTIONNAIRE DE SOLUTION UNIQUE ---

solution_flag(Found) ->
    receive
        {set_solution, WorkerId, Conf, Refvec} -> 
            if 
                Found == false ->
                    % Affiche SEULEMENT la première solution reçue
                    io:format("~n======================================~n"),
                    io:format("*** SOLUTION TROUVEE (Worker ~w) ***~n", [WorkerId]),
                    Chess = displayOnChess(Conf, Refvec),
                    io:format("Positions: ~w~n", [Chess]),
                    io:format("======================================~n"),
                    broadcast_stop(),
                    solution_flag(true);
                true -> 
                    solution_flag(true)
            end;
        {get, From} -> 
            From ! {solution_flag, Found},
            solution_flag(Found);
        _ -> solution_flag(Found)
    end.

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

check_solution_found() ->
    case whereis(?SOLUTION_FOUND) of
        undefined -> false;
        Pid -> 
            Pid ! {get, self()},
            receive
                {solution_flag, Found} -> Found
            after 10 -> false
            end
    end.

% --- COMMANDES ---

startAll() -> startAl(?M).
startAl(0) -> true;
startAl(M) -> start(M-1), startAl(M-1).

generateAll() -> [spawn(distributor, generate, [I]) || I <- lists:seq(0, ?M-1)].

sendConf(Conf,I) -> 
    case check_solution_found() of
        false -> procName(I) ! {state,Conf};
        true -> ok 
    end.

sendStop(I)  -> procName(I) ! stop.

% --- LOGIQUE DES WORKERS ---

generate(I)-> 
    W=procName(I),
    case check_solution_found() of
        false ->
            W!{self(),terminatedi},
            receive 
                {W,{I,Initiator,Terminit,Terminatedi, Len,Nbrecdi,Nbsenti,S,T,_}} -> 
                    if 
                        (not Terminatedi) -> W ! {self(),gen}, generate(I);
                        (Terminatedi and (Len == 0)) -> sendStop(I);
                        true -> generate(I)
                    end
            after 1000 -> ok
            end;
        true -> ok
    end.

onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T,SF) -> 
    receive
        stop_now -> ok;
        stop -> ok;

        {state,Conf} -> 
            Refvec=refvec(),
            case is_Terminal(Conf,Refvec) of
                true ->
                    Fst = first(Conf,Refvec),
                    case countSetBits(Fst) of
                        ?N -> 
                            % ENVOI AU FLAG CENTRAL POUR AFFICHAGE UNIQUE
                            ?SOLUTION_FOUND ! {set_solution, I, Conf, Refvec},
                            ok; 
                        _ -> 
                            onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi+1,Nbsenti,S,[Conf|T],SF)
                    end;
                false ->
                    onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi+1,Nbsenti,[Conf|S],T,SF)
            end;

        {From, gen} ->
            case S of 
                [] -> 
                    if 
                        (Initiator) -> 
                            J= (I+1) rem ?M,
                            procName(J) ! {rec, Nbrecdi},
                            onReceive(I,Initiator,true,Terminatedi,Nbrecdi,Nbsenti,S,T,SF);
                        true -> 
                            onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T,SF)
                    end;
                [Conf|Tl] -> 
                    Refvec=refvec(),
                    case is_Terminal(Conf,Refvec) of
                        false ->
                            {Rs1,Rs2}= split(Conf,Refvec),
                            I1=h(Rs1,?M,Refvec),
                            I2=h(Rs2,?M,Refvec),
                            % Distribution simple
                            if I1 == I -> NewS = [Rs1|Tl], Ns = Nbsenti; true -> sendConf(Rs1,I1), NewS = Tl, Ns = Nbsenti+1 end,
                            if I2 == I -> FinalS = [Rs2|NewS], FinalNs = Ns; true -> sendConf(Rs2,I2), FinalS = NewS, FinalNs = Ns+1 end,
                            onReceive(I,Initiator,false,Terminatedi,Nbrecdi,FinalNs,FinalS,T,SF);
                        true ->
                            % Vérification si solution ici aussi
                            Fst = first(Conf,Refvec),
                            case countSetBits(Fst) of
                                ?N -> ?SOLUTION_FOUND ! {set_solution, I, Conf, Refvec}, ok;
                                _ -> onReceive(I,Initiator,false,Terminatedi,Nbrecdi,Nbsenti,Tl,[Conf|T],SF)
                            end
                    end
            end;

        {From,terminatedi} -> 
            From ! {procName(I),{I,Initiator,Terminit,Terminatedi,length(S),Nbrecdi,Nbsenti,S,T,SF}},
            onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T,SF);

        {rec,K} -> 
            J = (I+1) rem ?M,
            procName(J) ! {rec, Nbrecdi+K},
            onReceive(I,Initiator,Terminit,Terminatedi,Nbrecdi,Nbsenti,S,T,SF)

        % (Ajoutez ici les autres messages snd/term si nécessaire)
    end.

