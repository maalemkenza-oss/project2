-module(library).
-export([symboliclique/2,ensclique/2,inttovect/2,countSetBits/1,symbolictreated/2,enstreated/2,makeConfiguration/3]).
-export([split_Conf/2,first/2,second/2,is_Terminal/2]).
-export([choose/3,vecttoint/1,split/2,displayOfConf/2,getInitialConf/1,displayOnChess/2]).
-import(lists,[map/2,sum/1,reverse/1,filter/2,append/2]).
-import(sets,[from_list/1,to_list/1,subtract/2,add_element/2]).
-import(codinglist,[coding_list/1]).


index(E,L)-> index2(E,L,0).
index2(E,L,N)->
  case L of 
      [E|_] -> N ;
      [_|T]-> index2(E,T,N+1)
 end.
puiss(0)-> 1;
puiss(N) -> 2*puiss(N-1).

intofindex(I)-> puiss(I).

symboliclique(S,Refvec)-> L=to_list(S),sum([intofindex(index(E,Refvec))|| E<-L]).

select([]) -> [];
select([{0,_}|T])-> select(T);
select([{1,E}|T])-> [E|select(T)].   
     


ensclique(C,Refvec)-> V=lists:reverse(inttovect(C,length(Refvec))),Z=lists:zip(V,Refvec),from_list(select(Z)). 
  
vecttoint(V)-> vecttoint2(V,0).
vecttoint2(V,N)-> if 
                   (N==0) -> V1=reverse(V),V1;
                    (N/=0)  -> V1=V,V1
                  end,
case V1 of 
    [1|T]->puiss(N)+ vecttoint2(T,N+1);
    [0|T]-> vecttoint2(T,N+1);
    []-> 0
end.      

inttovect(N,Size) -> [X || <<X:1>> <= <<N:Size>> ].

countSetBits(N)-> V= [X || <<X:1>> <= binary:encode_unsigned(N) ],sum(V).

symbolictreated(T,Refvec)->symboliclique(T,Refvec).

enstreated(Ts,Refvec)-> ensclique(Ts,Refvec).  

makeConfiguration(Ts,C,Refvec) -> Size = length(Refvec),BTs= << Ts:Size >>,BC= <<C:Size >>,<< BTs/bitstring,BC/bitstring >>.

split_Conf(Conf,Refvec) -> Size = length(Refvec), <<Fst:Size,Snd:Size>> = Conf,
                            {Fst,Snd}.
 first(Conf ,Refvec) -> {Fst,_}=split_Conf(Conf,Refvec),Fst.
 second(Conf ,Refvec) -> {_,Snd}=split_Conf(Conf,Refvec),Snd.

is_Terminal(Conf,Refvec) -> Fst= first(Conf ,Refvec), Snd= second(Conf ,Refvec),Guard=((Snd band Fst) /= Snd), not(Guard).

choose(Fst,Snd,Refvec)->  V= << <<bnot(X):1>> || <<X:1>> <= <<(Fst band Snd):(length(Refvec))>> >>,
                         V1=[X || <<X:1>> <= V ], 
                         N1= vecttoint(V1),  EL=(N1 band Snd),
                         Ens=ensclique(EL,Refvec),hd(to_list(Ens)).

split(Conf , Refvec)  -> Treated=first(Conf ,Refvec),Clique= second(Conf ,Refvec),E=choose(Treated,Clique,Refvec),io:format("e=~w~n",[E]),
                       R=ensclique(Clique,Refvec),S=to_list(R),Z=filter(fun(X)-> X band E==0 end,S),
                        
                       Zero=from_list(append([E],Z)),One=subtract(R,from_list([E])),
                       Mate= symboliclique(Zero,Refvec),Rival=symboliclique(One,Refvec),
                       Ts= symbolictreated(add_element(E,enstreated(Treated,Refvec)),Refvec),
                       Rs1= makeConfiguration(Ts,Mate,Refvec), Rs2= makeConfiguration(Ts,Rival,Refvec),
                       {Rs1,Rs2}.
displayOfConf(Conf,Refvec)-> Fst= first(Conf ,Refvec), Snd= second(Conf ,Refvec),
                             Treated=to_list(ensclique(Fst,Refvec)),
                             Clique= to_list(ensclique(Snd,Refvec)),
                             {Treated,Clique}.
getInitialConf(Refvec)-> makeConfiguration(symboliclique(from_list([]),Refvec),symboliclique(from_list(Refvec),Refvec),Refvec).

displayOnChess(Conf,Refvec) -> {_,Clique}= displayOfConf(Conf,Refvec),Chess=[index(E,Refvec)|| E<-Clique],Chess.