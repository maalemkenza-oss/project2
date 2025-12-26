-module(testhash).
-export([h/3,allConfiguration/3,maph/3,numOfConfByMachines/3]).
-import(lists,[map/2,sum/1,reverse/1,filter/2,append/2]).
-import(sets,[from_list/1,to_list/1,subtract/2,add_element/2]).
-import(codinglist,[coding_list/1]).
-import(library,[second/2,split/2,is_Terminal/2,getInitialConf/1,displayOfConf/2,countSetBits/1,second/2]).


h(Conf,M,Refvec) -> Snd = second(Conf,Refvec),Ssnd=integer_to_list(Snd),N1=crypto:hash(md5, Ssnd),(binary:decode_unsigned(N1)) rem M .

allConfiguration(Refvec,S,All)->   
             case S of 
                [Conf|Tl]-> B=not(is_Terminal(Conf,Refvec)),B2= (countSetBits(second(Conf,Refvec)) >= math:sqrt(length(Refvec))),


            if 
               (B and B2) -> {Rs1,Rs2}=split(Conf,Refvec),
                                                 L=[Rs1,Rs2],S1=append(L,Tl),A1=append([Conf],All),allConfiguration(Refvec,S1,A1);
               (not(B)and B2) -> T1=append([Conf],All),allConfiguration(Refvec,Tl,T1);
                 not(B2) -> allConfiguration(Refvec,Tl,All)

   
            end;
             []-> All
  end.

foreach( [H|T],L,A) ->
    {Sat,NotSat}=lists:partition(fun(X) -> X  == H end,L),A1=A++[Sat],
    foreach( T,NotSat,A1);
foreach( [],_,A) -> A .

maph(M,Refvec,L)-> [h(C,M,Refvec) ||   C<-L].
numOfConfByMachines(M,Refvec,L)-> Sq = lists:seq(0,M-1),D = maph(M,Refvec,L),Li=foreach( Sq,D,[]), [io_lib:write(length(X)) || X <- Li].