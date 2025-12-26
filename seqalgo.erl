-module(seqalgo).
-export([generate/3]).
-import(lists,[map/2,sum/1,reverse/1,filter/2,append/2]).
-import(sets,[from_list/1,to_list/1,subtract/2,add_element/2]).
-import(codinglist,[coding_list/1]).
-import(library,[split/2,is_Terminal/2,getInitialConf/1,displayOfConf/2,countSetBits/1,second/2]).
generate(Refvec,S,T)->   
             case S of 
                [Conf|Tl]-> B=not(is_Terminal(Conf,Refvec)),B2= (countSetBits(second(Conf,Refvec)) >= math:sqrt(length(Refvec))),


            if 
               (B and B2) -> {Rs1,Rs2}=split(Conf,Refvec),
                                                 L=[Rs1,Rs2],S1=append(L,Tl),generate(Refvec,S1,T);
               (not(B)and B2) -> T1=append([Conf],T),generate(Refvec,Tl,T1);
                 not(B2) -> generate(Refvec,Tl,T)

   
            end;
             []-> T
  end.