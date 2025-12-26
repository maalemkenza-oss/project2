-module(seqalgo).
-export([generate/3]).
-import(lists,[map/2,sum/1,reverse/1,filter/2,append/2]).
-import(sets,[from_list/1,to_list/1,subtract/2,add_element/2]).
-import(codinglist,[coding_list/1]).
-import(library,[split/2,is_Terminal/2,getInitialConf/1,displayOfConf/2,countSetBits/1,second/2]).
 
generate(Refvec, S) ->
    case S of 
        [Conf|Tl]-> 
            B = not(is_Terminal(Conf, Refvec)),
            B2 = (countSetBits(second(Conf, Refvec)) >= math:sqrt(length(Refvec))),
            
            if 
                (B and B2) -> 
                    {Rs1, Rs2} = split(Conf, Refvec),
                    % Exploration en profondeur d'abord: essayer Rs1
                    case generate(Refvec, [Rs1|Tl]) of
                        none -> 
                            % Si Rs1 ne donne pas de solution, essayer Rs2
                            generate_one_helper(Refvec, [Rs2|Tl]);
                        Solution -> 
                            Solution  % Solution trouvée, la retourner
                    end;
                    
                (not(B) and B2) -> 
                    % Configuration terminale valide = SOLUTION TROUVÉE
                    Conf;  % Retourner immédiatement
                    
                not(B2) -> 
                    % Configuration non valide, continuer
                    generate_one_helper(Refvec, Tl)
            end;
        []-> 
            none  % Pas de solution dans cette branche
    end.
