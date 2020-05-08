stack ls dependencies tree --prune base,ghc-prim,integer-gmp,deepseq,array,time,template-haskell,filepath,directory,process,transformers,unix,containers,text,hashable,unordered-containers,bytestring,mtl,binary,stm

stack dot --external --prune base,ghc-prim,integer-gmp,deepseq,array,time,template-haskell,filepath,directory,process,transformers,unix,containers,text,hashable,unordered-containers,bytestring,mtl,binary,stm | dot -Tjpg -o deps.jpg
