# dependency-collapser

Collapses syntactic dependencies in original typed Stanford dependency format (not universal dependencies).


## Overview

This code is a reimplementation of [the JoBimText project for collapsing dependencies](http://ltmaggie.informatik.uni-hamburg.de/jobimtext/components/dependency-collapsing/) but with the following changes:

- UIMA dependencies (in the software sense) are removed
- Code is F#, not Java
- Code has been simplified/refactored to some extent

More significant refactoring is likely in the future once benchmarking on JoBimText demonstrates the current project has identical behavior.

The collapser takes tokens in a quasi Conll format.
For example, `They sit in the car.` would be represented by this list of tokens:

```
[
    Rules.Token.Create(0,"They","PRP","nsubj",2)
    Rules.Token.Create(1,"sit","VBP","root",0)
    Rules.Token.Create(2,"in","IN","prep",2)
    Rules.Token.Create(3,"the","DT","det",5)
    Rules.Token.Create(4,"car","NN","pobj",3)
    Rules.Token.Create(5,".",".","punct",2)
]
```

The first element in the list, `They` is a personal pronoun (`PRP` in the Penn Treebank tagset) and has relation nominal subject (`nsubj`) to its head, the second token `sit`. 
Note that head indices are 1-based not 0-based, as is common.

For more information on the relations, as well as what the collapser is doing, [see the Stanford Dependencies documentation](https://nlp.stanford.edu/software/stanford-dependencies.shtml). 
In their terminology, we are collapsing and propogating dependencies (i.e., with cc-coordination).

The code includes an evaluator that uses [a .NET wrapper for the Stanford Parser](https://sergey-tihon.github.io/Stanford.NLP.NET/).
It is a bit tricky to install; you also have to download the parser distribution (matching the version number) and unzip its model jar.
This is only necessary if you want to run an evaluation comparing performance of the collapser to Stanford's native collapser.
Additional code allows for comparison with AllenNLP.

## Building

- [dotnet core sdk 3+](https://docs.microsoft.com/en-us/dotnet/core/install/linux-package-manager-ubuntu-1804)
- [paket](https://fsprojects.github.io/Paket/installation.html)
- `dotnet paket install`
- Build in the IDE of your choice
