module StanfordParser

open java.util
open edu.stanford.nlp.ling
open edu.stanford.nlp.trees
open edu.stanford.nlp.parser.lexparser
open edu.stanford.nlp.util
open edu.stanford.nlp.semgraph

// Path to models extracted from `stanford-parser-3.8.0-models.jar`
let modelsDirectry =
    __SOURCE_DIRECTORY__
    + @"/../stanford-corenlp-full-2017-06-09/models/"
    + @"edu/stanford/nlp/models/"

// Loading english PCFG parser from file
//let options = [| "-outputFormatOptions"; "includePunctuationDependencies" |] //to keep punctuation, but has no effect
let lp =  LexicalizedParser.loadModel( modelsDirectry + @"lexparser/englishPCFG.ser.gz") //, options)
//lp.setOptionFlags( [| "-outputFormatOptions"; "includePunctuationDependencies" |] ) //to keep punctuation, but has no effect
let tlp = PennTreebankLanguagePack()
//NOTES:
// from 3.5.2, Stanford switched from SD to UD representations
// In UD, PP head is the complement, therefore collapsing is not needed, e.g. "prep_in" = "nmod". 
// See "enhanced" UD: https://nlp.stanford.edu/~sebschu/pubs/schuster-manning-lrec2016.pdf
// -  Enhanced UD augments nmod with prep, e.g. "nmod:in" or "nmod:on" 
// -  Enhanced UD clones coordinated subjects, objects, and modifiers
// -  Enhanced UD creates a link between a controlled very, e.g. to X, and its subject/controller
// When UD is the default, collapse/propogated seems to be the same as enhanced or enhanced ++
tlp.setGenerateOriginalDependencies(true) //NOTE: comment this line to use universal dependencies by default
let gsf = tlp.grammaticalStructureFactory()

///Get regular and CCprocessed stanford dependencies from a pretokenized sentence
let GetDependencies ( words : string[]) =
    let rawWords = SentenceUtils.toCoreLabelList(words)
    //lp.setOptionFlags( [| "-outputFormatOptions"; "includePunctuationDependencies" |] ) //to keep punctuation, but has no effect
    let tree = lp.apply(rawWords)

    //static so refactored as such
    // Extract dependencies from lexical tree
    //let tlp = PennTreebankLanguagePack()
    //let gsf = tlp.grammaticalStructureFactory()
    
    //Approach 1: worked but could not get punctuation dependencies, even with options
    //regenerate the grammaticalStructure since collapsing in in-place and destroys the typed dependency representation
    //let dep = gsf.newGrammaticalStructure(tree).typedDependencies().toArray() |> Seq.cast<edu.stanford.nlp.trees.TypedDependency> |> Seq.toArray
    //let cc = gsf.newGrammaticalStructure(tree).typedDependenciesCCprocessed(GrammaticalStructure.Extras.MAXIMAL).toArray() //collapsed propagated dependencies
    
    //Approach 2: use semantic graph factory to get the punctuation dependencies
    let basicGraph = SemanticGraphFactory.makeFromTree(tree, SemanticGraphFactory.Mode.BASIC, GrammaticalStructure.Extras.NONE,null,true,true)
    let ccGraph = SemanticGraphFactory.makeFromTree(tree, SemanticGraphFactory.Mode.CCPROCESSED, GrammaticalStructure.Extras.MAXIMAL,null,true,true)
    //make nice for .net; sort by dependent index
    let dep = basicGraph.typedDependencies().toArray() |> Seq.cast<edu.stanford.nlp.trees.TypedDependency> |> Seq.sortBy( fun td -> td.dep().index() ) |> Seq.toArray
    let cc = ccGraph.typedDependencies().toArray() |> Seq.cast<edu.stanford.nlp.trees.TypedDependency> |> Seq.sortBy( fun td -> td.dep().index() ) |> Seq.toArray
    dep,cc

//References
//https://sergey-tihon.github.io/Stanford.NLP.NET/samples/Parser.Sample.html
//https://nlp.stanford.edu/software/dependencies_manual.pdf

