module Evaluation

///AllenNLP SRL verb
type SRLVerb =
    {
        verb : string
        description : string
        tags : string[]
    }

///AllenNLP SRL result
type SRLResult =
    {
        words : string[]
        verbs : SRLVerb[]
    }

// type HierplaneTree =
//     {
//         linkToPosition : Map<string,string>
//         nodeTypeToStyle : Map<string,string[]>
//         text : string
//     }

///AllenNLP dependency parse result
type DependencyParseResult =
    {
        arc_loss : float
        //TODO: we want to ignore this; it is redundant with other data
        //hierplane_tree : HierplaneTree
        loss : float
        pos : string[]
        predicted_dependencies : string[]
        predicted_heads : int[]
        tag_loss : float
        words : string[]
    }

///AllenNLP coreference result. At the document level only.
type CoreferenceResult =
    {
        /// A list of clusters. A cluster is a list of spans. A span is a tuple of onset/offset word indices for the mention (allen uses list instead of tuple)
        clusters : int[][][]
        /// A flat array of words in the document, used to associate indices in the spans with word tokens
        document : string[]
        /// UNUSED. For each top span, the index of the most likely antecedent, with -1 representing none
        predicted_antecedents : int[]
        /// UNUSED. spans that survived pruning. A span is a tuple of onset/offset word indices for the mention (allen uses list instead of tuple)
        top_spans : int[][]
    }

///Coreference information remaped to the sentence level
type SentenceCoreference =
    {
        ///offset into the document, useful for indexing back into clusters
        offset: int
        ///start/stop word in sentence normalized to sentence indices
        spans : int[][]
        ///for each span, the id of chain; maps to CoreferenceResult clusters
        clusters : int[]
    }

///Sentence level annotation combining  annotations from all NLP services
type SentenceAnnotation = 
    {
        /// Position in text
        id : int
        /// Arbitrary tags assigned to annotation, e.g. source information
        tags : string[]
        sen : string
        srl : SRLResult
        dep : DependencyParseResult
        cor : SentenceCoreference
    }

///Document level annotation combining annotations from all NLP services
type DocumentAnnotation = 
    {
        sentences : SentenceAnnotation[]
        coreference : CoreferenceResult
    }

type EvaluationRecord =
    {
        ///Words from AllenNLP
        Words : string[]
        ///POS from AllenNLP
        POS : string[]
        ///Dependencies from AllenNLP
        Dependencies : string[]
        ///Heads from AllenNLP
        Heads : int[]
        ///True if dependencies from AllenNLP and Stanford parser are identical, PRE-collapse
        IdenticalDependenciesAS : bool
        ///True if dependencies from AllenNLP and Stanford parser are identical, POST-collapse
        IdenticalCCAS : bool
        ///True if Stanford-internal collapsed is identical to us collapsing on Stanford (reference dependencies)
        IdenticalCCRS : bool
        ///Matching dependencies between AllenNLP and Stanford
        DependenciesMatchingAS : string[]
        ///Dependencies only Allen has, relative to Stanford
        DependenciesAllenOnly : string[]
        ///Dependencies only Stanford has, relative to Allen
        DependenciesStanfordOnly : string[]
        ///Matching collapsed dependencies between AllenNLP and Stanford
        CCMatchingAS : string[]
        ///Collapsed dependencies only Allen has, relative to Stanford
        CCAllenOnly : string[]
        ///Collapsed dependencies only Stanford has, relative to Allen
        CCStanfordOnlyA : string[]
        ///Matching between our collapsing of Stanford dependencies and Stanford collapsed
        CCMatchingRS : string[]
        ///Collapsed dependencies only we have, relative to Stanford collapsed
        CCReferenceOnly : string[]
        ///Collapsed dependencies only Stanford has, relative to reference dependencies collapsed
        CCStanfordOnlyR : string[]
    }

/// Compares collapsed dependencies for AllenNLP and Stanford using our collapsed dependencies 
/// and Stanford's collapsed dependencies as comparisons.
/// TODO: Include a JobImText comparison for the same text to test whether we are collapsing identically
let DoEvaluation( jsonParseFilePath : string) =
    let json = jsonParseFilePath |> System.IO.File.ReadAllText
    let documentAnnotation = Newtonsoft.Json.JsonConvert.DeserializeObject<DocumentAnnotation>( json )

    let evaluationRecords = ResizeArray<EvaluationRecord>()

    for sen in documentAnnotation.sentences do

        //Collapse Allen depedencies
        let allenDep,allenCC = 
            Collapser.CollapseTokens(
                [
                    for i = 0 to sen.dep.words.Length - 1 do
                        yield Rules.Token.Create( i, sen.dep.words.[i], sen.dep.pos.[i], sen.dep.predicted_dependencies.[i], sen.dep.predicted_heads.[i] )
                ] )  
        
        //Prepare for comparisons
        let allenDepSet =  allenDep |> Seq.map Collapser.StanfordFormat |> Set.ofSeq 
        let allenCCSet = allenCC |> Seq.map Collapser.StanfordFormat |> Set.ofSeq

        //Get "gold" dependencies and CC from Stanford
        let stanfordDep,stanfordCC = StanfordParser.GetDependencies( sen.dep.words )

        //Prepare for comparisons
        let stanfordDepSet = stanfordDep |> Array.map( fun d -> d |> sprintf "%A" ) |> Set.ofSeq
        let stanfordCCSet = stanfordCC |> Array.map( fun d -> d |> sprintf "%A" ) |> Set.ofSeq

        //Collapse on Stanford's "gold" dependencies (this reference comparison holds deps constant, so any changes are due to collapsing rules) 
        let referenceDep,referenceCC = 
            Collapser.CollapseTokens(
                [
                    for i = 0 to stanfordDep.Length - 1 do
                        yield Rules.Token.Create( i, stanfordDep.[i].dep().word(), stanfordDep.[i].dep().tag(), stanfordDep.[i].reln().getShortName(), stanfordDep.[i].gov().index() )
                ] )  

        //Prepare for comparisons (cc-only, since deps are constant)
        let referenceCCSet = referenceCC |> Seq.map Collapser.StanfordFormat |> Set.ofSeq

        //Sanity checking: what we pass in to the collapser should match what Stanford gave us
        let referenceSet = referenceDep |> Seq.map Collapser.StanfordFormat |> Set.ofSeq
        if referenceSet <> stanfordDepSet then
            failwith "Reference dependencies do not match Stanford dependencies. This should be impossible."

        //Store comparisons: what dependencies match and what are unique to each method (A:Allen, S:Stanford, R:Reference; we do collapse in every case but S)
        evaluationRecords.Add(
            {
                Words = sen.dep.words
                POS = sen.dep.pos
                Dependencies = sen.dep.predicted_dependencies
                Heads = sen.dep.predicted_heads
                IdenticalDependenciesAS = (allenDepSet = stanfordDepSet)
                IdenticalCCAS = (allenCCSet = stanfordCCSet)
                IdenticalCCRS = (referenceCCSet = stanfordCCSet)
                DependenciesMatchingAS = (Set.intersect allenDepSet stanfordDepSet) |> Set.toArray
                DependenciesAllenOnly = (Set.difference allenDepSet stanfordDepSet) |> Set.toArray
                DependenciesStanfordOnly = (Set.difference stanfordDepSet allenDepSet) |> Set.toArray
                CCMatchingAS = (Set.intersect allenCCSet stanfordCCSet) |> Set.toArray
                CCAllenOnly = (Set.difference allenCCSet stanfordCCSet) |> Set.toArray
                CCStanfordOnlyA = (Set.difference stanfordCCSet allenCCSet) |> Set.toArray
                CCMatchingRS = (Set.intersect referenceCCSet stanfordCCSet) |> Set.toArray
                CCReferenceOnly = (Set.difference referenceCCSet stanfordCCSet) |> Set.toArray
                CCStanfordOnlyR = (Set.difference stanfordCCSet referenceCCSet) |> Set.toArray
            }
        )
    //
    evaluationRecords

        