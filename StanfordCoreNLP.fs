module StanfordCoreNLP

// https://sergey-tihon.github.io/Stanford.NLP.NET/samples/CoreNLP.Pipeline.html
//#r "IKVM.OpenJDK.Core.dll"
//#r "IKVM.OpenJDK.Util.dll"
//#r "stanford-corenlp-3.8.0.dll"

open System
open System.IO
open java.util
open java.io
open edu.stanford.nlp.pipeline

// Path to the folder with models extracted from `stanford-corenlp-3.8.0-models.jar`
let jarRoot = __SOURCE_DIRECTORY__ + @"/../stanford-corenlp-full-2017-06-09/models"

// Annotation pipeline configuration
let props = Properties()
//NOTE: could compare with their openie: https://stanfordnlp.github.io/CoreNLP/openie.html
props.setProperty("annotators","tokenize, ssplit, pos, parse") |> ignore
//props.setProperty("annotators","tokenize, ssplit, pos, lemma, ner, parse, dcoref") |> ignore
//props.setProperty("ner.useSUTime","0") |> ignore

// We should change current directory, so StanfordCoreNLP could find all the model files automatically
let curDir = Environment.CurrentDirectory
Directory.SetCurrentDirectory(jarRoot)
let pipeline = StanfordCoreNLP(props)
Directory.SetCurrentDirectory(curDir)

let ApplyPipeline( text : string ) = 
    // Annotation
    let annotation = Annotation(text)
    pipeline.annotate(annotation)

    // Result - Pretty Print
    let stream = new ByteArrayOutputStream()
    pipeline.prettyPrint(annotation, new PrintWriter(stream))
    printfn "%O" <| stream.toString()
    stream.close()
