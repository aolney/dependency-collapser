// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
   
let DoExample() =
    printfn "%s" "Processing 'They sit in the car.'"
    let dependencies,dependenciesCC = 
        Collapser.CollapseTokens(
            [
                Rules.Token.Create(0,"They","PRP","nsubj",2)
                Rules.Token.Create(1,"sit","VBP","root",0)
                Rules.Token.Create(2,"in","IN","prep",2)
                Rules.Token.Create(3,"the","DT","det",5)
                Rules.Token.Create(4,"car","NN","pobj",3)
                Rules.Token.Create(5,".",".","punct",2)
            ] )  
    printfn "%s" "Our results: uncollapsed and collapsed"
    dependencies |> Seq.map Collapser.StanfordFormat |> Seq.iter (printfn "%s")
    printfn "%s" "--------------------"
    dependenciesCC |> Seq.map Collapser.StanfordFormat |> Seq.iter (printfn "%s")
    printfn "%s" "===================="
    printfn "%s" "Stanford results: uncollapsed and collapsed"
    let stanfordDep,stanfordCC = StanfordParser.GetDependencies( [| "They";"sit";"in";"the";"car";"." |] )
    stanfordDep |> Seq.iter( printfn "%A" )
    printfn "%s" "--------------------"
    stanfordCC |> Seq.iter( printfn "%A" )
    printfn "%s" "press any key to continue"
    System.Console.ReadLine() |> ignore

[<EntryPoint>]
let main argv = 

    //Toy example
    DoExample()

    //Run evaluation/text
    let filePath =
        if System.Type.GetType ("Mono.Runtime") = null then
            @"Z:\aolney\research_projects\mofacts\materials\ap-book\wes-download-html\mcghimgs\ch9.parse.json"
        else
            "/z/aolney/research_projects/mofacts/materials/ap-book/wes-download-html/mcghimgs/ch9.parse.json"
    let evaluationRecords = Evaluation.DoEvaluation(filePath)
    0 // return an integer exit code
