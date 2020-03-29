module Collapser

open Rules
open System.Collections.Generic

///Get candidates for replacement (dependencies) and indices for replacing them
let getCandidatesAndIndices (rule : Rule) (dependencies : Dependency seq) =

    //for each rule relation test each dependency and return matching dependencies
    let candidates = 
        rule.Relations 
        |> List.map( fun relation -> 
            dependencies 
            |> Seq.choose( fun dep -> 
                dep.Type |> doMatch dep relation.Regex 
            ) 
            |> Seq.toList
        )

    //confirm candidate dependencies and construct indices for matching rules
    let confirmedCandidates = ResizeArray<ResizeArray<Dependency>>()
    let ruleTokens = Dictionary<int,ResizeArray<RuleToken>>()
    let targetRuleTokens =  Dictionary<int,ResizeArray<RuleToken>>()

    for i = 0 to candidates.Length - 1 do
        confirmedCandidates.Add( new ResizeArray<Dependency>() )
        for dep in candidates.[i] do
            let from,To = rule.Relations.[i].From,rule.Relations.[i].To//Indexing with 'i' works because candidates can be duplicates; we implicitly loop over relations for non duplicates
            let headMatch = dep.Governor |> propertyMatch rule.Properties.[from]
            let dependentMatch = dep.Dependent |> propertyMatch rule.Properties.[To]
            let relationMatch = dep.Type |> doMatch null rule.Relations.[i].Regex
            match headMatch,dependentMatch,relationMatch with
            | Some(_),Some(_),Some(_) -> 
                confirmedCandidates.[i].Add(dep)
                //initialize empty containers
                if not <| ruleTokens.ContainsKey(from) then ruleTokens.Add( from, ResizeArray<RuleToken>() )
                if not <| targetRuleTokens.ContainsKey(To) then targetRuleTokens.Add( To, ResizeArray<RuleToken>() )
                //add ruleToken to respective containers
                let ruleToken = { Index=from; TargetIndex=To; Token= dep.Governor; TargetToken= dep.Dependent; DependencyType=dep.Type; Durable=rule.Relations.[i].Type = RelationType.DurableRelation }
                ruleTokens.[from].Add(ruleToken)
                targetRuleTokens.[To].Add(ruleToken)
            | _ -> ()
        ()
    ()
    //
    confirmedCandidates, ruleTokens, targetRuleTokens

///Build models ?representing incremental rule applications?
let createModels (rule:Rule) (ruleTokens:Dictionary<int,ResizeArray<RuleToken>>) (targetRuleTokens:Dictionary<int,ResizeArray<RuleToken>>) =

    let models = new ResizeArray<Model>()
    models.Add( Model.Create( rule.From, rule.To ) )

    for x = 0 to rule.Properties.Count - 1 do
        for relation in rule.Relations do
            if ruleTokens.[relation.From].Count > 0 && targetRuleTokens.[relation.To].Count > 0 then
                for fromRuleToken in ruleTokens.[relation.From] do
                    let modelCount = models.Count - 1
                    for i = 0 to modelCount do
                        match models.[i].AddRuleToken( fromRuleToken ) with
                        | Some(model) -> models.Add(model)
                        | None -> ()
        ()
    ()
    //
    models

///Using models, select dependencies to remove or add, given this rule
let selectDependenciesWithModels (rule : Rule) models (dependenciesToRemove:HashSet<Token*Token>) (dependenciesToAdd : HashSet<Dependency>) =
    
    let keys = HashSet<string>() //TODO: no longer needed?

    //If models are valid, adjust the dependencies accordingly
    for model in models do
        match model.Tokens.TryGetValue(rule.From),model.Tokens.TryGetValue(rule.To) with
        | (true,governor),(true,dependent) ->
            //Original logic pretty ugly; attempting simplification
            let modelIsValid = 
                rule.Relations 
                |> List.forall( fun relation -> 
                    model.Tokens.ContainsKey( relation.From) && 
                    model.Tokens.ContainsKey( relation.To ) &&
                    model.Relations
                    |> Seq.exists( fun ruleToken -> 
                        ruleToken.Index = relation.From && 
                        ruleToken.TargetIndex = relation.To && 
                        (ruleToken.DependencyType |> doMatch null relation.Regex).IsSome ) )
            if modelIsValid then
                let dependencyType = rule.ResolvedRelationName( model )
                let key = governor.Index.ToString() + "|" + dependent.Index.ToString() + "|" + dependencyType

                //only apply an operation once; NOTE: is this necessary if we are hashing?
                if keys.Add(key) then
                    //collect removal operations
                    model.Relations 
                    |> Seq.filter( fun rt -> not <| rt.Durable )
                    |> Seq.iter( fun rt -> dependenciesToRemove.Add( rt.Token,rt.TargetToken) |> ignore )
                
                    //add new dependency if type not void
                    if dependencyType <> "void" then
                        dependenciesToAdd.Add( {Type=dependencyType; Governor=governor; Dependent=dependent } ) |> ignore

        | _ -> ()

let StanfordFormat (dependency:Dependency) =
    let governorString = 
        if dependency.Type = "root" then
            "ROOT-0"
        else 
            dependency.Governor.Word + "-" + (dependency.Governor.Index + 1).ToString()
    let dependentString = dependency.Dependent.Word + "-" + (dependency.Dependent.Index + 1).ToString()
    dependency.Type + "(" + governorString + ", " + dependentString + ")"

///This attempts to follow jobimtext while also cleaning it up
let CollapseTokens( tokens : Token list) =

    //dependencies are initialized from tokens but then mutated after each stage
    let dependencies =
        tokens
        |> dependenciesFromTokens
        |> ResizeArray
        
    //Apply rules in stages
    for stage in stageRuleList do

        //Collect operations; perform *after* all rules are applied
        let dependenciesToRemove = HashSet<Token*Token>()
        let dependenciesToAdd = HashSet<Dependency>()

        //Apply rules for current stage
        for rule in stage.Rules do

            //Given this rule, for each of its relations, get candidates for replacement (dependencies) and indices for replacing them
            let confirmedCandidates, ruleTokens, targetRuleTokens = getCandidatesAndIndices rule dependencies 

            //All rule relations must have a candidate dependency or the rule will never succeed
            if confirmedCandidates |> Seq.forall( fun depList -> depList.Count > 0 ) then

                //Build models ?representing incremental application of this rule?
                let models = createModels rule ruleTokens targetRuleTokens

                //NOTE: dead code with conflicting indices omitted

                //Using models, select dependencies to remove or add, given this rule
                selectDependenciesWithModels rule models dependenciesToRemove dependenciesToAdd

        // remove dependencies using collected information
        dependencies
        |> List.ofSeq //clone to avoid modifying the iterating collection
        |> List.iter( fun dep -> if dependenciesToRemove.Contains(dep.Governor,dep.Dependent) then dependencies.Remove(dep) |> ignore )
                            
        //add dependencies using collected information
        dependencies.AddRange( dependenciesToAdd )

    //
    tokens |> dependenciesFromTokens,
    dependencies |> Seq.sortBy( fun dep -> dep.Dependent.Index * 10 + dep.Governor.Index ) //hackish way to keep token order but further order by head when equal
     