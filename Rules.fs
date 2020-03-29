module Rules

open System.Collections.Generic

let tryGetValue (collection:IDictionary<'T,'U>) key =
    match collection.TryGetValue(key) with
    | true,v -> Some(v)
    | false,_ -> None

let toLower (s:string) =
    s.ToLower()

type Token =
     {
        ///0-based index
        Index : int
        Word : string
        POS : string
        ///Defaults to word if empty on creation
        Lemma : string
        DependencyType : string
        Head : int
        ///The text dominated by this token //apparently unneeded
        //Span : string
     } with 
     /// If no lemma is provided, uses word as lemma
     static member Create(index, word, pos, depType, head)= 
        { Index=index; Word=word; POS=pos; Lemma=word; DependencyType=depType;Head=head} //;Span=""}
     /// Full constructor
     static member Create(index,word, pos, lemma, depType, head) = 
        { Index=index; Word=word; POS=pos; Lemma=lemma; DependencyType=depType;Head=head} //;Span=""}

type Dependency =
     {
        Type : string
        Governor : Token
        Dependent : Token
     }

//TODO: simplify
type RuleToken = 
    {
        ///From index (governor)
        Index : int
        ///Governor token (see constructor)
        Token : Token
        ///To index (dependent)
        TargetIndex : int
        ///Dependent token (see constructor)
        TargetToken : Token
        ///Current dependency type (so may not match target/dependent's dependency type)
        DependencyType : string
        Durable : bool
    } with
    member this.Signature() = 
        let typeString = if this.Durable then "d" else "r" 
        typeString + this.Index.ToString() + "_" + this.TargetIndex.ToString()

type SpecifierType = Word | POS | Lemma 

type RelationType =  Relation | DurableRelation

type Property =
    {
        /// The type the pattern is applied to
        Type : SpecifierType
        /// TheOLDSpecifierTypeer used to reference a matched word
        Reference : int
        /// The regular expression pattern
        Regex : string
    }

type Relation = 
    {
        /// The type the pattern is applied to
        Type : RelationType
        /// The reference number of the word specifier of the start word (dependent)
        From : int
        /// The reference number of the word specifier of the end word (head)
        To : int 
        /// The regular expression pattern
        Regex : string
    } with
    member this.Signature() = 
        let typeString = match this.Type with | Relation -> "r" | DurableRelation -> "d"
        typeString + this.From.ToString() + "_" + this.To.ToString()


/// Find the spanned text for a 0-based index into tokens.
/// Note that the dependency parse heads are 1-indexed.
let spannedText (token : Token) (tokens: list<Token>) =
    let frontier = HashSet<int>()
    frontier.Add(token.Index + 1) |> ignore
    let mutable notDone = true
    while notDone do
        notDone <- false
        for f in frontier |> List do
            for t in tokens do
                if frontier.Contains( t.Head ) then
                    notDone <- frontier.Add( t.Index + 1) //if not present already, notDone set to true
    //
    frontier |> Seq.sort |> Seq.map( fun f -> tokens.[f-1].Word) |> String.concat " "
        
/// Add text of dominated tokens, i.e. a span, to each token
//let addSpans (tokens: list<Token>) =
//    tokens |> List.map( fun t -> {t with Span=(spannedText t tokens)})

/// Get the governing token of a token
let governor (tokens: list<Token>) (token : Token) =
    if token.Head > 0 then
        tokens.[ token.Head - 1]
    //ROOT token is self-governing
    else
        token

let dependenciesFromTokens tokens =
    tokens |> List.map( fun token -> { Governor= token |> governor tokens; Dependent = token; Type=token.DependencyType } )

/// Do a regex match and return an option of the retval
let doMatch retVal pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success then Some(retVal) else None

/// Match on the appropriate property
let propertyMatch (property : Property) token =
    match property.Type with
    //| Word -> token.Span |> doMatch token.Span property.Regex //assumed span was needed b/c "getCoveredText" is called in the original, but apparently that call is scoped to the token.
    | Word -> token.Word |> doMatch token.Word property.Regex
    //| POS -> token.POS |> doMatch token.POS property.Regex 
    | POS -> token.POS |> doMatch token.Word property.Regex //This is somewhat undocumented, but often added relations have a name that is the word associated with a POS match. Currently no rules require POS for a POS match in this way.
    | Lemma -> token.Lemma |> doMatch token.Lemma property.Regex


type Model =
    {
        From : int
        To : int
        Tokens : Dictionary<int,Token>
        Relations : ResizeArray<RuleToken>
        AddedTokens : HashSet<string>
    } with
    static member Create(from, To)= 
        { From=from; To=To; Tokens=Dictionary<int,Token>(); Relations=ResizeArray<RuleToken>();AddedTokens=HashSet<string>()}
    member this.AddRuleToken( ruleToken : RuleToken ) =
        match this.AddedTokens.Add( sprintf "%A" ruleToken ) with
        | false -> None
        | true ->
            //clone the current model with new reference types and fill them
            let newModel = { 
                this with 
                    Tokens = Dictionary<int,Token>(this.Tokens)
                    Relations = ResizeArray<RuleToken>(this.Relations)
                    AddedTokens = HashSet<string>(this.AddedTokens) //NOTE added tokens will be identical bw original and copy
                } 
            match this.Tokens.TryGetValue(ruleToken.Index ),  this.Tokens.TryGetValue(ruleToken.TargetIndex )  with
            | (false, token), (false, targetToken) -> 
                newModel.Tokens.Add( ruleToken.Index, ruleToken.Token )
                newModel.Tokens.Add( ruleToken.TargetIndex, ruleToken.TargetToken )
                newModel.Relations.Add( ruleToken )
                Some(newModel)
            | (true, token), (false, targetToken) when token = ruleToken.Token ->
                newModel.Tokens.Add( ruleToken.TargetIndex, ruleToken.TargetToken )
                newModel.Relations.Add( ruleToken )
                Some(newModel)
            | (false, token), (true, targetToken) when targetToken = ruleToken.TargetToken ->
                newModel.Tokens.Add( ruleToken.Index, ruleToken.Token )
                newModel.Relations.Add( ruleToken )
                Some(newModel)
            | _ -> 
                for relation in this.Relations do if relation = ruleToken then newModel.Relations.Add( ruleToken )
                //only emit a new model if it is different from the currenct model
                let newModelString = sprintf "%A" newModel //hack: we are using types that don't have structural equality, so we fake it
                let thisString = sprintf "%A" this
                if newModelString = thisString then None else Some(newModel)

type Rule =
    {
        Properties : Map<int,Property>
        Relations : Relation list
        /// The reference number of the word the new dependency orginates from (dependent)
        From : int
        /// The reference number of the word the new dependency goes to (head)
        To : int
        /// TODO: rename RelationNameTemplate. Arbitrary label of the relation name; it can contain the word,pos,or lemma matched, e.g. "relationName:the_Pos_tag_is_{p1}".
        RelationName : string
    } with
    member this.ResolvedRelationName( model : Model ) =
        //get all property/relation word matches for this model
        let matchDictionary = Dictionary<string,string>()
        this.Properties 
        |> Map.toList 
        |> List.iter( fun (i,property) -> 
            match model.Tokens.[i] |> propertyMatch property with
            | Some(matchedWords) -> matchDictionary.Add( property |> sprintf "%A", matchedWords )
            | None -> ()
        )
        this.Relations
        |> List.iter( fun relation ->
            model.Relations
            |> Seq.iter( fun ruleToken -> 
                if relation.Signature() = ruleToken.Signature() then
                    let dependencyType = ruleToken.DependencyType
                    match dependencyType |> doMatch dependencyType relation.Regex with
                    | Some(matchedWords) ->  matchDictionary.Add( relation |> sprintf "%A", matchedWords )
                    | None -> ()
                ()
            )
        )

        let mutable retVal = this.RelationName
        let regex = System.Text.RegularExpressions.Regex(@"\{(\w+)\}")
        
        regex.Matches(this.RelationName)
        |> Seq.cast
        |> Seq.choose( fun (m : System.Text.RegularExpressions.Match) -> 
            let id = m.Groups.[1].Value         
            if id.StartsWith("r") || id.StartsWith("d") then //only relations start with r or d
                match this.Relations |> List.tryFind( fun relation -> id = relation.Signature() ) with
                | Some(r) -> r |> sprintf "%A" |> tryGetValue matchDictionary
                | None -> None
            else
                this.Properties.[ id.Substring(1) |> System.Int32.Parse ] |> sprintf "%A" |> tryGetValue matchDictionary 

        )
        |> Seq.iter( fun s -> 
            retVal <- regex.Replace(retVal, s, 1) //replace only the first found
            )
        //
        retVal |> toLower

type Stage =
    {
        Id : int
        Rules : Rule list
    }


///// Automatically created from OLDstageRuleList to better match jobimtext API
let stageRuleList =
    [{Id = 1;
      Rules =
       [{Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = DurableRelation;
                       From = 1;
                       To = 2;
                       Regex = "prep";}; {Type = Relation;
                                          From = 2;
                                          To = 3;
                                          Regex = "prep";}];
         From = 1;
         To = 3;
         RelationName = "prep";};
        {Properties = Map.ofList <| [(1, {Type = Word;
                                Reference = 1;
                                Regex = "well";}); (2, {Type = Word;
                                                        Reference = 2;
                                                        Regex = "as";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = ".*";}];
         From = 1;
         To = 2;
         RelationName = "void";};
        {Properties = Map.ofList <| [(1, {Type = Word;
                                Reference = 1;
                                Regex = ".*";}); (2, {Type = Word;
                                                      Reference = 2;
                                                      Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "xcomp";}; {Type = DurableRelation;
                                           From = 1;
                                           To = 2;
                                           Regex = "ccomp";}];
         From = 1;
         To = 2;
         RelationName = "void";};
        {Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = DurableRelation;
                       From = 1;
                       To = 2;
                       Regex = "prep";}; {Type = Relation;
                                          From = 2;
                                          To = 3;
                                          Regex = "conj";}];
         From = 1;
         To = 3;
         RelationName = "conj";};
        {Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = DurableRelation;
                       From = 1;
                       To = 2;
                       Regex = "prep";}; {Type = DurableRelation;
                                          From = 2;
                                          To = 3;
                                          Regex = "conj";}];
         From = 1;
         To = 3;
         RelationName = "prep";};
        {Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = DurableRelation;
                       From = 1;
                       To = 2;
                       Regex = "cc";}; {Type = Relation;
                                        From = 2;
                                        To = 3;
                                        Regex = "mwe";}];
         From = 1;
         To = 2;
         RelationName = "void";};
        {Properties =
          Map.ofList <|
            [(1, {Type = Word;
                  Reference = 1;
                  Regex = ".*";}); (2, {Type = POS;
                                        Reference = 2;
                                        Regex = "IN|TO|VBG";}); (3, {Type = Word;
                                                                     Reference = 3;
                                                                     Regex = ".*";});
             (4, {Type = Word;
                  Reference = 4;
                  Regex = ".*";})];
         Relations = [{Type = DurableRelation;
                       From = 1;
                       To = 2;
                       Regex = "prep";}; {Type = DurableRelation;
                                          From = 2;
                                          To = 3;
                                          Regex = "pobj";}; {Type = DurableRelation;
                                                             From = 2;
                                                             To = 4;
                                                             Regex = "mwe";}];
         From = 1;
         To = 4;
         RelationName = "mwe_helper";};
        {Properties = Map.ofList <| [(1, {Type = Word;
                                Reference = 1;
                                Regex = ".*";}); (2, {Type = Word;
                                                      Reference = 2;
                                                      Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "parataxis";}; {Type = DurableRelation;
                                               From = 1;
                                               To = 2;
                                               Regex = "^(?!parataxis|tmod$).*";}];
         From = 1;
         To = 2;
         RelationName = "void";};
        {Properties = Map.ofList <| [(1, {Type = Word;
                                Reference = 1;
                                Regex = ".*";}); (2, {Type = Word;
                                                      Reference = 2;
                                                      Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "tmod";}; {Type = DurableRelation;
                                          From = 1;
                                          To = 2;
                                          Regex = "parataxis";}];
         From = 1;
         To = 2;
         RelationName = "void";}];};
     {Id = 2;
      Rules =
       [{Properties =
          Map.ofList <|
            [(1, {Type = Word;
                  Reference = 1;
                  Regex = ".*";}); (2, {Type = Word;
                                        Reference = 2;
                                        Regex = "next";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = "to";});
             (4, {Type = Word;
                  Reference = 4;
                  Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = ".*";}; {Type = Relation;
                                        From = 2;
                                        To = 3;
                                        Regex = ".*";}; {Type = Relation;
                                                         From = 3;
                                                         To = 4;
                                                         Regex = "pobj";}];
         From = 1;
         To = 4;
         RelationName = "prep_{w2}_to";};
        {Properties =
          Map.ofList <|
            [(1, {Type = Word;
                  Reference = 1;
                  Regex = ".*";}); (2, {Type = POS;
                                        Reference = 2;
                                        Regex = "IN|TO|VBG";}); (3, {Type = Word;
                                                                     Reference = 3;
                                                                     Regex = ".*";})];
         Relations = [{Type = DurableRelation;
                       From = 1;
                       To = 2;
                       Regex = "prep";}; {Type = DurableRelation;
                                          From = 2;
                                          To = 3;
                                          Regex = "pobj";}];
         From = 1;
         To = 3;
         RelationName = "prep_{w2}";};
        {Properties =
          Map.ofList <|
            [(1, {Type = Word;
                  Reference = 1;
                  Regex = ".*";}); (2, {Type = Word;
                                        Reference = 2;
                                        Regex = "as";}); (3, {Type = Word;
                                                              Reference = 3;
                                                              Regex = ".*";});
             (4, {Type = Word;
                  Reference = 4;
                  Regex = "such";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "prep";}; {Type = Relation;
                                          From = 2;
                                          To = 3;
                                          Regex = "pobj";}; {Type = Relation;
                                                             From = 2;
                                                             To = 4;
                                                             Regex = "mwe";}];
         From = 1;
         To = 3;
         RelationName = "prep_such_as";};
        {Properties =
          Map.ofList <|
            [(1, {Type = Word;
                  Reference = 1;
                  Regex = ".*";}); (2, {Type = POS;
                                        Reference = 2;
                                        Regex = "IN|TO";}); (3, {Type = Word;
                                                                 Reference = 3;
                                                                 Regex = ".*";});
             (4, {Type = Word;
                  Reference = 4;
                  Regex = ".*";})];
         Relations = [{Type = DurableRelation;
                       From = 1;
                       To = 2;
                       Regex = "prep";}; {Type = Relation;
                                          From = 2;
                                          To = 3;
                                          Regex = "pobj";}; {Type = Relation;
                                                             From = 2;
                                                             To = 4;
                                                             Regex = "punct";}];
         From = 1;
         To = 4;
         RelationName = "punct";};
        {Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = POS;
                                          Reference = 2;
                                          Regex = "IN|TO";}); (3, {Type = Word;
                                                                   Reference = 3;
                                                                   Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "prep";}; {Type = Relation;
                                          From = 2;
                                          To = 3;
                                          Regex = "pcomp";}];
         From = 1;
         To = 3;
         RelationName = "prepc_{w2}";};
        {Properties =
          Map.ofList <|
            [(1, {Type = Word;
                  Reference = 1;
                  Regex = ".*";});
             (2, {Type = Word;
                  Reference = 2;
                  Regex = "and|or|but|nor|in|only|as|at|vs\.|&|versus|and/or";});
             (3, {Type = Word;
                  Reference = 3;
                  Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "cc";}; {Type = Relation;
                                        From = 1;
                                        To = 3;
                                        Regex = "conj";}];
         From = 1;
         To = 3;
         RelationName = "conj_{w2}";};
        {Properties =
          Map.ofList <|
            [(1, {Type = Word;
                  Reference = 1;
                  Regex = ".*";}); (2, {Type = Word;
                                        Reference = 2;
                                        Regex = "not|instead|rather";});
             (3, {Type = Word;
                  Reference = 3;
                  Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "cc";}; {Type = Relation;
                                        From = 1;
                                        To = 3;
                                        Regex = "conj";}];
         From = 1;
         To = 3;
         RelationName = "conj_negcc";};
        {Properties = Map.ofList <| [(1, {Type = Word;
                                Reference = 1;
                                Regex = ".*";}); (2, {Type = Word;
                                                      Reference = 2;
                                                      Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "xsubj|ref|possessive";}];
         From = 1;
         To = 2;
         RelationName = "void";};
        {Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = DurableRelation;
                       From = 1;
                       To = 2;
                       Regex = "prep";}; {Type = Relation;
                                          From = 2;
                                          To = 3;
                                          Regex = "advmod";}];
         From = 1;
         To = 3;
         RelationName = "advmod";};
        {Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = DurableRelation;
                       From = 1;
                       To = 2;
                       Regex = "prep";}; {Type = Relation;
                                          From = 2;
                                          To = 3;
                                          Regex = "dep";}];
         From = 1;
         To = 3;
         RelationName = "dep";};
        {Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = "to";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "prep";}; {Type = Relation;
                                          From = 2;
                                          To = 3;
                                          Regex = "pcomp";}];
         From = 1;
         To = 3;
         RelationName = "prepc_{w2}_{w3}";}];};
     {Id = 3;
      Rules =
       [{Properties = Map.ofList <| [(1, {Type = Word;
                                Reference = 1;
                                Regex = ".*";}); (2, {Type = Word;
                                                      Reference = 2;
                                                      Regex = "such";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "prep_as";}];
         From = 1;
         To = 2;
         RelationName = "prep_such_as";};
        {Properties = Map.ofList <| [(1, {Type = Word;
                                Reference = 1;
                                Regex = ".*";}); (2, {Type = Word;
                                                      Reference = 2;
                                                      Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "prep_[^_]+";}; {Type = DurableRelation;
                                                From = 1;
                                                To = 2;
                                                Regex = "prep_[^_]+_.+";}];
         From = 1;
         To = 2;
         RelationName = "void";};
        {Properties = Map.ofList <| [(1, {Type = Word;
                                Reference = 1;
                                Regex = ".*";}); (2, {Type = Word;
                                                      Reference = 2;
                                                      Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "conj_&";}];
         From = 1;
         To = 2;
         RelationName = "conj_and";};
        {Properties =
          Map.ofList <|
            [(1, {Type = Word;
                  Reference = 1;
                  Regex = ".*";}); (2, {Type = Word;
                                        Reference = 2;
                                        Regex = ".*";}); (3, {Type = Word;
                                                              Reference = 3;
                                                              Regex = ".*";});
             (4, {Type = Word;
                  Reference = 4;
                  Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 3;
                       Regex = "conj_and";}; {Type = Relation;
                                              From = 1;
                                              To = 2;
                                              Regex = "pobj";}; {Type = Relation;
                                                                 From = 3;
                                                                 To = 4;
                                                                 Regex = "pobj";}];
         From = 2;
         To = 4;
         RelationName = "conj_and";};
        {Properties =
          Map.ofList <|
            [(1, {Type = Word;
                  Reference = 1;
                  Regex = ".*";}); (2, {Type = POS;
                                        Reference = 2;
                                        Regex = "IN|TO|VBG";}); (3, {Type = Word;
                                                                     Reference = 3;
                                                                     Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "prep";}; {Type = Relation;
                                          From = 2;
                                          To = 3;
                                          Regex = "pobj";}];
         From = 1;
         To = 3;
         RelationName = "void";};
        {Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "prep";}; {Type = Relation;
                                          From = 2;
                                          To = 3;
                                          Regex = "advmod";}];
         From = 1;
         To = 3;
         RelationName = "void";};
        {Properties =
          Map.ofList <|
            [(1, {Type = Word;
                  Reference = 1;
                  Regex = ".*";}); (2, {Type = Word;
                                        Reference = 2;
                                        Regex = "to|of|with";});
             (3, {Type = Word;
                  Reference = 3;
                  Regex = ".*";})];
         Relations = [{Type = DurableRelation;
                       From = 1;
                       To = 2;
                       Regex = "prep[c]?_.+";}; {Type = Relation;
                                                 From = 2;
                                                 To = 3;
                                                 Regex = "pobj";}];
         From = 1;
         To = 3;
         RelationName = "pobj";};
        {Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "cc";}; {Type = DurableRelation;
                                        From = 1;
                                        To = 3;
                                        Regex = "conj_and";}];
         From = 1;
         To = 2;
         RelationName = "void";};
        {Properties =
          Map.ofList <| [(1, {Type = POS;
                    Reference = 1;
                    Regex = "CD";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "advmod";}; {Type = Relation;
                                            From = 1;
                                            To = 3;
                                            Regex = "prep_of";}];
         From = 1;
         To = 3;
         RelationName = "prep_{w2}_of";};
        {Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "advmod";}; {Type = Relation;
                                            From = 2;
                                            To = 3;
                                            Regex = "prepc_of";}];
         From = 1;
         To = 3;
         RelationName = "prepc_{w2}_of";};
        {Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "advmod";}; {Type = Relation;
                                            From = 1;
                                            To = 3;
                                            Regex = "prep_to";}];
         From = 1;
         To = 3;
         RelationName = "prep_{w2}_to";};
        {Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "advmod";}; {Type = Relation;
                                            From = 2;
                                            To = 3;
                                            Regex = "prep_to";}];
         From = 1;
         To = 3;
         RelationName = "prep_{w2}_to";};
        {Properties =
          Map.ofList <|
            [(1, {Type = Word;
                  Reference = 1;
                  Regex = ".*";}); (2, {Type = Word;
                                        Reference = 2;
                                        Regex = "[aA]ccording|[dD]ue";});
             (3, {Type = Word;
                  Reference = 3;
                  Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = ".*";}; {Type = Relation;
                                        From = 2;
                                        To = 3;
                                        Regex = "prep_to";}];
         From = 1;
         To = 3;
         RelationName = "prep_{w2}_to";};
        {Properties =
          Map.ofList <|
            [(1, {Type = Word;
                  Reference = 1;
                  Regex = ".*";}); (2, {Type = Word;
                                        Reference = 2;
                                        Regex = "addition";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "prep_in";}; {Type = Relation;
                                             From = 2;
                                             To = 3;
                                             Regex = "prep_to";}];
         From = 1;
         To = 3;
         RelationName = "prep_in_addition_to";};
        {Properties =
          Map.ofList <|
            [(1, {Type = Word;
                  Reference = 1;
                  Regex = ".*";}); (2, {Type = Word;
                                        Reference = 2;
                                        Regex = "addition";}); (3, {Type = Word;
                                                                    Reference = 3;
                                                                    Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "prep_in";}; {Type = Relation;
                                             From = 2;
                                             To = 3;
                                             Regex = "prep_of";}];
         From = 1;
         To = 3;
         RelationName = "prep_in_front_of";};
        {Properties =
          Map.ofList <|
            [(1, {Type = Word;
                  Reference = 1;
                  Regex = ".*";}); (2, {Type = Word;
                                        Reference = 2;
                                        Regex = "[aA]long|[tT]ogether";});
             (3, {Type = Word;
                  Reference = 3;
                  Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "advmod";}; {Type = Relation;
                                            From = 2;
                                            To = 3;
                                            Regex = "prep_with";}];
         From = 1;
         To = 3;
         RelationName = "prep_{w2}_with";};
        {Properties = Map.ofList <| [(1, {Type = Word;
                                Reference = 1;
                                Regex = ".*";}); (2, {Type = Word;
                                                      Reference = 2;
                                                      Regex = "with";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "prepc_along|prepc_together";}];
         From = 1;
         To = 2;
         RelationName = "{r1_2}_with";};
        {Properties =
          Map.ofList <|
            [(1, {Type = Word;
                  Reference = 1;
                  Regex = ".*";}); (2, {Type = Word;
                                        Reference = 2;
                                        Regex = "out|off";}); (3, {Type = Word;
                                                                   Reference = 3;
                                                                   Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "prt";}; {Type = Relation;
                                         From = 1;
                                         To = 3;
                                         Regex = "prep_of";}];
         From = 1;
         To = 3;
         RelationName = "prep_{w2}_of";};
        {Properties = Map.ofList <| [(1, {Type = Word;
                                Reference = 1;
                                Regex = ".*";}); (2, {Type = Word;
                                                      Reference = 2;
                                                      Regex = "of";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "prepc_as|prepc_out";}];
         From = 1;
         To = 2;
         RelationName = "{r1_2}_of";};
        {Properties =
          Map.ofList <|
            [(1, {Type = Word;
                  Reference = 1;
                  Regex = ".*";}); (2, {Type = Word;
                                        Reference = 2;
                                        Regex = ".*";}); (3, {Type = Word;
                                                              Reference = 3;
                                                              Regex = "of|to";});
             (4, {Type = Word;
                  Reference = 4;
                  Regex = ".*";})];
         Relations =
          [{Type = Relation;
            From = 1;
            To = 2;
            Regex = "prep_of|prep_to";}; {Type = Relation;
                                          From = 1;
                                          To = 4;
                                          Regex = "mwe_helper";}; {Type = Relation;
                                                                   From = 3;
                                                                   To = 4;
                                                                   Regex = "mwe";}];
         From = 1;
         To = 2;
         RelationName = "prep_{w4}_{w3}";};
        {Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "prepc_.*";}; {Type = Relation;
                                              From = 2;
                                              To = 3;
                                              Regex = "pcomp";}];
         From = 1;
         To = 3;
         RelationName = "pcomp";};
        {Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "prep[c]?_by";}; {Type = DurableRelation;
                                                 From = 1;
                                                 To = 3;
                                                 Regex = "auxpass";}];
         From = 1;
         To = 2;
         RelationName = "agent";};
        {Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = DurableRelation;
                       From = 1;
                       To = 2;
                       Regex = "vmod";}; {Type = Relation;
                                          From = 2;
                                          To = 3;
                                          Regex = "prep_by";}];
         From = 2;
         To = 3;
         RelationName = "agent";};
        {Properties =
          Map.ofList <|
            [(1, {Type = Word;
                  Reference = 1;
                  Regex = "[fF]rom";}); (2, {Type = Word;
                                             Reference = 2;
                                             Regex = ".*";}); (3, {Type = Word;
                                                                   Reference = 3;
                                                                   Regex = ".*";});
             (4, {Type = Word;
                  Reference = 4;
                  Regex = ".*";})];
         Relations = [{Type = Relation;
                       From = 1;
                       To = 2;
                       Regex = "prep_to";}; {Type = DurableRelation;
                                             From = 3;
                                             To = 4;
                                             Regex = "prep_from";}];
         From = 3;
         To = 2;
         RelationName = "prep_to";}];};
     {Id = 4;
      Rules =
       [{Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = DurableRelation;
                       From = 1;
                       To = 2;
                       Regex = "dobj|nsubj";}; {Type = DurableRelation;
                                                From = 2;
                                                To = 3;
                                                Regex = "conj.*";}];
         From = 1;
         To = 3;
         RelationName = "{d1_2}";};
        {Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = DurableRelation;
                       From = 1;
                       To = 2;
                       Regex = "prep.*";}; {Type = DurableRelation;
                                            From = 2;
                                            To = 3;
                                            Regex = "conj.*";}];
         From = 1;
         To = 3;
         RelationName = "{d1_2}";};
        {Properties =
          Map.ofList <| [(1, {Type = Word;
                    Reference = 1;
                    Regex = ".*";}); (2, {Type = Word;
                                          Reference = 2;
                                          Regex = ".*";}); (3, {Type = Word;
                                                                Reference = 3;
                                                                Regex = ".*";})];
         Relations = [{Type = DurableRelation;
                       From = 1;
                       To = 2;
                       Regex = "nsubj";}; {Type = DurableRelation;
                                           From = 2;
                                           To = 3;
                                           Regex = "conj.*";}];
         From = 3;
         To = 2;
         RelationName = "{d1_2}";}];}]


//--------------------------------------------------------------
// API v1
//--------------------------------------------------------------

type OLDSpecifierType = Word | POS | Lemma | Relation | DurableRelation

type Specifier = 
    {
        /// The type the pattern is applied to
        Type : OLDSpecifierType
        /// TheOLDSpecifierTypeer used to reference a matched word
        Reference : int option
        /// The reference number of the word specifier of the start word and end word
        FromTo : (int * int) option
        /// The regular expression pattern
        Regex : string
    }

type OldRule =
    {
        Specs : Specifier list
        /// The reference number of the word the new dependency orginates from
        From : int
        /// The reference number of the word the new dependency goes to
        To : int
        /// Arbitrary label of the relation name; it can contain the word,pos,or lemma matched, e.g. "relationName:the_Pos_tag_is_{p1}".
        RelationName : string
    }

type OldStage =
    {
        Id : int
        Rules : OldRule list
    }

let ( OLDSstageRuleList : OldStage list) = 
    [
        //STAGE 1
        {
            Id = 1
            Rules = 
                [
                //	##prep adjusting Rule
                //	w1:.*
                //	d1_2:prep
                //	w2:.*
                //	r2_3:prep
                //	w3:.*
                //	relationName:prep
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(1,2); Regex = "prep" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                                { Type = Relation        ; Reference = None; FromTo = Some(2,3); Regex = "prep" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "prep"
                    };
                //	##remove as well as Rule
                //	w1:well
                //	r1_2:.*
                //	w2:as
                //	relationName:void
                //	from:1
                //	to:2
                   {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = "well" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = ".*" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = "as" }
                            ]
                        From = 1
                        To = 2
                        RelationName = "void"
                    };
                //	##remove xcomp Rule
                //	w1:.*
                //	r1_2:xcomp
                //	d1_2:ccomp
                //	w2:.*
                //	relationName:void
                //	from:1
                //	to:2
                   {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation        ; Reference = None; FromTo = Some(1,2); Regex = "xcomp" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(1,2); Regex = "ccomp" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 2
                        RelationName = "void"
                    };
                //	##adjust conj Rule
                //	w1:.*
                //	d1_2:prep
                //	w2:.*
                //	r2_3:conj
                //	w3:.*
                //	relationName:conj
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(1,2); Regex = "prep" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                                { Type = Relation        ; Reference = None; FromTo = Some(2,3); Regex = "conj" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "conj"
                    };   
                //	##adjust prep Rule
                //	w1:.*
                //	d1_2:prep
                //	w2:.*
                //	d2_3:conj
                //	w3:.*
                //	relationName:prep
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(1,2); Regex = "prep" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(2,3); Regex = "conj" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "prep"
                    };   
                //	##remove mwe Rule
                //	w1:.*
                //	d1_2:cc
                //	w2:.*
                //	r2_3:mwe
                //	w3:.*
                //	relationName:void
                //	from:1
                //	to:2
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(1,2); Regex = "cc" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(2,3); Regex = "mwe" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 2
                        RelationName = "void"
                    };   
                //	##adjust mwe Rule
                //	w1:.*
                //	d1_2:prep
                //	p2:IN|TO|VBG
                //	d2_3:pobj
                //	w3:.*
                //	d2_4:mwe
                //	w4:.*
                //	relationName:mwe_helper
                //	from: 1
                //	to: 4
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(1,2); Regex = "prep" }
                                { Type = POS ; Reference = Some(2); FromTo = None; Regex = "IN|TO|VBG" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(2,3); Regex = "pobj" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(2,4); Regex = "mwe" }
                                { Type = Word ; Reference = Some(4); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 4
                        RelationName = "mwe_helper"
                    }; 
                //	##remove parataxis Rule
                //	w1:.*
                //	r1_2:parataxis
                //	d1_2:^(?!parataxis|tmod$).*
                //	w2:.*
                //	relationName:void
                //	from:1
                //	to:2
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ;        Reference = None; FromTo = Some(1,2); Regex = "parataxis" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(1,2); Regex = "^(?!parataxis|tmod$).*" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 2
                        RelationName = "void"
                    }; 
                //	##remove tmod Rule
                //	w1:.*
                //	r1_2:tmod
                //	d1_2:parataxis
                //	w2:.*
                //	relationName:void
                //	from:1
                //	to:2
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ;        Reference = None; FromTo = Some(1,2); Regex = "tmod" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(1,2); Regex = "parataxis" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 2
                        RelationName = "void"
                    }; 
                ]
        };
        //STAGE 2
        {
            Id = 2
            Rules = 
                [
                //	##next_to Rule
                //	w1:.*
                //	r1_2:.*
                //	w2:next
                //	r2_3:.*
                //	w3:to
                //	r3_4:pobj
                //	w4:.*
                //	relationName:prep_{w2}_to
                //	from: 1
                //	to: 4
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = ".*" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = "next" }
                                { Type = Relation ; Reference = None; FromTo = Some(2,3); Regex = ".*" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = "to" }
                                { Type = Relation ; Reference = None; FromTo = Some(3,4); Regex = "pobj" }
                                { Type = Word ; Reference = Some(4); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 4
                        RelationName = "prep_{w2}_to"
                    }; 
                //	##prep_in Rule
                //	w1:.*
                //	d1_2:prep
                //	p2:IN|TO|VBG
                //	d2_3:pobj
                //	w3:.*
                //	relationName:prep_{w2}
                //	from: 1
                //	to: 3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(1,2); Regex = "prep" }
                                { Type = POS ; Reference = Some(2); FromTo = None; Regex = "IN|TO|VBG" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(2,3); Regex = "pobj" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "prep_{w2}"
                    }; 
                //	##such_as Rule
                //	w1:.*
                //	r1_2:prep
                //	w2:as
                //	r2_3:pobj
                //	w3:.*
                //	r2_4:mwe
                //	w4:such
                //	relationName:prep_such_as
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "prep" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = "as" }
                                { Type = Relation ; Reference = None; FromTo = Some(2,3); Regex = "pobj" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(2,4); Regex = "mwe" }
                                { Type = Word ; Reference = Some(4); FromTo = None; Regex = "such" }
                            ]
                        From = 1
                        To = 3 //TODO: not 4?
                        RelationName = "prep_such_as"
                    }; 
                //	##punct Rule
                //	w1:.*
                //	d1_2:prep
                //	p2:IN|TO
                //	r2_3:pobj
                //	w3:.*
                //	r2_4:punct
                //	w4:.*
                //	relationName:punct
                //	from:1
                //	to:4
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(1,2); Regex = "prep" }
                                { Type = POS ; Reference = Some(2); FromTo = None; Regex = "IN|TO" }
                                { Type = Relation ; Reference = None; FromTo = Some(2,3); Regex = "pobj" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(2,4); Regex = "punct" }
                                { Type = Word ; Reference = Some(4); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 4
                        RelationName = "punct"
                    }; 
                //	##prepc_in Rule
                //	w1:.*
                //	r1_2:prep
                //	p2:IN|TO
                //	r2_3:pcomp
                //	w3:.*
                //	relationName:prepc_{w2}
                //	from: 1
                //	to: 3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "prep" }
                                { Type = POS ; Reference = Some(2); FromTo = None; Regex = "IN|TO" }
                                { Type = Relation ; Reference = None; FromTo = Some(2,3); Regex = "pcomp" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "prepc_{w2}"
                    }; 
                //	##conj_and Rule
                //	w1:.*
                //	r1_2:cc
                //	r1_3:conj
                //	w2:and|or|but|nor|in|only|as|at|vs\.|&|versus|and/or
                //	w3:.*
                //	relationName:conj_{w2}
                //	from: 1
                //	to: 3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "cc" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,3); Regex = "conj" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = "and|or|but|nor|in|only|as|at|vs\.|&|versus|and/or" } //TODO: remove nor?
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "conj_{w2}"
                    }; 
                //	##negcc Rule
                //	w1:.*
                //	r1_2:cc
                //	r1_3:conj
                //	w2:not|instead|rather
                //	w3:.*
                //	relationName:conj_negcc
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "cc" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,3); Regex = "conj" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = "not|instead|rather" } //TODO: nor?
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "conj_negcc"
                    };
                //	##remove Rule
                //	w1:.*
                //	r1_2:xsubj|ref|possessive
                //	w2:.*
                //	relationName:void
                //	from:1
                //	to:2
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "xsubj|ref|possessive" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 2
                        RelationName = "void"
                    };
                //	##advmod Rule
                //	w1:.*
                //	d1_2:prep
                //	w2:.*
                //	r2_3:advmod
                //	w3:.*
                //	relationName:advmod
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(1,2); Regex = "prep" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                                { Type = Relation        ; Reference = None; FromTo = Some(2,3); Regex = "advmod" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "advmod"
                    };
                //	##dep Rule
                //	w1:.*
                //	d1_2:prep
                //	w2:.*
                //	r2_3:dep
                //	w3:.*
                //	relationName:dep
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(1,2); Regex = "prep" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                                { Type = Relation        ; Reference = None; FromTo = Some(2,3); Regex = "dep" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "dep"
                    };
                //	##owing Rule
                //	w1:.*
                //	r1_2:prep
                //	w2:.*
                //	r2_3:pcomp
                //	w3:to
                //	relationName:prepc_{w2}_{w3}
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "prep" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(2,3); Regex = "pcomp" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = "to" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "prepc_{w2}_{w3}"
                    };
                ]
        };
        //STAGE 3
        {
            Id = 3
            Rules = 
                [
                //	##adjust prep_as (such_as) Rule
                //	w1:.*
                //	r1_2:prep_as
                //	w2:such
                //	relationName:prep_such_as
                //	from:1
                //	to:2
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "prep_as" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = "such" }
                            ]
                        From = 1
                        To = 2
                        RelationName = "prep_such_as"
                    };
                //	##remove prep_ Rule
                //	w1:.*
                //	r1_2:prep_[^_]+
                //	w2:.*
                //	d1_2:prep_[^_]+_.+
                //	relationName:void
                //	from:1
                //	to:2
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation        ; Reference = None; FromTo = Some(1,2); Regex = "prep_[^_]+" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(1,2); Regex = "prep_[^_]+_.+" }
                            ]
                        From = 1
                        To = 2
                        RelationName = "void"
                    };
                //	##adjust conj_&
                //	w1:.*
                //	r1_2:conj_&
                //	w2:.*
                //	relationName:conj_and
                //	from:1
                //	to:2
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "conj_&" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 2
                        RelationName = "conj_and"
                    };
                //	##adjust conj_and
                //	w1:.*
                //	r1_3:conj_and
                //	w2:.*
                //	r1_2:pobj
                //	w3:.*
                //	r3_4:pobj
                //	w4:.*
                //	relationName:conj_and
                //	from:2
                //	to:4
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,3); Regex = "conj_and" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "pobj" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(3,4); Regex = "pobj" }
                                { Type = Word ; Reference = Some(4); FromTo = None; Regex = ".*" }
                            ]
                        From = 2
                        To = 4
                        RelationName = "conj_and"
                    };
                //	##remove pobj Rule
                //	w1:.*
                //	r1_2:prep
                //	p2:IN|TO|VBG
                //	r2_3:pobj
                //	w3:.*
                //	relationName:void
                //	from: 1
                //	to: 3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "prep" }
                                { Type = POS ; Reference = Some(2); FromTo = None; Regex = "IN|TO|VBG" }
                                { Type = Relation ; Reference = None; FromTo = Some(2,3); Regex = "pobj" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "void"
                    };
                //	##remove prep Rule
                //	w1:.*
                //	r1_2:prep
                //	w2:.*
                //	r2_3:advmod
                //	w3:.*
                //	relationName:void
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "prep" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(2,3); Regex = "advmod" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "void"
                    };

                //	##adjust pobj Rule
                //	w1:.*
                //	d1_2:prep[c]?_.+
                //	w2:to|of|with
                //	r2_3:pobj
                //	w3:.*
                //	relationName:pobj
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(1,2); Regex = "prep[c]?_.+" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = "to|of|with" }
                                { Type = Relation        ; Reference = None; FromTo = Some(2,3); Regex = "pobj" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "pobj"
                    };
                //	##remove conj_and Rule
                //	w1:.*
                //	r1_2:cc
                //	w2:.*
                //	d1_3:conj_and
                //	w3:.*
                //	relationName:void
                //	from:1
                //	to:2
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation        ; Reference = None; FromTo = Some(1,2); Regex = "cc" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(1,3); Regex = "conj_and" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 2
                        RelationName = "void"
                    };
                //	##prep_out_of Rule
                //	p1:CD
                //	r1_2:advmod
                //	w2:.*
                //	r1_3:prep_of
                //	w3:.*
                //	relationName:prep_{w2}_of
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = POS ; Reference = Some(1); FromTo = None; Regex = "CD" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "advmod" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,3); Regex = "prep_of" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "prep_{w2}_of"
                    };
                //	##prep_*_of Rule
                //	w1:.*
                //	r1_2:advmod
                //	w2:.*
                //	r2_3:prepc_of
                //	w3:.*
                //	relationName:prepc_{w2}_of
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "advmod" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(2,3); Regex = "prepc_of" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "prepc_{w2}_of"
                    };
                //	##prep_*_to Rule
                //	w1:.*
                //	r1_2:advmod
                //	w2:.*
                //	r1_3:prep_to
                //	w3:.*
                //	relationName:prep_{w2}_to
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "advmod" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,3); Regex = "prep_to" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "prep_{w2}_to"
                    };
                //	##prep_*_to2 Rule
                //	w1:.*
                //	r1_2:advmod
                //	w2:.*
                //	r2_3:prep_to
                //	w3:.*
                //	relationName:prep_{w2}_to
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "advmod" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(2,3); Regex = "prep_to" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "prep_{w2}_to"
                    };
                //	##according_to Rule
                //	w1:.*
                //	r1_2:.*
                //	w2:[aA]ccording|[dD]ue
                //	r2_3:prep_to
                //	w3:.*
                //	relationName:prep_{w2}_to
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = ".*" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = "[aA]ccording|[dD]ue" }
                                { Type = Relation ; Reference = None; FromTo = Some(2,3); Regex = "prep_to" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "prep_{w2}_to" 
                    };
                //	##in_addition_to Rule
                //	w1:.*
                //	r1_2:prep_in
                //	w2:addition
                //	r2_3:prep_to
                //	w3:.*
                //	relationName:prep_in_addition_to
                //	from: 1
                //	to: 3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "prep_in" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = "addition" }
                                { Type = Relation ; Reference = None; FromTo = Some(2,3); Regex = "prep_to" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "prep_in_addition_to" 
                    };
                //	##in_front_of Rule
                //	w1:.*
                //	r1_2:prep_in
                //	w2:front
                //	r2_3:prep_of
                //	w3:.*
                //	relationName:prep_in_front_of
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "prep_in" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = "addition" }
                                { Type = Relation ; Reference = None; FromTo = Some(2,3); Regex = "prep_of" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "prep_in_front_of" 
                    };
                //	##along_with Rule
                //	w1:.*
                //	r1_2:advmod
                //	w2:[aA]long|[tT]ogether
                //	r2_3:prep_with
                //	w3:.*
                //	relationName:prep_{w2}_with
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "advmod" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = "[aA]long|[tT]ogether" }
                                { Type = Relation ; Reference = None; FromTo = Some(2,3); Regex = "prep_with" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "prep_{w2}_with" 
                    };
                //	##along_with Rule (prepc)
                //	w1:.*
                //	r1_2:prepc_along|prepc_together
                //	w2:with
                //	relationName:{r1_2}_with
                //	from:1
                //	to:2
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "prepc_along|prepc_together" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = "with" }
                            ]
                        From = 1
                        To = 2
                        RelationName = "{r1_2}_with" 
                    };
                //	##out_of Rule
                //	w1:.*
                //	r1_2:prt
                //	w2:out|off
                //	r1_3:prep_of
                //	w3:.*
                //	relationName:prep_{w2}_of
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "prt" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = "out|off" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,3); Regex = "prep_of" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "prep_{w2}_of" 
                    };
                //	##as_of Rule (prepc)
                //	w1:.*
                //	r1_2:prepc_as|prepc_out
                //	w2:of
                //	relationName:{r1_2}_of
                //	from:1
                //	to:2
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "prepc_as|prepc_out" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = "of" }
                            ]
                        From = 1
                        To = 2
                        RelationName = "{r1_2}_of" 
                    };
                //	##because_of Rule
                //	w1:.*
                //	r1_2:prep_of|prep_to
                //	w2:.*
                //	w3:of|to
                //	r1_4:mwe_helper
                //	r3_4:mwe
                //	w4:.*
                //	relationName:prep_{w4}_{w3}
                //	from:1
                //	to:2
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "prep_of|prep_to" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = "of|to" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,4); Regex = "mwe_helper" }
                                { Type = Relation ; Reference = None; FromTo = Some(3,4); Regex = "mwe" }
                                { Type = Word ; Reference = Some(4); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 2
                        RelationName = "prep_{w4}_{w3}"
                    };
                //	##pcomp Rule
                //	w1:.*
                //	r1_2:prepc_.*
                //	w2:.*
                //	r2_3:pcomp
                //	w3:.*
                //	relationName:pcomp
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "prepc_.*" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }
                                { Type = Relation ; Reference = None; FromTo = Some(2,3); Regex = "pcomp" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "pcomp"
                    };
                //	##agent Rule (auxpass)
                //	w1:.*
                //	r1_2:prep[c]?_by
                //	d1_3:auxpass
                //	w2:.*
                //	w3:.*
                //	relationName:agent
                //	from:1
                //	to:2
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = Relation        ; Reference = None; FromTo = Some(1,2); Regex = "prep[c]?_by" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(1,3); Regex = "auxpass" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }                                
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 2
                        RelationName = "agent"
                    };
                //	##agent Rule (vmod)
                //	w1:.*
                //	d1_2:vmod
                //	w2:.*
                //	r2_3:prep_by
                //	w3:.*
                //	relationName:agent
                //	from:2
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation ; Reference = None; FromTo = Some(1,2); Regex = "vmod" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }                        
                                { Type = Relation        ; Reference = None; FromTo = Some(2,3); Regex = "prep_by" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 2
                        To = 3
                        RelationName = "agent"
                    };
                //	##prep_to Rule
                //	w1:[fF]rom
                //	r1_2:prep_to
                //	w2:.*
                //	d3_4:prep_from
                //	w3:.*
                //	w4:.*
                //	relationName:prep_to
                //	from:3
                //	to:2
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = "[fF]rom" }
                                { Type = Relation ; Reference = None; FromTo = Some(1,2); Regex = "prep_to" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }                        
                                { Type = DurableRelation; Reference = None; FromTo = Some(3,4); Regex = "prep_from" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                                { Type = Word ; Reference = Some(4); FromTo = None; Regex = ".*" }
                            ]
                        From = 3
                        To = 2
                        RelationName = "prep_to"
                    };
                ]
        };
        //STAGE 4
        {
            Id = 4
            Rules = 
                [
                //	##cc process
                //	w1:.*
                //	d1_2:dobj|nsubj
                //	w2:.*
                //	d2_3:conj.*
                //	w3:.*
                //	relationName:{d1_2}
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation; Reference = None; FromTo = Some(1,2); Regex = "dobj|nsubj" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }                        
                                { Type = DurableRelation; Reference = None; FromTo = Some(2,3); Regex = "conj.*" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "{d1_2}"
                    };
                //	##cc process
                //	w1:.*
                //	d1_2:prep.*
                //	w2:.*
                //	d2_3:conj.*
                //	w3:.*
                //	relationName:{d1_2}
                //	from:1
                //	to:3
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation; Reference = None; FromTo = Some(1,2); Regex = "prep.*" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }                        
                                { Type = DurableRelation; Reference = None; FromTo = Some(2,3); Regex = "conj.*" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 1
                        To = 3
                        RelationName = "{d1_2}"
                    };
                //	##cc process
                //	w1:.*
                //	d1_2:nsubj
                //	w2:.*
                //	d1_3:conj.*
                //	w3:.*
                //	relationName:{d1_2}
                //	from:3
                //	to:2
                    {   
                        Specs = 
                            [
                                { Type = Word ; Reference = Some(1); FromTo = None; Regex = ".*" }
                                { Type = DurableRelation; Reference = None; FromTo = Some(1,2); Regex = "nsubj" }
                                { Type = Word ; Reference = Some(2); FromTo = None; Regex = ".*" }                        
                                { Type = DurableRelation; Reference = None; FromTo = Some(2,3); Regex = "conj.*" }
                                { Type = Word ; Reference = Some(3); FromTo = None; Regex = ".*" }
                            ]
                        From = 3
                        To = 2
                        RelationName = "{d1_2}"
                    };
                ]
        }
    ]

/// Converts v1 API attempt to v2. Potentially useful for future conversions
/// NOTE: loses the nice comments from jobimtext TODO: interleave those
let ConvertOLDStageRuleList() =
    let (newStageList : Stage list) = 
        OLDSstageRuleList
        |> List.map( fun stage ->
            let newRules =
                stage.Rules
                |> List.map( fun rule ->
                    let properties = 
                        rule.Specs 
                        |> List.filter( fun s -> s.Reference.IsSome )
                        |> List.map( fun s -> 
                            s.Reference.Value,{
                                Type = match s.Type with | Word -> SpecifierType.Word | POS -> SpecifierType.POS | Lemma -> SpecifierType.Lemma
                                Reference = s.Reference.Value
                                Regex = s.Regex
                            }
                        )
                        |> Map.ofList
                    let relations = 
                        rule.Specs 
                        |> List.filter( fun s -> s.FromTo.IsSome )
                        |> List.map( fun s -> 
                            {
                                Type = match s.Type with | Relation -> RelationType.Relation |DurableRelation -> RelationType.DurableRelation
                                From = s.FromTo.Value |> fst
                                To = s.FromTo.Value |> snd
                                Regex = s.Regex
                            }
                        )
                    {
                        Properties = properties;
                        Relations = relations
                        From = rule.From
                        To = rule.To
                        RelationName = rule.RelationName
                    }
                )
            { Id=stage.Id; Rules=newRules }
        )
    System.IO.File.WriteAllText("rules-new.txt", (sprintf "%A" newStageList ).Replace("map", "Map.ofList <|") )