namespace IniTypedProvider

open Defaults
open Parsing
open IniDesign

type KeyDesignInfo =
    { Section      : string
      Key          : string
      InferredType : System.Type
      TypedDefault : DefaultValue option
      BlockDefault : string option
      IniValue     : string option }

module DesignMerge =

    let allKeys (ini : Map<string,Map<string,string>>)
                (defaultsBlock : Map<string,Map<string,string>>)
                (perKeyDefaults : Map<string * string, DefaultValue>) =

        let secKeys m =
            m
            |> Map.toSeq
            |> Seq.collect (fun (s, ks) -> ks |> Map.toSeq |> Seq.map (fun (k,_) -> (s,k)))
            |> Set.ofSeq

        Set.unionMany [
            secKeys ini
            secKeys defaultsBlock
            perKeyDefaults |> Map.toSeq |> Seq.map fst |> Set.ofSeq
        ]

    let buildKeyInfos ini defaultsBlock perKeyDefaults =
        allKeys ini defaultsBlock perKeyDefaults
        |> Seq.map (fun (sec,key) ->
            let typedDefaultOpt = perKeyDefaults |> Map.tryFind (sec,key)
            let iniValueOpt =
                ini
                |> Map.tryFind sec
                |> Option.bind (Map.tryFind key)
            let blockDefaultOpt =
                defaultsBlock
                |> Map.tryFind sec
                |> Option.bind (Map.tryFind key)

            let inferredType =
                match typedDefaultOpt, iniValueOpt, blockDefaultOpt with
                | Some dv, _, _ -> Defaults.typeOfDefault dv
                | None, Some raw, _ -> Parsing.inferFromString raw
                | None, None, Some raw -> Parsing.inferFromString raw
                | None, None, None -> typeof<string>

            { Section      = sec
              Key          = key
              InferredType = inferredType
              TypedDefault = typedDefaultOpt
              BlockDefault = blockDefaultOpt
              IniValue     = iniValueOpt })
        |> Seq.toList

    let optionOf t = typedefof<option<_>>.MakeGenericType [| t |]

    let propertyType (info : KeyDesignInfo) =
        match info.TypedDefault, info.BlockDefault, info.IniValue with
        | Some _, _, _ -> info.InferredType
        | None, Some _, _ -> info.InferredType
        | None, None, Some _ -> info.InferredType
        | None, None, None -> optionOf info.InferredType