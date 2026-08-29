namespace IniTypedProvider

open System

open Defaults
open Parsing
open IniDesign

type KeyDesignInfo =
    { Section      : string
      Key          : string
      InferredType : Type
      TypedDefault : DefaultValue option
      BlockDefault : string option
      IniValue     : string option }

module DesignMerge =

    let allKeys (ini : Map<string,Map<string,string>>)
                (defaultsBlock : Map<string,Map<string,string>>)
                (perKeyDefaults : Map<string * string, DefaultValue>) =

        let sectionKeys (m:Map<'a, Map<'b, 'c>>):Set<'a * 'b> =
            m
            |> Map.toSeq
            |> Seq.collect (fun (s:'a, ks:Map<'b, 'c>) -> ks |> Map.toSeq |> Seq.map (fun (k:'b,_) -> (s,k)))
            |> Set.ofSeq

        Set.unionMany [
            sectionKeys ini
            sectionKeys defaultsBlock
            perKeyDefaults
            |> Map.toSeq
            |> Seq.map fst
            |> Set.ofSeq
        ]

    let buildKeyInfos (ini:Map<string, Map<string, string>>) (defaultsBlock:Map<string, Map<string, string>>) (perKeyDefaults:Map<(string * string), DefaultValue>):KeyDesignInfo list =
        allKeys ini defaultsBlock perKeyDefaults
        |> Seq.map (fun (section:string,key:string) ->
            let typedDefaultOpt:DefaultValue option =
                perKeyDefaults
                |> Map.tryFind (section,key)
            let iniValueOpt:string option =
                ini
                |> Map.tryFind section
                |> Option.bind (Map.tryFind key)
            let blockDefaultOpt:string option =
                defaultsBlock
                |> Map.tryFind section
                |> Option.bind (Map.tryFind key)

            let inferredType:Type =
                match typedDefaultOpt, iniValueOpt, blockDefaultOpt with
                | Some dv, _, _ -> Defaults.typeOfDefault dv
                | None, Some raw, _ -> Parsing.inferFromString raw
                | None, None, Some raw -> Parsing.inferFromString raw
                | None, None, None -> typeof<string>

            { Section      = section
              Key          = key
              InferredType = inferredType
              TypedDefault = typedDefaultOpt
              BlockDefault = blockDefaultOpt
              IniValue     = iniValueOpt })
        |> Seq.toList

    let optionOf (t:Type):Type = typedefof<option<_>>.MakeGenericType [| t |]

    let propertyType (info:KeyDesignInfo):Type =
        match info.TypedDefault, info.BlockDefault, info.IniValue with
        | Some _, _, _ -> info.InferredType
        | None, Some _, _ -> info.InferredType
        | None, None, Some _ -> info.InferredType
        | None, None, None -> optionOf info.InferredType