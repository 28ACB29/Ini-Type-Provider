namespace IniTypedProvider

open Microsoft.FSharp.Quotations
open System

open Defaults
open DesignMerge
open Parsing
open RuntimeParse

module Getters =

    let getterExpr (info:KeyDesignInfo) (docExpr:Expr):Expr =
        let section:string = info.Section
        let key:string = info.Key
        let t:Type = info.InferredType
        let propType:Type = propertyType info
        let isOption:bool =
            propType.IsGenericType &&
            propType.GetGenericTypeDefinition() = typedefof<option<_>>

        let defaultObjOpt:objnull option =
            info.TypedDefault |> Option.map boxDefault

        let blockDefaultObjOpt:objnull option =
            info.BlockDefault
            |> Option.bind (fun (raw:string) ->
                if t = typeof<string> then Some (box raw)
                elif t = typeof<bool> then parseBool raw |> Option.map box
                else parseNumeric t raw)

        if not isOption then
            <@@
                let document:IniRuntimeDocument = %%docExpr : IniRuntimeDocument
                match document.TryGet(section, key) with
                | Some (raw:string) ->
                    if t = typeof<string> then box raw
                    elif t = typeof<bool> then
                        match parseBool raw with
                        | Some (v:bool) -> box v
                        | None ->
                            match defaultObjOpt, blockDefaultObjOpt with
                            | Some (d:objnull), _ -> d
                            | None, Some (d:objnull) -> d
                            | None, None -> failwithf "Invalid bool for %s.%s" section key
                    else
                        match parseNumeric t raw with
                        | Some (v:objnull) -> v
                        | None ->
                            match defaultObjOpt, blockDefaultObjOpt with
                            | Some (d:objnull), _ -> d
                            | None, Some (d:objnull) -> d
                            | None, None -> failwithf "Invalid numeric for %s.%s" section key
                | None ->
                    match defaultObjOpt, blockDefaultObjOpt with
                    | Some (d:objnull), _ -> d
                    | None, Some (d:objnull) -> d
                    | None, None -> failwithf "Missing required key %s.%s" section key
            @@>
        else
            <@@
                let document:IniRuntimeDocument = %%docExpr : IniRuntimeDocument
                match document.TryGet(section, key) with
                | Some (raw:string) ->
                    if t = typeof<string> then Some (box raw)
                    elif t = typeof<bool> then parseBool raw |> Option.map box
                    else parseNumeric t raw
                | None -> None
            @@>