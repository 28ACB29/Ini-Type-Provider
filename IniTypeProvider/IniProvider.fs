namespace IniTypedProvider

open System
open System.IO
open Microsoft.FSharp.Core.CompilerServices
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Quotations

open IniDesign
open DesignMerge
open RuntimeLoad
open Getters
open Defaults

[<TypeProvider>]
type IniProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces (config)

    let ns  = "IniTyped"
    let asm = System.Reflection.Assembly.GetExecutingAssembly()

    let root = ProvidedTypeDefinition(asm, ns, "IniProvider", Some typeof<obj>)

    do
        let pathParam =
            ProvidedStaticParameter("Path", typeof<string>)

        let defaultsTextParam =
            ProvidedStaticParameter("DefaultsText", typeof<string>, "")

        root.DefineStaticParameters(
            parameters = [ pathParam; defaultsTextParam ],
            instantiationFunction = fun typeName args ->
                let path = args.[0] :?> string
                let defaultsText = args.[1] :?> string

                let fullPath =
                    if Path.IsPathRooted path then path
                    else Path.Combine(config.ResolutionFolder, path)

                let ini = parseIniFile fullPath
                let defaultsBlock =
                    if String.IsNullOrWhiteSpace defaultsText then Map.empty
                    else parseDefaultsText defaultsText

                let emptyDefaults : Map<string * string, DefaultValue> = Map.empty
                let keyInfos = buildKeyInfos ini defaultsBlock emptyDefaults

                let secondStage =
                    ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)

                let perKeyParams =
                    keyInfos
                    |> List.map (fun info ->
                        ProvidedStaticParameter(
                            parameterName = $"Default_{info.Section}_{info.Key}",
                            parameterType = typeof<DefaultValue>,
                            parameterDefaultValue = null))

                secondStage.DefineStaticParameters(
                    parameters = perKeyParams,
                    instantiationFunction = fun typeName2 args2 ->

                        // ------------------------------------------------------------
                        // FIXED: perKeyDefaults now uses List.zip and Array.toList
                        // ------------------------------------------------------------
                        let perKeyDefaults =
                            List.zip keyInfos (args2 |> Array.toList)
                            |> List.fold (fun acc (info, arg) ->
                                match arg with
                                | null -> acc
                                | :? DefaultValue as dv ->
                                    acc |> Map.add (info.Section, info.Key) dv
                                | _ -> acc)
                                Map.empty

                        let finalKeyInfos = buildKeyInfos ini defaultsBlock perKeyDefaults

                        let provided =
                            ProvidedTypeDefinition(asm, ns, typeName2, Some typeof<IniRuntimeDocument>, isErased = true)

                        // Root constructor: load document once
                        let ctor =
                            ProvidedConstructor(
                                parameters = [],
                                invokeCode = fun _ -> <@@ loadRuntime fullPath @@>)
                        provided.AddMember ctor

                        // Group keys by section
                        let sections =
                            finalKeyInfos
                            |> Seq.groupBy (fun k -> k.Section)
                            |> Seq.toList

                        for (sectionName, keys) in sections do
                            let secType =
                                ProvidedTypeDefinition(sectionName, Some typeof<obj>, isErased = true)

                            // Section stores reference to root document
                            let docField = ProvidedField("_doc", typeof<IniRuntimeDocument>)
                            secType.AddMember docField

                            let secCtor =
                                ProvidedConstructor(
                                    parameters = [ ProvidedParameter("doc", typeof<IniRuntimeDocument>) ],
                                    invokeCode = fun args ->
                                        Expr.Sequential(
                                            Expr.FieldSet(args.[0], docField, args.[1]),
                                            args.[0]
                                        ))
                            secType.AddMember secCtor

                            // Root exposes section instances
                            let secProp =
                                ProvidedProperty(
                                    sectionName,
                                    secType,
                                    getterCode = fun args ->
                                        let docExpr = args.[0]
                                        Expr.NewObject(secCtor, [ docExpr ])
                                )
                            provided.AddMember secProp

                            // Add key properties
                            for info in keys do
                                let propType = propertyType info
                                let prop =
                                    ProvidedProperty(
                                        info.Key,
                                        propType,
                                        getterCode = fun args ->
                                            let docExpr = Expr.FieldGet(args.[0], docField)
                                            let body = getterExpr info docExpr
                                            Expr.Coerce(body, propType)
                                    )
                                secType.AddMember prop

                            provided.AddMember secType

                        provided
                )

                secondStage
        )

        this.AddNamespace(ns, [ root ])

[<TypeProviderAssembly>]
do ()
