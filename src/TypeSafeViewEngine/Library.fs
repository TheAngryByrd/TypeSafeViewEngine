namespace TypeSafeViewEngine
open System
open System.Security.Cryptography
open System.Text

open Giraffe.ViewEngine
open TypeShape
open TypeShape.Core
open TypeShape.Core.SubtypeExtensions

[<AutoOpen>]
module Lib =

    type Regex = Regex of string
    module Regex =
        let isMatch (Regex pattern) input =
            RegularExpressions.Regex.IsMatch(input, pattern)
    module Paths =
        open Microsoft.FSharp.Quotations

        let namePathRegex userExpr =
            let rec innerLoop expr state =
                match expr with
                |Patterns.Lambda(_, body) ->
                    innerLoop body state
                |Patterns.PropertyGet(Some parent, propInfo, []) ->
                    sprintf "%s%s" propInfo.Name state  |> innerLoop parent
                |Patterns.Call (None, _, expr1::[Patterns.Let (v, expr2, _)]) when v.Name = "mapping"->
                    let parentPath = innerLoop expr1 "\[\d\]"
                    let childPath = innerLoop expr2 ""
                    sprintf "%s\[%s\]" parentPath childPath
                |ExprShape.ShapeVar x ->
                    state
                |_ ->
                    failwithf "Unsupported expression: %A" expr
            innerLoop userExpr "" |> sprintf "%s" |> Regex

        type Path =
            static member Make([<ReflectedDefinition(true)>] f:Expr<'T -> 'R>) =
                match f with
                |Patterns.WithValue(f, _, expr) ->
                    namePathRegex expr
                | _ -> failwith "Unexpected argument"

    // maybe use this instead of pattern matching?
    type IRenderer =
        abstract CanRender : 't -> string -> bool
        abstract Render : 't -> string -> XmlNode

    /// <summary>
    /// The "path" to a particular field. This allows for nested attributes and arrays that can be specified by naming fields with the syntax: name="attr[nested][nested]"
    /// </summary>
    type NamePath = string

    /// <summary>
    /// The name of the Field from the record/class. Not to be confused with <see cref="T:TypeSafeViewEngine.Lib.NamePath"/>
    /// </summary>
    type FieldName = string

    type RenderConfig = {
        BoolType : bool -> NamePath -> FieldName -> XmlNode
        StringType : string -> NamePath -> FieldName -> XmlNode
        HiddenType : string -> NamePath -> FieldName -> XmlNode
        CustomField : Map<Regex, obj -> NamePath -> FieldName -> XmlNode>
        WholeForm : XmlNode list -> XmlNode
    }

        with
            member x.WithCustomRender name renderer =
                {
                    x with
                        CustomField = x.CustomField |> Map.add name renderer
                }


    let rec private editFor'<'t> (value: 't) (namePath : NamePath) (fieldName: FieldName) (config : RenderConfig) (depth : int) : XmlNode =
        /// Generates a property setter for a particular 'declType that sets a property of type 'field.
        /// This is used in record/class generation to iterate through the fields or properties of the type and
        /// seed them with default data of their matching types
        let inputFor (shape: IShapeMember<'declType>) =
            shape.Accept { new IMemberVisitor<'declType, 'declType -> string -> string -> XmlNode> with
                member __.Visit (shape : ShapeMember<'declType, 'field>) =
                        fun declTypeInstance namePath name -> editFor'<'field>(shape.Get declTypeInstance) namePath name config (depth + 1)
            }
        let fieldConfigs = config.CustomField |> Seq.map Operators.(|KeyValue|)
        match fieldConfigs |> Seq.tryFind(fun (pattern, _) -> Regex.isMatch pattern namePath) |> Option.map snd   with
        | Some renderer -> renderer value namePath fieldName
        | None ->

            match shapeof<'t> with
            | Shape.Bool ->
                config.BoolType (unbox value) namePath fieldName
            | Shape.String ->
                config.StringType (unbox value) namePath fieldName
              // to make a record, we generate setters for the fields of the record and then run them against an empty record instance
            | Shape.FSharpRecord (:? ShapeFSharpRecord<'t> as shape) ->
                let fields = shape.Fields |> Array.map (fun f -> f.Label, inputFor f value)
                [
                    for name, htmlFunc in fields do
                        let namePath =
                            if depth > 0 then
                                $"{namePath}[{name}]"
                            else
                                name

                        htmlFunc namePath name
                ]
                |> config.WholeForm
            | Shape.Enumerable (e)  ->
                e.Element.Accept { new ITypeVisitor<XmlNode> with
                    member _.Visit<'t> () =
                        let nodes = (unbox value) |> List.mapi (fun i v ->
                            let namePath = $"{namePath}[{i}]"
                            editFor'<'t> v namePath fieldName config (depth + 1))
                        div [] nodes
                    }

            | unknownShape  -> failwithf "unhandled shape %A" unknownShape

    let editFor<'t> (value: 't) (fieldName: FieldName) (config : RenderConfig) = editFor'<'t> value "" fieldName config 0


    let boolField (v: bool) namePath name =
        div [] [
            label [_for namePath] [str name]
            input [_name namePath; _type "checkbox"; if v then _checked; _data "toggle" "toggle"]
        ]

    let textField (v: string) namePath name =
        div [] [
            label [_for namePath] [str name]
            input [_name namePath; _type "text"; _value v]
        ]

    let hiddenField (v: string) namePath name =
        input [_name namePath; _type "hidden"; _value v]

    let formRender nodes =
        div [] nodes

    let defaultRenderConfig = {
        BoolType = boolField
        StringType = textField
        HiddenType = hiddenField
        CustomField = Map.empty
        WholeForm = formRender
    }
