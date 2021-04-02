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

    /// <summary>
    /// The "path" to a particular field. This allows for nested attributes and arrays that can be specified by naming fields with the syntax: name="attr[nested][nested]"
    /// </summary>
    type NamePath = string

    /// <summary>
    /// The name of the Field from the record/class. Not to be confused with <see cref="T:TypeSafeViewEngine.Lib.NamePath"/>
    /// </summary>
    type FieldName = string


    /// <summary>
    /// Single Case union for Regexes
    /// </summary>
    type Regex = Regex of string
    module Regex =
        /// <summary>
        /// Passthrough to <see cref="System.RegularExpressions.Regex.IsMatch"/>
        /// </summary>
        /// <param name="pattern">The regular expression pattern to match.</param>
        /// <param name="input">The string to search for a match.</param>
        /// <returns></returns>
        let isMatch ((Regex p) as pattern) input =
            RegularExpressions.Regex.IsMatch(input, p)

    module Paths =
        open Microsoft.FSharp.Quotations

        let private isNumber t =
            match Type.GetTypeCode t with
            | TypeCode.Byte
            | TypeCode.SByte
            | TypeCode.Int16
            | TypeCode.UInt16
            | TypeCode.Int32
            | TypeCode.UInt32
            | TypeCode.Int64
            | TypeCode.UInt64
                -> true
            | _ -> false

        /// <summary>
        /// Generates a structured <see cref="T:NamePath"/> from the expression.
        ///
        /// Examples:
        /// * `&lt;@ fun (vm : MyModel) -> vm.Name @&gt;`  will generate `Name`
        /// * `&lt;@ fun (vm : MyModel) -> vm.Addresses[0].Zip @&gt;`  will generate `Addresses[0][Zip]`
        /// </summary>
        /// <param name="userExpr">Expression for generating a path</param>
        /// <returns>A NamePath</returns>
        let namePath userExpr : NamePath =
            let rec innerLoop expr state =
                match expr with
                |Patterns.Lambda(_, body) ->
                    innerLoop body state
                |Patterns.PropertyGet(Some parent, propInfo, [Patterns.Value (value, ty)]) when isNumber ty ->
                    sprintf "[%s][%s]" (string value) state  |> innerLoop parent
                |Patterns.PropertyGet(Some parent, propInfo, []) ->
                    sprintf "%s%s" propInfo.Name state  |> innerLoop parent
                |ExprShape.ShapeVar x ->
                    state
                |_ ->
                    failwithf "Unsupported expression: %A" expr
            innerLoop userExpr "" |> sprintf "%s"

        /// <summary>
        ///
        /// Generates a structured <see cref="T:Regex"/> that is able to match against <see cref="T:NamePath"/>
        ///
        /// * `&lt;@ fun (vm : MyModel) -> vm.Name @&gt;`  will generate `Name` which will match the Name field.
        /// * `&lt;@ fun (vm : MyModel) -> vm.Addresses[0].Zip @&gt;`  will generate `Addresses\[0\]\[Zip\]` which will match the first Zip in the list.
        /// * `&lt;@ fun (vm : MyModel) -> vm.Addresses |> Seq.map(fun a -> a.Zip)@&gt;`  will generate `Addresses\[\d\]\[Zip\]` which will match any Zip in this list.
        /// </summary>
        /// <param name="userExpr">Expression for generating a path</param>
        /// <returns>Regex matcing NamePaths</returns>
        let namePathRegex userExpr =
            let rec innerLoop expr state =
                match expr with
                |Patterns.Lambda(_, body) ->
                    innerLoop body state
                |Patterns.PropertyGet(Some parent, propInfo, [Patterns.Value (value, ty)]) when isNumber ty ->
                    sprintf "\[%s\]\[%s\]" (string value) state  |> innerLoop parent
                |Patterns.PropertyGet(Some parent, propInfo, []) ->
                    sprintf "%s%s" propInfo.Name state  |> innerLoop parent
                // This part generates the \d for matching any digit
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
            /// <summary>
            /// A helper for <see cref="namePath"/>
            /// </summary>
            /// <param name="f"></param>
            /// <returns></returns>
            static member MakeNamePath([<ReflectedDefinition(true)>] f:Expr<'T -> 'R>) =
                match f with
                |Patterns.WithValue(f, _, expr) -> namePath expr
                | _ -> namePath f

            /// <summary>
            /// A helper for <see cref="namePathRegex"/>
            /// </summary>
            /// <param name="f"></param>
            /// <returns></returns>
            static member MakeRegex([<ReflectedDefinition(true)>] f:Expr<'T -> 'R>) =
                match f with
                |Patterns.WithValue(f, _, expr) -> namePathRegex expr
                | _ -> namePathRegex f



    // maybe use this instead of pattern matching?
    type IRenderer =
        abstract CanRender : 't -> string -> bool
        abstract Render : 't -> string -> XmlNode

    /// <summary>
    /// Use this to configure the rendering the html for particular types of a ViewModel.
    /// </summary>
    type RenderConfig = {
        BoolType : bool -> NamePath -> FieldName -> XmlNode
        StringType : string -> NamePath -> FieldName -> XmlNode
        IntType : string -> NamePath -> FieldName -> XmlNode
        DoubleType : string -> NamePath -> FieldName -> XmlNode
        GuidType : Guid -> NamePath -> FieldName -> XmlNode
        DateTimeType : DateTime -> NamePath -> FieldName -> XmlNode
        DateTimeOffsetType : DateTimeOffset -> NamePath -> FieldName -> XmlNode
        CustomField : Map<Regex, obj -> NamePath -> FieldName -> XmlNode>
        WholeForm : XmlNode list -> XmlNode
    }

        with
            /// <summary>
            /// Allows injecting custom rendering for a given <see cref="T:Regex"/> that matches a <see cref="T:NamePath"/>.
            /// </summary>
            /// <param name="pattern">The regex to match a given <see cref="T:NamePath"/>.</param>
            /// <param name="renderer">The function the will return custom html. </param>
            /// <returns></returns>
            member x.WithCustomRender(pattern,renderer) =
                { x with CustomField = x.CustomField |> Map.add pattern renderer }
            /// <summary>
            /// Allows injecting custom rendering for a given <see cref="T:Regex"/> that matches a <see cref="T:NamePath"/>.
            /// </summary>
            /// <param name="expr">The expression for matching against a <see cref="T:NamePath"/>. See <see cref="namePathRegex"/> for examples. </param>
            /// <param name="renderer">The function the will return custom html. </param>
            /// <returns></returns>
            member x.WithCustomRender(expr : Quotations.Expr<'T -> 'R>,renderer) =
                x.WithCustomRender(Paths.namePathRegex expr, renderer)



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
            // | Shape.Int16
            | Shape.Int32
            // | Shape.Int64
            // | Shape.UInt16
            // | Shape.UInt32
            // | Shape.UInt64
            // | Shape.Byte
            // | Shape.SByte
                -> config.IntType (string value) namePath fieldName
            // | Shape.Single
            // | Shape.Double
            // | Shape.Decimal ->
            //     config.DoubleType (string value) namePath fieldName
            | Shape.String ->
                config.StringType (unbox value) namePath fieldName
            // | Shape.DateTime ->
            //     config.DateTimeType (unbox value) namePath fieldName
            // | Shape.DateTimeOffset ->
            //     config.DateTimeOffsetType (unbox value) namePath fieldName
            // | Shape.Guid ->
            //     config.GuidType (unbox value) namePath fieldName
            | Shape.Enumerable (e)  ->
                e.Element.Accept { new ITypeVisitor<XmlNode> with
                    member _.Visit<'t> () =
                        let nodes = (unbox value) |> List.mapi (fun i v ->
                            let namePath = $"{namePath}[{i}]"
                            editFor'<'t> v namePath fieldName config (depth + 1))
                        div [] nodes
                    }
            | Shape.CliMutable (:? ShapeCliMutable<'t> as shape) ->
                let fields = shape.Properties |> Array.map (fun f -> f.Label, inputFor f value)
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


            | unknownShape  -> failwithf "unhandled shape %A" unknownShape

    /// <summary>
    /// Generates html input fields based on the structure given in `value`.  Passing a ViewModel (that conforms to CLIMutable) will generate a whole input form.
    /// </summary>
    /// <param name="value">The object to generate an input form for.</param>
    /// <param name="fieldName">The name of the form.</param>
    /// <param name="config">The RenderConfig that allow for customizing of output.</param>
    /// <returns></returns>
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
    let intField (v : string) namePath name =
        div [] [
            label [_for namePath] [str name]
            input [_name namePath; _type "text"; _value v; _step "1"]
        ]
    let doubleField (v : string) namePath name =
        div [] [
            label [_for namePath] [str name]
            input [_name namePath; _type "text"; _value v; _step "any"]
        ]
    let hiddenField (v: string) namePath name =
        input [_name namePath; _type "hidden"; _value v]

    let hiddenFieldGuid (v: Guid) namePath name =
        hiddenField (v.ToString "N") namePath name

    let dateTimeField (v : DateTime) namePath name =
        textField (v.ToString("s")) namePath name

    let dateTimeOffsetField (v : DateTimeOffset) namePath name =
        textField (v.ToString("s")) namePath name

    let formRender nodes =
        div [] nodes

    let defaultRenderConfig = {
        BoolType = boolField
        StringType = textField
        IntType = intField
        DoubleType = doubleField
        GuidType = hiddenFieldGuid
        DateTimeType = dateTimeField
        DateTimeOffsetType = dateTimeOffsetField
        CustomField = Map.empty
        WholeForm = formRender
    }
