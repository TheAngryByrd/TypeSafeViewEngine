namespace TypeSafeViewEngine.Tests



module StringPatterns =
    open System
    let (|EqualsCI|_|) (s2 : string) (s1 : string) =
        if s1.Equals(s2, StringComparison.InvariantCultureIgnoreCase) then Some ()
        else None

module Json =
    module Converters =
        open StringPatterns
        open Newtonsoft.Json
        type CheckboxConverter() =
            inherit JsonConverter()
            override __.CanConvert t =
                t = typeof<bool>
            override __.ReadJson(reader, value, existingValue, serializer) =
                match reader.TokenType with
                | JsonToken.String ->
                    match reader.Value.ToString() with
                    | EqualsCI "on" -> true |> box
                    | _ -> false |> box
                | _ ->
                    let newserializer = JsonSerializer()
                    for c in serializer.Converters |> Seq.filter(fun c -> typeof<CheckboxConverter> = c.GetType() |> not) do
                        serializer.Converters.Add c
                    newserializer.Deserialize(reader, value)

            override __.WriteJson(writer, value, serializer) =
                writer.WriteValue(unbox<bool>value)


module Tests =

    [<CLIMutableAttribute>]
    type FormStateBool = {
        Enabled : bool
    }


    [<CLIMutableAttribute>]
    type FormStateHidden3 = {
        Name : string
    }


    type FormSimpleNested3 = {
        Foo : string
    }

    type FormSimpleNested2 = {
        Theme : string
        Timezone : string
        Foo : FormSimpleNested3
    }
    type FormSimpleNested = {
        Name : string
        Settings : FormSimpleNested2
    }


    type FormListNested = {
        Name : string
        LotteryNumbers : string list
    }

    type FormListNestedRecord = {
            Name : string
            Foos : FormSimpleNested3 list
        }


    type Address = {
        Street : string
        Zip : string
    }
    [<CLIMutableAttribute>]
    type FormStateHidden4 = {
        Name : string
        // name = "FavoriteRestaurant[Street]"
        FavoriteRestaurant : Address
        // name = "Nicknames[0]"
        Nicknames : string list
        // "Addesses[0][Street]"
        Addesses : Address list
    }
    open Expecto
    open TypeSafeViewEngine
    open Giraffe.ViewEngine

    [<Tests>]
    let namePathGenerationTests =
        testList "NamePath Generation" [
            testList "Regex" [
                testCase "Nested Array Record" <| fun () ->
                    let expected = Regex "Addesses\[\d\]\[Street\]"
                    let actual = Paths.Path.MakeRegex((fun (form : FormStateHidden4) -> form.Addesses |> Seq.map(fun a -> a.Street)))
                    Expect.equal actual expected ""
                testCase "Specific Field in Nested Array Record" <| fun () ->
                    let expected = Regex "Addesses\[0\]\[Street\]"
                    let actual = Paths.Path.MakeRegex((fun (form : FormStateHidden4) -> form.Addesses.[0].Street))
                    Expect.equal actual expected ""
            ]
            testList "NamePath" [
                testCase "Specific Field in Nested Array Record" <| fun () ->
                    let expected = "Addesses[2][Street]"
                    let actual = Paths.Path.MakeNamePath((fun (form : FormStateHidden4) -> form.Addesses.[2].Street))
                    Expect.equal actual expected ""
            ]
        ]

    [<Tests>]
    let unitTests =
        testList "Lib" [
            testCase "bool" <| fun _ ->
                let viewModel = {Enabled = true}
                let xmlNode = editFor viewModel "myForm" defaultRenderConfig
                let expected = """<div><div><label for="Enabled">Enabled</label><input name="Enabled" type="checkbox" checked data-toggle="toggle"></div></div>"""
                let actual = RenderView.AsString.htmlNode xmlNode
                Expect.equal actual expected ""

            testCase "custom input" <| fun _ ->
                let viewModel = {Name = "James Kirk"}
                let pattern = Paths.Path.MakeRegex(fun (vm : FormStateHidden3) -> vm.Name)
                let renderConfig = defaultRenderConfig.WithCustomRender(pattern, (fun value namePath name -> p [] [value |> unbox |> str]))

                let xmlNode = editFor viewModel "myForm" renderConfig
                let expected = """<div><p>James Kirk</p></div>"""
                let actual = RenderView.AsString.htmlNode xmlNode
                Expect.equal actual expected ""

            testCase "simple nested" <| fun _ ->
                let viewModel = {Name = "James Kirk";  Settings = { Theme = "Dark"; Timezone = "Space"; Foo = { Foo = "Bar"}}}

                let xmlNode = editFor viewModel "myForm" defaultRenderConfig
                let expected = """<div><div><label for="Name">Name</label><input name="Name" type="text" value="James Kirk"></div><div><div><label for="Settings[Theme]">Theme</label><input name="Settings[Theme]" type="text" value="Dark"></div><div><label for="Settings[Timezone]">Timezone</label><input name="Settings[Timezone]" type="text" value="Space"></div><div><div><label for="Settings[Foo][Foo]">Foo</label><input name="Settings[Foo][Foo]" type="text" value="Bar"></div></div></div></div>"""
                let actual = RenderView.AsString.htmlNode xmlNode
                Expect.equal actual expected ""

            testCase "simple list" <| fun _ ->
                let viewModel = {Name = "James Kirk";  LotteryNumbers = ["1";"2";"3"]}

                let xmlNode = editFor viewModel "myForm" defaultRenderConfig
                let expected = """<div><div><label for="Name">Name</label><input name="Name" type="text" value="James Kirk"></div><div><div><label for="LotteryNumbers[0]">LotteryNumbers</label><input name="LotteryNumbers[0]" type="text" value="1"></div><div><label for="LotteryNumbers[1]">LotteryNumbers</label><input name="LotteryNumbers[1]" type="text" value="2"></div><div><label for="LotteryNumbers[2]">LotteryNumbers</label><input name="LotteryNumbers[2]" type="text" value="3"></div></div></div>"""
                let actual = RenderView.AsString.htmlNode xmlNode
                Expect.equal actual expected ""

            testCase "simple list record" <| fun _ ->
                let viewModel = {Name = "James Kirk";  Foos = [{Foo = "Bar"}; {Foo = "Baz"}]}

                let xmlNode = editFor viewModel "myForm" defaultRenderConfig
                let expected = """<div><div><label for="Name">Name</label><input name="Name" type="text" value="James Kirk"></div><div><div><div><label for="Foos[0][Foo]">Foo</label><input name="Foos[0][Foo]" type="text" value="Bar"></div></div><div><div><label for="Foos[1][Foo]">Foo</label><input name="Foos[1][Foo]" type="text" value="Baz"></div></div></div></div>"""
                let actual = RenderView.AsString.htmlNode xmlNode
                Expect.equal actual expected ""


            testCase "custom list record" <| fun _ ->
                let viewModel = {Name = "James Kirk";  Foos = [{Foo = "Bar"}; {Foo = "Baz"}]}
                let path = (<@ fun (vm : FormListNestedRecord) -> vm.Foos |> Seq.map(fun f -> f.Foo) @>)
                let hidden value namePath fieldName =
                    input [_type "hidden"; _name namePath ; _value (unbox value)]
                let renderConfig = defaultRenderConfig.WithCustomRender(path,hidden)
                let xmlNode = editFor viewModel "myForm" renderConfig
                let expected = """<div><div><label for="Name">Name</label><input name="Name" type="text" value="James Kirk"></div><div><div><input type="hidden" name="Foos[0][Foo]" value="Bar"></div><div><input type="hidden" name="Foos[1][Foo]" value="Baz"></div></div></div>"""
                let actual = RenderView.AsString.htmlNode xmlNode
                Expect.equal actual expected ""
        ]

    open Giraffe
    open Microsoft.AspNetCore.Hosting.Server.Features
    open Microsoft.AspNetCore.Hosting
    open Microsoft.AspNetCore.Http
    open FSharp.Control.Tasks.Affine
    open System.Threading.Tasks
    open Microsoft.Extensions.DependencyInjection
    open Newtonsoft.Json
    open OpenQA.Selenium
    open canopy.parallell
    let debugHost (webhost : IWebHost) =
        webhost.ServerFeatures.Get<IServerAddressesFeature>().Addresses |> Seq.iter(printfn "%s")

    let submitId = "submit"
    let submitIdPath = $"#{submitId}"
    let browseTo (webhost : IWebHost) (browser : IWebDriver) =
        let url =
            webhost.ServerFeatures.Get<IServerAddressesFeature>().Addresses
            |> Seq.head
        functions.url url browser
        functions.waitForElement submitIdPath browser


    let page bodyForm =
        html [] [
            head [] [
            ]
            body [] [
                form [_id "form"; _method "POST"; ] [
                    bodyForm
                    input [_id submitId ;_type "submit"; _value "Submit"; ]
                ]

                script [_type "application/javascript"; _src "https://code.jquery.com/jquery-3.6.0.slim.min.js"] []
                script [_type "application/javascript"; _src "https://cdn.jsdelivr.net/gh/marioizquierdo/jquery.serializeJSON/jquery.serializejson.min.js"] []
                script [_type "application/javascript"; _src "/js/formToJsonAjax.js"] []
            ]
        ]

    let bindAndPublish (binder : HttpContext -> Task<'a>) (event : Event<'a>) next (ctx : HttpContext) = task {
        try
            let! bindedValue = binder ctx
            //Started in background since event.Trigger seems to hang in conjuction with Async.AwaitEvent
            async {event.Trigger bindedValue} |> Async.Start
            return! Successful.OK "Thanks" next ctx
        with e ->
            return! ServerErrors.INTERNAL_ERROR (sprintf "%A" e) next ctx
    }


    let roundTripTestImpl (viewModel : 'a) additionalAsserts = async {
        let form = editFor viewModel "myForm" defaultRenderConfig
        let page = page form
        let event = Event<'a>()
        // Start awaiting for this event to get published otherwise we may miss it
        let! postedData = event.Publish |> Async.AwaitEvent |> Async.StartChild
        let binder (ctx : HttpContext) = ctx.BindJsonAsync<'a>()

        let webApp = choose [
            route "/" >=> GET >=> htmlView page
            route "/" >=> POST >=> bindAndPublish binder event
        ]

        let configureServices ctx (services : IServiceCollection) =
            let settings = JsonSerializerSettings(MissingMemberHandling = MissingMemberHandling.Ignore)
            settings.Converters.Add(Json.Converters.CheckboxConverter())
            services.AddSingleton<Json.ISerializer>(NewtonsoftJson.Serializer(settings)) |> ignore
            ()

        use! webhost = Server.createGiraffeServer configureServices webApp
        use browser = functions.start canopy.types.ChromeHeadless
        browseTo webhost browser
        functions.click submitIdPath browser
        let! actual = postedData

        Expect.equal actual viewModel ""

        do! additionalAsserts viewModel browser event
    }


    let roundTripTestCase name (viewModel : 'a) additionalAsserts =
        testCaseAsync name <| roundTripTestImpl viewModel additionalAsserts
    let froundTripTestCase name (viewModel : 'a) additionalAsserts =
        ftestCaseAsync name <| roundTripTestImpl viewModel additionalAsserts
    let proundTripTestCase name (viewModel : 'a) additionalAsserts =
        ptestCaseAsync name <| roundTripTestImpl viewModel additionalAsserts

    let noMoreAsserts _ _ _ = async.Zero ()


    let ``$name`` (namePath : NamePath) =  $"[name='{namePath}']"

    let boolAsserts (vm : FormStateBool) (browser : IWebDriver) (event : Event<_>)  = async {
        let expected = {vm with Enabled = false}
        let! postedData = event.Publish |> Async.AwaitEvent |> Async.StartChild
        let namePath = Paths.Path.MakeNamePath(fun (vm : FormStateBool) -> vm.Enabled)
        functions.uncheck (``$name`` namePath) browser
        functions.click submitIdPath browser
        let! actual = postedData
        Expect.equal actual expected ""
    }

    let updateElement predicate f st =
        st |> List.mapi (fun i v -> if predicate i v  then f v else v)


    let simpleListRecordAsserts (vm : FormListNestedRecord) (browser : IWebDriver) (event : Event<_>)  = async {
        let! postedData = event.Publish |> Async.AwaitEvent |> Async.StartChild
        let newFooValue = "Spock"
        let foos = vm.Foos |> updateElement (fun i _ -> i = 1) (fun foo -> {foo with Foo = newFooValue})
        let expected = {vm with Foos = foos}
        let namePath = Paths.Path.MakeNamePath(fun (vm : FormListNestedRecord) -> vm.Foos.[1].Foo)
        functions.write (``$name`` namePath) newFooValue browser
        functions.click submitIdPath browser
        let! actual = postedData
        Expect.equal actual expected ""
    }

    [<Tests>]
    let roundTripTests =
        // testSequenced <|
        testList "Round Trip" [
            roundTripTestCase "bool" {Enabled = true} boolAsserts
            roundTripTestCase "simple nested" {Name = "James Kirk";  Settings = { Theme = "Dark"; Timezone = "Space"; Foo = { Foo = "Bar"}}} noMoreAsserts
            roundTripTestCase "simple list" {Name = "James Kirk";  LotteryNumbers = ["1";"2";"3"]} noMoreAsserts
            roundTripTestCase "simple list record" {Name = "James Kirk";  Foos = [{Foo = "Bar"}; {Foo = "Baz"}]} simpleListRecordAsserts
        ]

