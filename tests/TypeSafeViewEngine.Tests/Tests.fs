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
    type FormStateString = {
        Name : string
    }

    [<CLIMutableAttribute>]
    type FormStateInt32 = {
        FavoriteNumber32 : int32

    }[<CLIMutableAttribute>]
    type FormStateUInt32 = {
        FavoriteNumberu32 : uint32
    }

    [<CLIMutableAttribute>]
    type FormSimpleNested3 = {
        Foo : string
    }
    [<CLIMutableAttribute>]
    type FormSimpleNested2 = {
        Theme : string
        Timezone : string
        Foo : FormSimpleNested3
    }
    [<CLIMutableAttribute>]
    type FormSimpleNested = {
        Name : string
        Settings : FormSimpleNested2
    }

    [<CLIMutableAttribute>]
    type FormListNested = {
        Name : string
        LotteryNumbers : string list
    }
    [<CLIMutableAttribute>]
    type FormListNestedRecord = {
            Name : string
            Foos : FormSimpleNested3 list
        }

    [<CLIMutableAttribute>]
    type Address = {
        Street : string
        Zip : string
    }
    [<CLIMutableAttribute>]
    type FormStateComplex = {
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
                    let actual = Paths.Path.MakeRegex((fun (form : FormStateComplex) -> form.Addesses |> Seq.map(fun a -> a.Street)))
                    Expect.equal actual expected ""
                testCase "Specific Field in Nested Array Record" <| fun () ->
                    let expected = Regex "Addesses\[0\]\[Street\]"
                    let actual = Paths.Path.MakeRegex((fun (form : FormStateComplex) -> form.Addesses.[0].Street))
                    Expect.equal actual expected ""
            ]
            testList "NamePath" [
                testCase "Specific Field in Nested Array Record" <| fun () ->
                    let expected = "Addesses[2][Street]"
                    let actual = Paths.Path.MakeNamePath((fun (form : FormStateComplex) -> form.Addesses.[2].Street))
                    Expect.equal actual expected ""
            ]
        ]


    let htmlGenTestInner viewModel renderConfig expected =
        let xmlNode = editFor viewModel "myForm" renderConfig
        let actual = RenderView.AsString.htmlNode xmlNode
        Expect.equal actual expected ""
    let htmlGenTest name vm renderConfig expected =
        testCase name <| fun () -> htmlGenTestInner vm renderConfig expected
    let fhtmlGenTest name vm renderConfig expected =
        ftestCase name <| fun () -> htmlGenTestInner vm renderConfig expected
    let phtmlGenTest name vm renderConfig expected =
        ptestCase name <| fun () -> htmlGenTestInner vm renderConfig expected

    [<Tests>]
    let unitTests =
        testList "Lib" [
            htmlGenTest "bool" {Enabled = true} defaultRenderConfig """<div><div><label for="Enabled">Enabled</label><input name="Enabled" type="checkbox" checked data-toggle="toggle"></div></div>"""
            htmlGenTest "string" {Name = "James Kirk"} defaultRenderConfig """<div><div><label for="Name">Name</label><input name="Name" type="text" value="James Kirk"></div></div>"""
            htmlGenTest "int32" {FavoriteNumber32 = 1701} defaultRenderConfig """<div><div><label for="FavoriteNumber32">FavoriteNumber32</label><input name="FavoriteNumber32" type="text" value="1701" step="1"></div></div>"""

            let customFieldNameConfig = defaultRenderConfig.WithCustomRender(Paths.Path.MakeRegex(fun (vm : FormStateString) -> vm.Name), (fun value _ _  -> p [] [value |> unbox |> str]))
            htmlGenTest "custon field name" {Name = "James Kirk"} customFieldNameConfig """<div><p>James Kirk</p></div>"""

            htmlGenTest "simple nested" {Name = "James Kirk";  Settings = { Theme = "Dark"; Timezone = "Space"; Foo = { Foo = "Bar"}}}  defaultRenderConfig """<div><div><label for="Name">Name</label><input name="Name" type="text" value="James Kirk"></div><div><div><label for="Settings[Theme]">Theme</label><input name="Settings[Theme]" type="text" value="Dark"></div><div><label for="Settings[Timezone]">Timezone</label><input name="Settings[Timezone]" type="text" value="Space"></div><div><div><label for="Settings[Foo][Foo]">Foo</label><input name="Settings[Foo][Foo]" type="text" value="Bar"></div></div></div></div>"""

            htmlGenTest "simple list" {Name = "James Kirk";  LotteryNumbers = ["1";"2";"3"]} defaultRenderConfig """<div><div><label for="Name">Name</label><input name="Name" type="text" value="James Kirk"></div><div><div><label for="LotteryNumbers[0]">LotteryNumbers</label><input name="LotteryNumbers[0]" type="text" value="1"></div><div><label for="LotteryNumbers[1]">LotteryNumbers</label><input name="LotteryNumbers[1]" type="text" value="2"></div><div><label for="LotteryNumbers[2]">LotteryNumbers</label><input name="LotteryNumbers[2]" type="text" value="3"></div></div></div>"""

            htmlGenTest "simple list record" {Name = "James Kirk";  Foos = [{Foo = "Bar"}; {Foo = "Baz"}]} defaultRenderConfig """<div><div><label for="Name">Name</label><input name="Name" type="text" value="James Kirk"></div><div><div><div><label for="Foos[0][Foo]">Foo</label><input name="Foos[0][Foo]" type="text" value="Bar"></div></div><div><div><label for="Foos[1][Foo]">Foo</label><input name="Foos[1][Foo]" type="text" value="Baz"></div></div></div></div>"""
            let path = (<@ fun (vm : FormListNestedRecord) -> vm.Foos |> Seq.map(fun f -> f.Foo) @>)
            let hidden value namePath fieldName =
                    input [_type "hidden"; _name namePath ; _value (unbox value)]
            let customListRecordConfig = defaultRenderConfig.WithCustomRender(path,hidden)
            htmlGenTest "custom list record" {Name = "James Kirk";  Foos = [{Foo = "Bar"}; {Foo = "Baz"}]}  customListRecordConfig """<div><div><label for="Name">Name</label><input name="Name" type="text" value="James Kirk"></div><div><div><input type="hidden" name="Foos[0][Foo]" value="Bar"></div><div><input type="hidden" name="Foos[1][Foo]" value="Baz"></div></div></div>"""

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
    module Page =
        let clickSubmit (browser : IWebDriver) =
            functions.click submitIdPath browser

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
        Page.clickSubmit browser
        let! actual = postedData

        Expect.equal actual viewModel ""

        do! additionalAsserts viewModel browser event.Publish
    }


    let roundTripTestCase name (viewModel : 'a) additionalAsserts =
        testCaseAsync name <| roundTripTestImpl viewModel additionalAsserts
    let froundTripTestCase name (viewModel : 'a) additionalAsserts =
        ftestCaseAsync name <| roundTripTestImpl viewModel additionalAsserts
    let proundTripTestCase name (viewModel : 'a) additionalAsserts =
        ptestCaseAsync name <| roundTripTestImpl viewModel additionalAsserts

    let noMoreAsserts _ _ _ = async.Zero ()

    let ``$name`` (namePath : NamePath) =  $"[name='{namePath}']"

    let boolAsserts (vm : FormStateBool) (browser : IWebDriver) (event : IEvent<_>)  = async {
        let! postedData = event |> Async.AwaitEvent |> Async.StartChild

        let expected = {vm with Enabled = false}
        let namePath = Paths.Path.MakeNamePath(fun (vm : FormStateBool) -> vm.Enabled)

        functions.uncheck (``$name`` namePath) browser
        Page.clickSubmit browser

        let! actual = postedData
        Expect.equal actual expected ""
    }

    let updateElement predicate updateFun items =
        items |> List.mapi (fun index item -> if predicate index item  then updateFun item else item)


    let simpleListRecordAsserts (vm : FormListNestedRecord) (browser : IWebDriver) (event : IEvent<_>)  = async {
        let! postedData = event |> Async.AwaitEvent |> Async.StartChild

        let indexOfItemToChange = 1
        let newFooValue = "Spock"
        let foos = vm.Foos |> updateElement (fun i _ -> i = indexOfItemToChange) (fun foo -> {foo with Foo = newFooValue})
        let expected = {vm with Foos = foos}
        let namePath = Paths.Path.MakeNamePath(fun (vm : FormListNestedRecord) -> vm.Foos.[indexOfItemToChange].Foo)

        functions.write (``$name`` namePath) newFooValue browser
        Page.clickSubmit browser

        let! actual = postedData
        Expect.equal actual expected ""
    }

    [<Tests>]
    let roundTripTests =
        // testSequenced <|
        testList "Round Trip" [
            roundTripTestCase "bool" {Enabled = true} boolAsserts
            roundTripTestCase "string" {Name = "James Kirk"} noMoreAsserts
            roundTripTestCase "int32" {FavoriteNumber32 = 1701} noMoreAsserts
            roundTripTestCase "simple nested" {Name = "James Kirk";  Settings = { Theme = "Dark"; Timezone = "Space"; Foo = { Foo = "Bar"}}} noMoreAsserts
            roundTripTestCase "simple list" {Name = "James Kirk";  LotteryNumbers = ["1";"2";"3"]} noMoreAsserts
            roundTripTestCase "simple list record" {Name = "James Kirk";  Foos = [{Foo = "Bar"}; {Foo = "Baz"}]} simpleListRecordAsserts
        ]

