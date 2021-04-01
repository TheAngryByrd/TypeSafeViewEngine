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
        FavoriteRestaurant : Address
        // name = "FavoriteRestaurant[Street]"
        Nicknames : string list
        // name = "Nicknames[0]"
        Addesses : Address list
        // "Addesses[0][Street]"
    }
    open Expecto
    open TypeSafeViewEngine
    open Giraffe.ViewEngine
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
                let renderConfig = defaultRenderConfig.WithCustomRender (nameof(viewModel.Name)) (fun value namePath name -> p [] [value |> unbox |> str])

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

    let browseTo (webhost : IWebHost) (browser : IWebDriver) =
        let url =
            webhost.ServerFeatures.Get<IServerAddressesFeature>().Addresses
            |> Seq.head
        functions.url url browser


    let page bodyForm =
        html [] [
            head [] [
            ]
            body [] [
                form [_id "form"; _method "POST"; ] [
                    bodyForm
                    input [_id "submit" ;_type "submit"; _value "Submit"; ]
                ]
                // script [_type "application/javascript"; _src "https://cdn.jsdelivr.net/gh/serbanghita/formToObject.js/dist/formToObject.min.js"] []

                script [_type "application/javascript"; _src "https://code.jquery.com/jquery-3.6.0.slim.min.js"] []
                script [_type "application/javascript"; _src "https://cdn.jsdelivr.net/gh/marioizquierdo/jquery.serializeJSON/jquery.serializejson.min.js"] []
                script [_type "application/javascript"; _src "/js/formToJsonAjax.js"] []
            ]
        ]

    let bindIt (binder : HttpContext -> Task<_>) (event : Event<_>) next (ctx : HttpContext) = task {
        try
            let! bindedValue = binder ctx
            //Started in background since event.Trigger seems to hang in conjuction with Async.AwaitEvent
            async {event.Trigger bindedValue} |> Async.Start
            return! Successful.OK "Thanks" next ctx
        with e ->
            return! ServerErrors.INTERNAL_ERROR (sprintf "%A" e) next ctx
    }



    [<Tests>]
    let roundTripTests =
        testList "Round Trip" [
            // testCase "hello" <| fun () ->
            //     use browser = functions.start canopy.types.Chrome
            //     functions.url "https://www.olivercoding.com/" browser
            //     functions.highlight "#mainLogoDiv" browser
            //     functions.sleep 5
            //     Expect.isTrue true ""
            testCaseAsync "bool" <| async {
                let myVM =  {Enabled = true}
                let form = editFor myVM "myForm" defaultRenderConfig
                let page = page form
                let event = Event<FormStateBool>()
                let binder (ctx : HttpContext) = ctx.BindJsonAsync<_>()

                let webApp = choose [
                    route "/" >=> GET >=> Giraffe.Core.htmlView page
                    route "/" >=> POST >=> bindIt binder event
                ]

                let configureServices ctx (services : IServiceCollection) =
                    let settings = JsonSerializerSettings(MissingMemberHandling = MissingMemberHandling.Ignore)
                    settings.Converters.Add(Json.Converters.CheckboxConverter())
                    services.AddSingleton<Json.ISerializer>(NewtonsoftJson.Serializer(settings)) |> ignore
                    ()

                use! webhost = Server.createGiraffeServer configureServices webApp
                use browser = functions.start canopy.types.Chrome
                browseTo webhost browser
                functions.sleep 1
                functions.click "#submit" browser
                let! postedData = event.Publish |> Async.AwaitEvent

                Expect.equal postedData myVM ""
            }

            testCaseAsync "simple nested" <| async {
                let myVM = {Name = "James Kirk";  Settings = { Theme = "Dark"; Timezone = "Space"; Foo = { Foo = "Bar"}}}
                let form = editFor myVM "myForm" defaultRenderConfig
                let page = page form
                let event = Event<_>()
                let binder (ctx : HttpContext) = ctx.BindJsonAsync<_>()

                let webApp = choose [
                    route "/" >=> GET >=> Giraffe.Core.htmlView page
                    route "/" >=> POST >=> bindIt binder event
                ]

                let configureServices ctx (services : IServiceCollection) =
                    let settings = JsonSerializerSettings(MissingMemberHandling = MissingMemberHandling.Ignore)
                    settings.Converters.Add(Json.Converters.CheckboxConverter())
                    services.AddSingleton<Json.ISerializer>(NewtonsoftJson.Serializer(settings)) |> ignore
                    ()

                use! webhost = Server.createGiraffeServer configureServices webApp
                use browser = functions.start canopy.types.Chrome
                browseTo webhost browser
                functions.sleep 1
                functions.click "#submit" browser
                let! postedData = event.Publish |> Async.AwaitEvent

                Expect.equal postedData myVM ""
            }


            testCaseAsync "simple list" <| async {
                let myVM = {Name = "James Kirk";  LotteryNumbers = ["1";"2";"3"]}
                let form = editFor myVM "myForm" defaultRenderConfig
                let page = page form
                let event = Event<_>()
                let binder (ctx : HttpContext) = ctx.BindJsonAsync<_>()

                let webApp = choose [
                    route "/" >=> GET >=> Giraffe.Core.htmlView page
                    route "/" >=> POST >=> bindIt binder event
                ]

                let configureServices ctx (services : IServiceCollection) =
                    let settings = JsonSerializerSettings(MissingMemberHandling = MissingMemberHandling.Ignore)
                    settings.Converters.Add(Json.Converters.CheckboxConverter())
                    services.AddSingleton<Json.ISerializer>(NewtonsoftJson.Serializer(settings)) |> ignore
                    ()

                use! webhost = Server.createGiraffeServer configureServices webApp
                use browser = functions.start canopy.types.Chrome
                browseTo webhost browser
                functions.sleep 1
                functions.click "#submit" browser
                let! postedData = event.Publish |> Async.AwaitEvent

                Expect.equal postedData myVM ""
            }

            testCaseAsync "simple list record" <| async {
                let myVM = {Name = "James Kirk";  Foos = [{Foo = "Bar"}; {Foo = "Baz"}]}
                let form = editFor myVM "myForm" defaultRenderConfig
                let page = page form
                let event = Event<_>()
                let binder (ctx : HttpContext) = ctx.BindJsonAsync<_>()

                let webApp = choose [
                    route "/" >=> GET >=> Giraffe.Core.htmlView page
                    route "/" >=> POST >=> bindIt binder event
                ]

                let configureServices ctx (services : IServiceCollection) =
                    let settings = JsonSerializerSettings(MissingMemberHandling = MissingMemberHandling.Ignore)
                    settings.Converters.Add(Json.Converters.CheckboxConverter())
                    services.AddSingleton<Json.ISerializer>(NewtonsoftJson.Serializer(settings)) |> ignore
                    ()

                use! webhost = Server.createGiraffeServer configureServices webApp
                use browser = functions.start canopy.types.Chrome
                browseTo webhost browser
                functions.sleep 1
                functions.click "#submit" browser
                let! postedData = event.Publish |> Async.AwaitEvent

                Expect.equal postedData myVM ""
            }
        ]