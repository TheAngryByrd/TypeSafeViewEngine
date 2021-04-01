namespace TypeSafeViewEngine.Tests

open System
open System.Net
open Microsoft.Extensions.Configuration
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.AspNetCore.Builder
open Giraffe

module Server =
    let getKestrelServer configureServices configureServer uri = async {
            let configBuilder = new ConfigurationBuilder()
            let configBuilder = configBuilder.AddInMemoryCollection()
            let config = configBuilder.Build()
            config.["server.urls"] <- uri
            let host = WebHostBuilder()
                        .UseConfiguration(config)
                        .UseKestrel()
                        .ConfigureServices(fun ctx app -> configureServices ctx app)
                        .Configure(fun ctx app -> configureServer ctx app )
                        .Build()

            do! host.StartAsync() |> Async.AwaitTask
            return host
        }

    let constructLocalUri port =
            sprintf "http://127.0.0.1:%d" port

    let getPort () =
        let listener = new Sockets.TcpListener(IPAddress.Loopback,0)
        listener.Start()
        let port  = (listener.LocalEndpoint :?> IPEndPoint).Port
        listener.Stop()
        port

    let createGiraffeServer configureServices webApp =
        let configureServices ctx (services : IServiceCollection) =
            services.AddGiraffe() |> ignore
            configureServices ctx services
            ()
        let configure ctx (app : IApplicationBuilder) =
            app.UseStaticFiles() |> ignore
            app.UseGiraffe webApp
            ()

        let uri = getPort () |> constructLocalUri
        getKestrelServer configureServices configure uri
