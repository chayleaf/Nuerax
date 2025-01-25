namespace Nuerax

open System
open System.Collections
open System.Reflection
open BepInEx
open BepInEx.Logging
open HarmonyLib
open NeuroFSharp

type Actions =
    | [<Action("choose_start", "Choose your disease's starting country")>] ChooseStart of countryName: string
    | [<Action("choose_disease", "Choose a disease to play as")>] ChooseDisease of diseaseName: string
    | [<Action("choose_genes", "Choose genes for your disease")>] ChooseGenes of
        gene1: string option *
        gene2: string option *
        gene3: string option *
        gene4: string option *
        gene5: string option
    | [<Action("choose_name", "Name your disease")>] ChooseName of diseaseName: string
    | [<Action("close_popup", "Close the popup")>] ClosePopup

type DiseaseCtx =
    { name: string
      description: string
      [<SkipSerializingIfEquals(false)>]
      playThisToUnlockNewDiseases: bool
      unlocked: bool }

type GeneCtx =
    { name: string
      description: string
      unlocked: bool }

type GeneCategoryCtx = { name: string; options: GeneCtx list }

type GenesCtx =
    { gene1: GeneCategoryCtx
      gene2: GeneCategoryCtx
      gene3: GeneCategoryCtx
      gene4: GeneCategoryCtx
      gene5: GeneCategoryCtx }

type NameCtx = { defaultName: string }

type PopupCtx = { title: string; description: string }

module Context =
    let disease (ty: Disease.EDiseaseType) : DiseaseCtx =
        let unlAll = CGameManager.localPlayerInfo.GetUnlockedDiseases() |> Seq.toList
        let name = CGameManager.GetDiseaseNameLoc ty
        let desc = CGameManager.DiseaseDescriptions.[ty]
        let next = CGameManager.GetDiseaseNext ty

        { name = name
          description = desc
          playThisToUnlockNewDiseases =
            List.contains ty unlAll
            && next.HasValue
            && not (List.contains next.Value unlAll)
          unlocked = List.contains ty unlAll }

    let gene (gene: Gene) : GeneCtx =
        { name = CLocalisationManager.GetText(gene.geneName)
          description = CLocalisationManager.GetText(gene.geneDescription)
          unlocked = CGameManager.localPlayerInfo.GetGeneUnlocked(gene) }

    let geneCategory (ty: Gene.EGeneCategory) : GeneCategoryCtx =
        let name = CLocalisationManager.GetText(CGameManager.GeneCategoryNames[ty])

        let opts =
            CGameManager.game.AvailableGenes
            |> Seq.toList
            |> List.filter (_.geneCategory >> (=) ty)
            |> List.map gene

        { name = name; options = opts }

type Game(plugin: MainClass) =
    inherit Game<Actions>()

    let mutable forceActs: Action list option = None

    override _.ReregisterActions() = ()

    override _.Name = "Plague Inc Evolved"

    override this.HandleAction(action: Actions) =
        match action with
        | ChooseStart countryName ->
            let screen = CUIManager.instance.GetCurrentScreen()
            let screen = screen :?> CCountrySelect

            let map =
                typeof<CInterfaceManager>
                    .GetField("countryMap", BindingFlags.NonPublic ||| BindingFlags.Instance)
                    .GetValue(CInterfaceManager.instance)
                :?> Generic.IDictionary<string, CountryView>
                |> Seq.map _.Value
                |> Seq.toList

            match
                map
                |> List.tryFind (_.GetCountry() >> _.name >> CLocalisationManager.GetText >> (=) countryName)
            with
            | Some country ->
                let pos = country.GetRandomPositionInsideCountry()

                let allowI =
                    typeof<CCountrySelect>
                        .GetField("mbAllowInteraction", BindingFlags.NonPublic ||| BindingFlags.Instance)
                        .GetValue(screen)
                    :?> bool

                typeof<Camera_Zoom>
                    .GetField("mbIsScrolling", BindingFlags.NonPublic ||| BindingFlags.Instance)
                    .SetValue(Camera_Zoom.instance, false)

                this.LogDebug $"{Camera_Zoom.instance.IsScrolling} {allowI}"
                screen.OnCountryClick(country, pos, true)
                Ok None
            | None -> Error(Some "This country doesn't exist!")
        | ClosePopup ->
            let screen = CUIManager.instance.GetCurrentScreen()
            let sub = screen.GetActiveSubScreen() :?> CHUDPopupSubScreen
            sub.OnMessageClose()
            Ok None
        | ChooseName name ->
            let screen = CUIManager.instance.GetCurrentScreen() :?> CGSScreen
            let sub = screen.GetActiveSubScreen() :?> CGSNameSubScreen

            if name.Length >= 20 then
                Error(Some "Name too long, 20 chars max!")
            elif name.Length = 0 then
                Error(Some "Name too short, 1 char min!")
            else
                sub.input.value <- name
                screen.OnClickNext()
                Ok None
        | ChooseGenes(gene1, gene2, gene3, gene4, gene5) ->
            let screen = CUIManager.instance.GetCurrentScreen() :?> CGSScreen
            let sub = screen.GetActiveSubScreen() :?> CGSGeneSubScreen

            sub.selectedGeneButtons
            |> Array.zip [| gene1; gene2; gene3; gene4; gene5 |]
            |> Array.fold
                (fun res (g, btn) ->
                    match res with
                    | Error err -> Error err
                    | Ok i ->
                        let gene = btn.selected
                        sub.ChooseCategory btn.category

                        if gene <> null then
                            sub.ToggleGene gene

                        let genes =
                            CGameManager.game.AvailableGenes
                            |> Seq.toList
                            |> List.filter (_.geneCategory >> (=) btn.category)

                        match g with
                        | None -> Ok(i + 1)
                        | Some g ->
                            match genes |> List.tryFind (_.geneName >> CLocalisationManager.GetText >> (=) g) with
                            | Some gene ->
                                sub.ToggleGene gene
                                Ok(i + 1)
                            | None ->
                                let names = genes |> List.map (_.geneName >> CLocalisationManager.GetText)
                                Error(Some $"Gene not found, available options: {this.Serialize names}"))
                (Ok(0))
            |> Result.map (fun _ ->
                screen.OnClickNext()
                None)
        | ChooseDisease diseaseName ->
            let unlAll =
                CGameManager.localPlayerInfo.GetUnlockedDiseases()
                |> Seq.map (fun x -> x, CGameManager.GetDiseaseNameLoc x)
                |> List.ofSeq

            match List.tryFind (snd >> (=) diseaseName) unlAll with
            | Some(x, _) ->
                let screen = CUIManager.instance.GetCurrentScreen() :?> CGSScreen
                let sub = screen.GetActiveSubScreen() :?> CGSDiseaseSubScreen
                sub.SetDiseaseType(x, true)
                screen.OnClickNext()
                Ok None
            | None ->
                $"This disasease doesn't exist, or isn't unlocked! Available diseases: {unlAll |> List.map snd |> this.Serialize}"
                |> (Some >> Error)
        |> Result.map (fun x ->
            forceActs
            |> Option.iter (fun acts -> this.UnregisterActions(acts |> List.map _.Name))

            forceActs <- None
            x)

    override _.LogError error =
        let fff = "fff"
        plugin.Logger.LogError $"{DateTime.UtcNow}.{DateTime.UtcNow.ToString(fff)} {error}"

    override _.LogDebug error =
        let fff = "fff"
        plugin.Logger.LogInfo $"{DateTime.UtcNow}.{DateTime.UtcNow.ToString(fff)} {error}"

    member this.DoForce<'T> (ephemeral: bool) (ctx: 'T) (prompt: string) (acts: Action list) =
        forceActs <- Some acts

        this.RetainActions(acts |> List.map (fun x -> x))

        this.Force
            { state = Some(this.Serialize ctx)
              query = prompt
              ephemeral_context = ephemeral
              action_names = acts |> List.map _.Name }

    member this.Update() =
        COptionsManager.instance.BackgroundPause <- EState.Off
        UnityEngine.Application.runInBackground <- true
        UnityEngine.PlayerPrefs.SetInt("isRunInBG", 1)
        // let sc = UnityEngine.Object.FindObjectOfType<CMainStartSubScreen>()
        let screen = CUIManager.instance.GetCurrentScreen() //.GetScreen("MainMenuScreen")

        if screen <> null && Option.isNone forceActs then
            let cur =
                typeof<IGameScreen>
                    .GetField("currentSubScreen", BindingFlags.NonPublic ||| BindingFlags.Instance)
                    .GetValue(screen)
                :?> Generic.IDictionary<string, IGameSubScreen>

            let openSub = Seq.tryHead cur |> Option.map (fun x -> (x.Key, x.Value))

            match screen.GetType().Name with
            | "CMainMenuScreen" ->
                // let screen = screen :?> CMainMenuScreen
                match openSub with
                | Some("Start", x) ->
                    let x = x :?> CMainStartSubScreen

                    if EventDelegate.IsValid x.buttonSinglePlayer.onClick then
                        EventDelegate.Execute x.buttonSinglePlayer.onClick
                        this.LogDebug $"Pressing Singleplayer"
                | Some("Menu", x) ->
                    let x = x :?> CMainNewSubScreen
                    x.buttonClassic.value <- false

                    if EventDelegate.IsValid x.buttonStart.onClick then
                        EventDelegate.Execute x.buttonStart.onClick
                        this.LogDebug $"Pressing Main Game"
                | Some(k, v) -> this.LogDebug $"Main menu / {k} / {v.GetType().Name}"
                | _ -> ()
            | "CGSScreen" ->
                // let screen = screen :?> CGSScreen
                match openSub with
                | Some("main", (:? CGSDiseaseSubScreen as x)) ->
                    let opts = x.allDiseases

                    if not (Array.isEmpty opts) then
                        let ctx = opts |> Array.map Context.disease
                        let act = this.Action ChooseDisease

                        act.MutateProp "diseaseName" (fun x ->
                            (x :?> StringSchema).SetEnum(ctx |> Array.filter _.unlocked |> Array.map _.name))

                        this.DoForce true ctx "Please pick a disease to play as." [ act ]
                | Some("main", (:? CGSGeneSubScreen as x)) ->
                    let cats = x.geneTypeButtons |> Array.map _.category

                    if not (Array.isEmpty cats) then
                        let ctx = cats |> Array.map Context.geneCategory

                        let c =
                            { gene1 = ctx.[0]
                              gene2 = ctx.[1]
                              gene3 = ctx.[2]
                              gene4 = ctx.[3]
                              gene5 = ctx.[4] }

                        let act = this.Action ChooseGenes

                        let setProp x y =
                            act.MutateProp x (fun x ->
                                (x :?> StringSchema)
                                    .SetEnum(
                                        c
                                        |> y
                                        |> _.options
                                        |> List.filter _.unlocked
                                        |> List.map _.name
                                        |> Array.ofList
                                    ))

                        setProp "gene1" _.gene1
                        setProp "gene2" _.gene2
                        setProp "gene3" _.gene3
                        setProp "gene4" _.gene4
                        setProp "gene5" _.gene5
                        this.DoForce true c "Please pick genes for your disease." [ act ]
                | Some("main", (:? CGSDifficultySubScreen as x)) ->
                    // hardcode normal difficulty
                    x.ChooseDifficulty(x.buttons.[1], true)
                | Some("main", (:? CGSNameSubScreen as x)) ->
                    let iv = x.input.value

                    if iv <> "" && not x.setupComplete then
                        let c = { defaultName = iv }
                        let act = this.Action ChooseName
                        this.DoForce true c "Please pick a name for your disease." [ act ]
                | Some(k, v) -> this.LogDebug $"CGS / {k} / {v.GetType().Name}"
                | _ -> ()
            | "CCountrySelect" ->
                // let screen = screen :?> CCountrySelect

                match openSub with
                | Some("popup", (:? CHUDPopupSubScreen as x)) ->
                    let ctx =
                        { title = x.title.text
                          description = x.description.text }

                    let act = this.Action ClosePopup
                    this.DoForce false ctx "Close the popup when you're ready to continue." [ act ]
                | Some(k, v) -> this.LogDebug $"CCS / {k} / {v.GetType().Name}"
                | None ->
                    let map =
                        typeof<CInterfaceManager>
                            .GetField("countryMap", BindingFlags.NonPublic ||| BindingFlags.Instance)
                            .GetValue(CInterfaceManager.instance)
                        :?> Generic.IDictionary<string, CountryView>

                    let ub =
                        typeof<CInterfaceManager>
                            .GetField("mpUserBubble", BindingFlags.NonPublic ||| BindingFlags.Instance)
                            .GetValue(CInterfaceManager.instance)
                        :?> BonusObject

                    if ub = null && CGameManager.localPlayerInfo.disease.nexus = null then
                        let names =
                            map
                            |> Seq.map (fun x -> x.Key, x.Value)
                            |> Seq.map (fun (_, v) ->
                                let c = v.GetCountry()
                                CLocalisationManager.GetText c.name)
                            |> Array.ofSeq

                        let act = this.Action ChooseStart
                        act.MutateProp "countryName" (fun x -> (x :?> StringSchema).SetEnum names)
                        // screen.OnCountryClick()
                        this.DoForce false "Starting country selection" "Please pick the starting country." [ act ]
            | "CHUDScreen" ->
                match openSub with
                (*
                | Some("popup", (:? CHUDPopupSubScreen as x)) ->
                    let ctx =
                        { title = x.title.text
                          description = x.description.text }

                    let act = this.Action ClosePopup
                    this.DoForce false ctx "Close the popup when you're ready to continue." [ act ]*)
                | Some _ ->
                    let all = cur |> Seq.map (fun x -> $"{x.Key} / {x.Value.GetType().Name}") |> List.ofSeq
                    this.LogDebug $"CHUDS / {this.Serialize all}"
                | None -> ()
            | k -> this.LogDebug $"UnkScreen {k}"

and [<BepInPlugin("org.pavluk.nuerax", "Nuerax", "1.0.0")>] MainClass() =
    inherit BaseUnityPlugin()
    let mutable harmony = null
    let mutable game = None
    let cts = new Threading.CancellationTokenSource()

    [<DefaultValue>]
    val mutable public Logger: ManualLogSource

    [<DefaultValue>]
    static val mutable private instance: MainClass

    static member Instance = MainClass.instance
    member _.Game = game.Value

    member this.Awake() =
        try
            MainClass.instance <- this
            harmony <- Harmony.CreateAndPatchAll(Assembly.GetExecutingAssembly())
            this.Logger <- base.Logger
            let cnt = Seq.fold (fun x _ -> x + 1) 0 (harmony.GetPatchedMethods())

            game <-
                Some(
                    let game = Game(this)
                    game.Start(Some("ws://127.0.0.1:8000"), cts.Token) |> ignore
                    game
                )

            this.Logger.LogInfo($"Plugin NeuroShogun is loaded with {cnt} patches!")
        with exc ->
            this.Logger.LogError($"ERROR {exc}")

    member _.Update() = game |> Option.iter _.Update()
(*let x = c.portStatus
                        let y = c.airportStatus
                        let bOpen = c.borderStatus
                        let aOpen = c.hasAirport && c.airportStatus
                        let pOpen = c.hasPorts && c.portStatus
                        let d = CNetworkManager.network.LocalPlayerInfo.disease
                        let defcon = CGameManager.GetDefconState(d.globalPriority)
                        let totalH = d.totalHealthy
                        let totalI = d.totalInfected
                        let totalD = d.totalDead
                        let totalZ = d.totalZombie
                        let totalP = World.instance.totalPopulation
                        let cureP = d.cureCompletePercent // 0..1
                        let cureC = d.cureFlag // complete
                        let cureR = d.CureDaysRemaining // <= 0 -> unknown
                        let curD = CGameManager.currentGameDate
                        let topContrib = d.GetTopCureContributors()
                        let allocA = d.cureResearchAllocationAverage // >0.4 - high, >0.1 - medium, >0 - low, 0 - none
                        let l = c.GetLocalDisease d
                        let intel = l.hasIntel
                        let countryI = l.infectedPopulation
                        let countryD = l.killedPopulation
                        let countryH = l.healthyPopulation
                        let countryZ = l.zombiePopulation*)
