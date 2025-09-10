namespace Nuerax

open System
open System.Collections
open System.Reflection
open BepInEx
open BepInEx.Logging
open HarmonyLib
open NeuroFSharp

type CureAction(name: string, description: string, cureName: string, cureDescription: string) =
    inherit Action(name, description)

    override _.Name = if CGameManager.IsCureGame then cureName else name
    override _.MutableName = true

    override _.Description =
        if CGameManager.IsCureGame then
            cureDescription
        else
            description

    override this.Clone() =
        let mutable ret = CureAction(name, description, cureName, cureDescription)
        ret.InitialSchema <- this.InitialSchema |> Option.map (fun x -> x.Clone() :?> ObjectSchema)
        ret.Schema <- this.Schema |> Option.map (fun x -> x.Clone() :?> ObjectSchema)
        ret

type CureField(cureName: string) =
    inherit FieldAttr()

    override _.Serialized _ _ serialized =
        if CGameManager.IsCureGame then
            [ (cureName, serialized.Value) ]
        else
            []

    override _.ExtraDeserializationNames =
        if CGameManager.IsCureGame then [ cureName ] else []

type VampireCtx = { countryName: string; health: string }

type BubbleCtx =
    { countryName: string
      bubbleName: string
      description: string }

type ShortTechCtx =
    { [<SkipSerializingIfNone>]
      transmissionName: string option
      [<SkipSerializingIfNone>]
      abilityName: string option
      [<SkipSerializingIfNone>]
      symptomName: string option }

type ReqCtx =
    { [<SkipSerializingIfNone>]
      anyResearched: ShortTechCtx list option
      [<SkipSerializingIfNone>]
      allResearched: ShortTechCtx list option }

type TechCtx =
    { [<SkipSerializingIfNone>]
      transmissionName: string option
      [<SkipSerializingIfNone>]
      abilityName: string option
      [<SkipSerializingIfNone>]
      symptomName: string option
      description: string
      [<SkipSerializingIfNone>]
      requires: ReqCtx option
      [<SkipSerializingIfNone>]
      conflicts: ReqCtx option
      evolved: bool
      [<SkipSerializingIfNone>]
      canEvolve: bool option
      [<SkipSerializingIfNone>]
      canDevolve: bool option
      [<SkipSerializingIfNone>]
      evolutionCost: int option
      [<SkipSerializingIfNone>]
      devolutionCost: int option
      [<SkipSerializingIfEquals 0>]
      ``globalInfectivityChange%``: int
      [<SkipSerializingIfEquals 0>]
      ``globalSeverityChange%``: int
      [<SkipSerializingIfEquals 0>]
      ``globalLethalityChange%``: int
    // i could specify the full exact changes, but i really don't feel like it
    // (plus it would give neuro more data than a human player, which isn't bad in itself,
    //  but it does give me an excue)
    }

type Tag =
    | Cold
    | Hot
    | Arid
    | Humid
    | Poverty
    | Wealthy
    | Rural
    | Urban
    | NorthAmerica
    | SouthAmerica
    | Europe
    | Africa
    | Asia_Pacific
    | PortsOpen
    | PortsClosed
    | AirportsOpen
    | AirportsClosed
    | BordersOpen
    | BordersClosed
    | HasEnemyBase
    | HasEnemyLab
    | HasColony
    | HasLair
    | HasDrones

type SortBy =
    | ``Healthy%``
    | ``Infected%``
    | ``Dead%``
    | CureInvestment
    | ``PublicOrder%``
    | ``Zombie%``
    | ``ApeHealthy%``
    | ``ApeInfected%``
    | ``ApeDead%``
    // cure mode
    | ``AuthorityLoss%``
    | ``Noncompliance%``

type CountryOrWorldContext =
    { [<SkipSerializingIfNone>]
      name: string option
      [<SkipSerializingIfEquals 0L>]
      infectedCount: int64
      [<SkipSerializingIfEquals 0f>]
      [<RenameField "infected%">]
      infectedPercent: float32
      [<SkipSerializingIfEquals 0>]
      infectedWeekly: int
      [<SkipSerializingIfEquals 0L>]
      healthyCount: int64
      [<SkipSerializingIfEquals 0f>]
      [<RenameField "healthy%">]
      healthyPercent: float32
      [<SkipSerializingIfEquals 0L>]
      deadCount: int64
      [<SkipSerializingIfEquals 0f>]
      [<RenameField "dead%">]
      deadPercent: float32
      [<SkipSerializingIfEquals 0>]
      deadWeekly: int
      [<SkipSerializingIfEquals 0L>]
      apeInfectedCount: int64
      [<SkipSerializingIfEquals 0f>]
      [<RenameField "apeInfected%">]
      apeInfectedPercent: float32
      [<SkipSerializingIfEquals 0L>]
      apeHealthyCount: int64
      [<SkipSerializingIfEquals 0f>]
      [<RenameField "apeHealthy%">]
      apeHealthyPercent: float32
      [<SkipSerializingIfEquals 0L>]
      apeDeadCount: int64
      [<SkipSerializingIfEquals 0f>]
      [<RenameField "apeDead%">]
      apeDeadPercent: float32
      [<SkipSerializingIfEquals 0f>]
      [<RenameField "authorityLoss%">]
      authorityLossPercent: float32
      [<SkipSerializingIfEquals 0f>]
      [<RenameField "noncompliance%">]
      noncompliancePercent: float32
      [<SkipSerializingIfNone>]
      tags: Tag list option
      [<SkipSerializingIfEquals 0L>]
      zombieCount: int64
      [<SkipSerializingIfEquals 0f>]
      [<RenameField "zombie%">]
      zombiePercent: float32
      // localCureResearch
      [<SkipSerializingIfEquals 0f>]
      [<RenameField "cureInvestment$">]
      cureInvestmentDollars: float32
      [<SkipSerializingIfEquals 0>]
      [<RenameField "cureRequirement$">]
      cureRequirementDollars: int
      // publicOrder * 100
      [<SkipSerializingIfEquals 0f>]
      [<RenameField "publicOrder%">]
      publicOrderPercent: float32
      [<SkipSerializingIfEquals 0>]
      dnaPointsBonusForStartingHere: int }

type Actions =
    | [<Action("reanimate",
               "Cellular regeneration makes some dead zombies rise again. Target country must have dead within.")>] Reanimate of
        countryName: string
    | [<Action("send_zombie_horde",
               "Gather a horde of zombies to send to another country. Start country must have zombies in.")>] ZombieHorde of
        sourceCountryName: string *
        destinationCountryName: string
    | [<Action("ape_rampage",
               "Intelligent apes come together to build a community, rescue others and protect themselves.")>] Rampage of
        countryName: string
    | [<Action("ape_travel", "Intelligent apes leave their homes and move to a new location.")>] ApeTravel of
        sourceCountryName: string *
        destinationCountryName: string
    | [<Action("create_colony",
               "Intelligent apes come together to build a community, rescue others and protect themselves.")>] CreateColony of
        countryName: string
    | [<Action("unscheduled_flight",
               "Infected people travel to another country. Can only be sent from a country with at least 1,000 infected population.")>] UnscheduledFlight of
        sourceCountryName: string *
        destinationCountryName: string
    | [<Action("immune_shock",
               "Increase your infectivity and make it significantly easier to kill people infected by both diseases in a target country.")>] ImmuneShock of
        countryName: string
    | [<Action("benign_mimic",
               "Target country gains significant boost to research of opponent's disease. Target must be fully infected and your disease goes dormant there.")>] BenignMimic of
        countryName: string
    | [<Action("infect_boost", "Increase infectivity of both diseases in a target country.")>] InfectBoost of
        countryName: string
    | [<Action("lethal_boost", "Increase lethality of both diseases in a target country.")>] LethalBoost of
        countryName: string
    | [<Action("toggle_blood_rage",
               "Vampire attacks research/military facilities if present, otherwise goes on killing spree to get DNA.")>] ToggleBloodRage of
        countryName: string
    | [<Action("vampire_flight", "Send a vampire to another country.")>] VampireFlight of
        sourceCountryName: string *
        destinationCountryName: string
    | [<Action("create_lair",
               "Create a lair for the vampire to heal in. Also generates DNA based on infected population.")>] CreateLair of
        countryName: string
    | [<Action("field_operatives", "Send the Field Operatives to a target country.")>] FieldOperatives of
        countryName: string
    | [<Action("targeted_quarantine", "Force a target country to shut border connections and lock down for 1 month.")>] TargetedQuarantine of
        countryName: string
    | [<Action("targeted_economic_aid",
               "Send significant financial aid package to help a country struggling with Quarantine measures. Decrease Non-Compliance in target country.")>] TargetedEconomicAid of
        countryName: string
    | [<Action("nuke", "Nuke a country of your choice")>] Nuke of countryName: string
    // for free hordes *only*
    | [<Action("send_zombie_horde_to", "Send a zombie horde to a country of your choice")>] SendZombieHordeTo of
        countryName: string
    | [<Action("send_trojan_plane", "Send a trojan plane to infect a country of your choice")>] SendTrojanPlane of
        countryName: string
    | [<Action("choose_start",
               "Choose your plague's starting country. It determines your plague's initial fitness to various conditions.")>] ChooseStart of
        countryName: string
    | [<Action("choose_plague", "Choose a plague to play as")>] ChoosePlague of plagueName: string
    | [<Action("choose_genes", "Choose genes for your plague")>] ChooseGenes of
        gene1: string option *
        gene2: string option *
        gene3: string option *
        gene4: string option *
        gene5: string option
    | [<Action("choose_name", "Name your plague")>] ChooseName of plagueName: string
    | [<Action("close_popup", "Close the popup")>] ClosePopup
    | [<Action("set_game_speed", "Set the game speed")>] SetGameSpeed of speed: int
    | [<Action("back_to_the_game", "Close the evolution screen, returning to the main game")>] CloseEvolutionScreen
    | [<Action("continue", "Close the game end screen, allowing you to play again")>] CloseGameEndScreen
    | [<Action("open_evolution_screen", "Open the evolution screen, which allows you to control your plague's evolution")>] OpenEvolutionScreen
    | [<Action("open_transmission_screen",
               "Open the transmission screen, which allows you to control the evolution of transmission vectors, helping your plague spread in different conditions")>] OpenTransmissionScreen
    | [<Action("open_abilities_screen",
               "Open the abilities screen, which allows you to control the evolution of abilities for your plague, making it stronger")>] OpenAbilitiesScreen
    | [<Action("open_symptoms_screen",
               "Open the symptoms screen, which allows you to control the evolution of your plague's symptoms, which determine your plague's infectivity and lethality")>] OpenSymptomsScreen
    | [<Action("evolve_transmission", "Evolve a new transmission vector for your plague for DNA points")>] EvolveTransmission of
        transmissionName: string
    | [<Action("evolve_ability", "Evolve a new ability for your plague for DNA points")>] EvolveAbility of
        abilityName: string
    | [<Action("evolve_symptom", "Evolve a new symptom for your plague for DNA points")>] EvolveSymptom of
        symptomName: string
    // TODO: note whether it will refund or cost DNA points and how much
    // note that cost increases with difficulty and with more devolving
    | [<Action("devolve_transmission", "Devolve a transmission vector, making your disease weaker.")>] DevolveTransmission of
        transmissionName: string
    | [<Action("devolve_ability", "Devolve an ability, making your disease weaker.")>] DevolveAbility of
        abilityName: string
    | [<Action("devolve_symptom", "Devolve a symptom, making your disease weaker.")>] DevolveSymptom of
        symptomName: string
    | [<Action("query_countries",
               "Query countries' statistics. This can help you decide how you should evolve your plague.")>] QueryCountries of
        // tags: cold, hot, arid, humid, poor, rich, rural, urban,
        //       portsOpen, portsClosed, airportsOpen, airportsClosed, bordersOpen, bordersClosed
        //       for simian/necroa/shadow: hasEnemyBase
        //       for simian: hasDrones
        tags: Tag list option *
        tagsNot: Tag list option *
        count: int option *
        // sortBy: healthy%, infected%, dead%, cureContribution,
        // for necroa: zombie%
        // for simian: apesHealthy%, apesInfected%, apesDead%
        sortBy: SortBy option *
        sortDescending: bool option
    | [<Action("focus_country",
               "Focus on a specific country. This will have no effect on gameplay, but you will start receiving live updates on that country's statistics. You can leave countryName null or empty to unfocus the country. Note that when you focus a country, viewers won't be able to see global stats anymore.")>] FocusCountry of
        countryName: string option
    | [<Action("click_bubble",
               "Click on a bubble, which usually gives you some rewards. It is always recommended to do this when you can.")>] ClickBubble of
        countryName: string *
        bubbleName: string

type PlagueCtx =
    { name: string
      description: string
      [<SkipSerializingIfEquals(false)>]
      playThisToUnlockNewPlagues: bool
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

type NameCtx = { previousName: string }

type PopupCtx = { title: string; description: string }

type Context =
    { dnaPoints: int
      ``globalInfectivity%``: int
      ``globalSeverity%``: int
      ``globalLethality%``: int
      inGameDate: string
      [<SkipSerializingIfNone>]
      bubbles: BubbleCtx list option
      [<SkipSerializingIfNone>]
      tips: string list option
      world: CountryOrWorldContext
      [<SkipSerializingIfNone>]
      focusedCountry: CountryOrWorldContext option
      [<SkipSerializingIfNone>]
      [<CureField "fieldOperatives">]
      vampires: VampireCtx list option }

type TechScreenCtx =
    { dnaPoints: int
      ``globalInfectivity%``: int
      ``globalSeverity%``: int
      ``globalLethality%``: int
      [<SkipSerializingIfNone>]
      transmissions: TechCtx list option
      [<SkipSerializingIfNone>]
      symptoms: TechCtx list option
      [<SkipSerializingIfNone>]
      abilities: TechCtx list option }

module Context =
    let enableDevolves = false

    let autoClick (bubble: BonusObject) =
        bubble.``type`` = BonusIcon.EBonusIconType.DNA
        || bubble.``type`` = BonusIcon.EBonusIconType.INFECT
        || bubble.``type`` = BonusIcon.EBonusIconType.DEATH
        || bubble.``type`` = BonusIcon.EBonusIconType.CASTLE
        || bubble.``type`` = BonusIcon.EBonusIconType.APE_COLONY
        || bubble.``type`` = BonusIcon.EBonusIconType.CURE
        || bubble.``type`` = BonusIcon.EBonusIconType.DISEASE_ORIGIN_COUNTRY
        || bubble.``type`` = BonusIcon.EBonusIconType.DEADBUBBLE_FOR_CURE

    let autoClick2 (bubble: BonusIcon) =
        bubble.``type`` = BonusIcon.EBonusIconType.DNA
        || bubble.``type`` = BonusIcon.EBonusIconType.INFECT
        || bubble.``type`` = BonusIcon.EBonusIconType.DEATH
        || bubble.``type`` = BonusIcon.EBonusIconType.CASTLE
        || bubble.``type`` = BonusIcon.EBonusIconType.APE_COLONY
        || bubble.``type`` = BonusIcon.EBonusIconType.CURE
        || bubble.``type`` = BonusIcon.EBonusIconType.DISEASE_ORIGIN_COUNTRY
        || bubble.``type`` = BonusIcon.EBonusIconType.DEADBUBBLE_FOR_CURE

    let disease (ty: Disease.EDiseaseType) : PlagueCtx =
        let unlAll = CGameManager.localPlayerInfo.GetUnlockedDiseases()
                     |> Seq.filter (fun x -> x <> Disease.EDiseaseType.SIMIAN_FLU && x <> Disease.EDiseaseType.VAMPIRE)
                     |> Seq.toList
        let name = CGameManager.GetDiseaseNameLoc ty
        let desc = CGameManager.DiseaseDescriptions.[ty]
        let next = CGameManager.GetDiseaseNext ty

        { name = name
          description = desc
          playThisToUnlockNewPlagues =
            List.contains ty unlAll
            && next.HasValue
            && not (List.contains next.Value unlAll)
          unlocked = List.contains ty unlAll }

    let gene (gene: Gene) : GeneCtx =
        { name = CLocalisationManager.GetText gene.geneName
          description = CLocalisationManager.GetText gene.geneDescription
          unlocked = CGameManager.localPlayerInfo.GetGeneUnlocked gene }

    let geneCategory (ty: Gene.EGeneCategory) : GeneCategoryCtx =
        let name = CLocalisationManager.GetText(CGameManager.GeneCategoryNames[ty])

        let opts =
            CGameManager.game.AvailableGenes
            |> Seq.toList
            |> List.filter (_.geneCategory >> (=) ty)
            |> List.map gene

        { name = name; options = opts }

    let techShort (tech: Technology) : ShortTechCtx =
        let name = CLocalisationManager.GetText tech.name

        let t, s, a =
            match tech.gridType with
            | Technology.ETechType.transmission -> Some name, None, None
            | Technology.ETechType.symptom -> None, Some name, None
            | Technology.ETechType.ability -> None, None, Some name
            | _ -> None, None, None

        { transmissionName = t
          symptomName = s
          abilityName = a }

    let tech (techs: Map<string, Technology>) (tech: Technology) : TechCtx =
        let name = CLocalisationManager.GetText tech.name

        let t, s, a =
            match tech.gridType with
            | Technology.ETechType.transmission -> Some name, None, None
            | Technology.ETechType.symptom -> None, Some name, None
            | Technology.ETechType.ability -> None, None, Some name
            | _ -> None, None, None

        let mkReq isNot any all =
            let proc x remove =
                let ret =
                    x
                    |> Seq.filter (fun x -> not (Seq.contains x remove))
                    |> Seq.map (fun x -> Map.find x techs |> techShort)
                    |> List.ofSeq

                if List.isEmpty ret then None else Some ret

            // for removing redundancy
            // for requirements: anything thats in all must be removed from any
            // for not: anything in any must be removed from all
            let ret =
                { anyResearched = proc any (if isNot then Seq.empty else all)
                  allResearched = proc all (if isNot then any else Seq.empty) }

            if Option.isSome ret.anyResearched || Option.isSome ret.allResearched then
                Some ret
            else
                None

        let d = CGameManager.localPlayerInfo.disease
        let evolved = d.IsTechEvolved tech

        { transmissionName = t
          symptomName = s
          abilityName = a
          description = CLocalisationManager.GetText tech.description
          requires = mkReq false tech.requiredTechOR tech.requiredTechAND
          conflicts = mkReq true tech.notTechOR tech.notTechAND
          evolved = evolved
          canEvolve = if evolved then None else Some(d.CanEvolve tech)
          canDevolve =
            if evolved && enableDevolves then
                Some(d.CanDeEvolve tech)
            else
                None
          evolutionCost = if evolved then None else Some(d.GetEvolveCost tech)
          devolutionCost =
            if evolved && enableDevolves then
                Some(d.GetDeEvolveCost tech)
            else
                None
          ``globalInfectivityChange%`` = int tech.changeToInfectiousness
          ``globalSeverityChange%`` = int tech.changeToSeverity
          ``globalLethalityChange%`` = int tech.changeToLethality }

    let world () : CountryOrWorldContext =
        let d = CGameManager.localPlayerInfo.disease
        let perc = (*) 100f

        { name = None
          tags = None
          infectedCount = d.totalInfected
          infectedPercent = perc d.globalInfectedPercent
          infectedWeekly = int d.infectedWeekly
          healthyCount = d.totalHealthy
          healthyPercent = perc d.globalHealthyPercent
          deadCount = d.totalDead
          deadPercent = perc d.globalDeadPercent
          deadWeekly = 0
          zombieCount = d.totalZombie
          zombiePercent = perc d.globalZombiePercent
          apeInfectedCount = int64 d.apeTotalInfected
          apeInfectedPercent = perc d.apeTotalInfectedPercent
          apeHealthyCount = int64 d.apeTotalHealthy
          apeHealthyPercent = perc d.apeTotalHealthyPercent
          apeDeadCount = int64 d.apeTotalDead
          apeDeadPercent = perc d.apeTotalDeadPercent
          cureInvestmentDollars = d.globalCureResearch
          cureRequirementDollars = int d.cureRequirements
          authorityLossPercent =
            if CGameManager.IsCureGame then
                d.authLossInfectedActual
                + d.authLossDeadActual
                + d.authLossSpread
                + d.authLossCompliance
            else
                0f
          noncompliancePercent =
            if CGameManager.IsCureGame then
                d.globalAvgCompliance
            else
                0f
          publicOrderPercent = 0f
          dnaPointsBonusForStartingHere = 0 }

    let country (country: Country) : CountryOrWorldContext =
        let tag f s = if f country then [ s ] else []

        let ld = country.GetLocalDisease CGameManager.localPlayerInfo.disease

        let tags =
            match country.continentType with
            | Country.EContinentType.NORTH_AMERICA -> [ NorthAmerica ]
            | Country.EContinentType.SOUTH_AMERICA -> [ SouthAmerica ]
            | Country.EContinentType.EUROPE -> [ Europe ]
            | Country.EContinentType.AFRICA -> [ Africa ]
            | Country.EContinentType.ASIA -> [ Asia_Pacific ]
            | _ -> []
            @ tag _.cold Cold
            @ tag _.hot Hot
            @ tag _.arid Arid
            @ tag _.humid Humid
            @ tag _.poverty Poverty
            @ tag _.wealthy Wealthy
            @ tag _.rural Rural
            @ tag _.urban Urban
            @ tag _.borderStatus BordersOpen
            @ tag (_.borderStatus >> not) BordersClosed
            @ tag (fun x -> x.hasAirports && not (ld.AreAirportsOpen())) AirportsClosed
            @ tag (fun x -> x.hasPorts && ld.ArePortsOpen()) PortsOpen
            @ tag (fun x -> x.hasPorts && not (ld.ArePortsOpen())) PortsClosed
            @ tag (fun x -> x.hasAirports && ld.AreAirportsOpen()) AirportsOpen
            @ tag (fun x -> x.hasAirports && not (ld.AreAirportsOpen())) AirportsClosed
            @ tag (fun _ -> ld.HasCastle) HasLair
            @ tag _.hasApeColony HasColony
            @ tag (fun _ -> ld.hasDrone) HasDrones
            @ tag _.HasFort() HasEnemyBase
            // also vampire
            @ tag _.hasApeLab HasEnemyLab

        let perc = (*) 100f

        { name = Some(CLocalisationManager.GetText country.name)
          infectedCount = country.totalInfected
          infectedPercent = perc country.infectedPercent
          infectedWeekly = 0
          healthyCount = country.totalHealthy
          healthyPercent = perc country.healthyPercent
          deadCount = country.totalDead
          deadPercent = perc country.deadPercent
          deadWeekly = 0
          zombieCount = country.totalZombie
          zombiePercent = perc country.zombiePercent
          apeInfectedCount = ld.apeInfectedPopulation
          apeInfectedPercent = perc ld.apeInfectedPercent
          apeHealthyCount = ld.apeHealthyPopulation
          apeHealthyPercent = perc ld.apeHealthyPercent
          apeDeadCount = ld.apeDeadPopulation
          apeDeadPercent =
            if ld.apeDeadPopulation = 0 then
                0f
            else
                perc ld.apeDeadPercent
          cureInvestmentDollars = ld.localCureResearch
          cureRequirementDollars = 0
          publicOrderPercent = min 100f (perc country.publicOrder)
          authorityLossPercent =
            if CGameManager.IsCureGame then
                perc ld.totalLocalAuthLoss
            else
                0f
          noncompliancePercent =
            if CGameManager.IsCureGame then
                max 0f (perc (1f - ld.compliance))
            else
                0f
          tags = Some tags
          dnaPointsBonusForStartingHere =
            if CUIManager.instance.GetCurrentScreen() :? CCountrySelect then
                country.startCountryEvoBonus
            else
                0 }

    let bubble (bubble: BonusObject) : BubbleCtx =
        let name, desc =
            match bubble.``type`` with
            | BonusIcon.EBonusIconType.DNA -> "DNA", "Get bonus DNA"
            | BonusIcon.EBonusIconType.CURE -> "Cure", "Slow down cure development"
            // cure mode?
            | BonusIcon.EBonusIconType.INFECT -> "First Infection", "Get bonus DNA"
            | BonusIcon.EBonusIconType.DEATH -> "Death", "Get bonus DNA"
            | BonusIcon.EBonusIconType.NEURAX -> "Neurax", "Send a plane to a country of your choice"
            | BonusIcon.EBonusIconType.COUNTRY_SELECT ->
                "Country Select", "Click this bubble to confirm starting country selection"
            | BonusIcon.EBonusIconType.COUNTRY_SELECT_P2_INTENTION ->
                "Country Select P2 Intention", "The other player intends to start here"
            | BonusIcon.EBonusIconType.COUNTRY_SELECT_P2_SELECTED ->
                "Country Select P2 Selected", "The other player will start here"
            | BonusIcon.EBonusIconType.NECROA -> "Necroa", "Send a zombie horde to a country of your choice"
            | BonusIcon.EBonusIconType.APE_COLONY -> "Ape Colony", ""
            | BonusIcon.EBonusIconType.NEXUS_FOUND -> "Nexus Found", "Destroy the enemy nexus"
            | BonusIcon.EBonusIconType.NEXUS_DNA -> "Nexus DNA", "Get DNA from your nexus"
            | BonusIcon.EBonusIconType.DOUBLE_INFECTED_DNA -> "Double Infected DNA", "Get DNA for double infection"
            | BonusIcon.EBonusIconType.NUKE -> "Nuke", "Nuke a country"
            | BonusIcon.EBonusIconType.CASTLE -> "Lair", "Get DNA from your lair"
            // cure mode
            | BonusIcon.EBonusIconType.MEDICAL_SYSTEMS_OVERWHELMED -> "Medical Systems Overwhelmed", ""
            // cure mode
            | BonusIcon.EBonusIconType.DISEASE_ORIGIN_COUNTRY -> "Disease Origin Country", ""
            // cure mode
            | BonusIcon.EBonusIconType.DEADBUBBLE_FOR_CURE -> "Deaths", ""
            | _ -> "???", "Get bonus points?"

        { countryName = CLocalisationManager.GetText(bubble.mpCountry.GetCountry().name)
          bubbleName = name
          description = desc }

    let vampire (vampire: Vampire) : VampireCtx =
        { countryName = CLocalisationManager.GetText vampire.currentCountry.name
          health = $"{int vampire.vampireHealth}/{int vampire.vampireHealthMax}" }

    let context () : Context =
        let d = CGameManager.localPlayerInfo.disease
        let techMap = d.technologies |> Seq.map (fun x -> x.id, x) |> Map.ofSeq

        let techs =
            d.technologies
            |> Seq.map (tech techMap)
            |> Seq.filter (_.canEvolve >> Option.exists id)
            |> List.ofSeq
            |> List.sortBy _.evolutionCost

        let cheapest =
            List.tryHead techs
            |> Option.bind _.evolutionCost
            |> Option.defaultValue 99999999

        let mostExpensive =
            List.tryLast techs
            |> Option.bind _.evolutionCost
            |> Option.defaultValue 99999999

        let startDate = DateTime World.instance.startDate
        let date = startDate.AddDays d.turnNumber

        let ub =
            typeof<CInterfaceManager>
                .GetField("mpUserBubble", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .GetValue
                CInterfaceManager.instance
            :?> BonusObject

        let bubbles =
            ub :: (CInterfaceManager.instance.mpBonuses |> List.ofSeq)
            |> List.filter (fun bubble ->
                let getBool name =
                    typeof<BonusObject>.GetField(name, BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue
                        bubble
                    :?> bool

                bubble <> null
                && bubble.mpCountry <> null
                && not (autoClick bubble)
                && not (getBool "mbUnclickable")
                && not (getBool "mbBlocked")
                && not (getBool "mbClicked")
                && not (getBool "mbDead")
                && bubble.isVisible)
            |> List.map bubble

        { dnaPoints = d.evoPoints
          ``globalInfectivity%`` = int d.globalInfectiousnessMax
          ``globalSeverity%`` = int d.globalSeverityMax
          ``globalLethality%`` = int d.globalLethalityMax
          inGameDate = $"{date:``yyyy-MM-dd``}"
          bubbles = if List.isEmpty bubbles then None else Some bubbles
          tips =
            let tips =
                if d.evoPoints >= mostExpensive then
                    [ "You have enough DNA points to evolve *anything* right now, it's heavily recommeded you open the evolution screen" ]
                else if d.evoPoints >= cheapest then
                    [ "You have enough DNA points to evolve certain transmission vectors/symptoms/abilities for your plague" ]
                else
                    []
                @ if List.isEmpty bubbles then
                      []
                  else
                      [ "You have bubbles available to click, it's heavily recommended you do so to claim some rewards. If you find the amount of bubbles overwhelming, consider lowering game speed." ]

            if List.isEmpty tips then None else Some tips
          world = world ()
          focusedCountry =
            if CInterfaceManager.instance.SelectedCountryView = null then
                None
            else
                Some(country (CInterfaceManager.instance.SelectedCountryView.GetCountry()))
          vampires =
            let vampires = d.vampires |> List.ofSeq |> List.map vampire
            if List.isEmpty vampires then None else Some vampires }

module Actions =
    let map () =
        typeof<CInterfaceManager>.GetField("countryMap", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue
            CInterfaceManager.instance
        :?> Generic.IDictionary<string, CountryView>
        |> Seq.map _.Value

    let countryNames () : string seq =
        map () |> Seq.map (_.GetCountry().name >> CLocalisationManager.GetText)

    let nuke<'T> (game: Game<'T>) =
        let act = game.Action Nuke
        let names = countryNames () |> Array.ofSeq
        act.MutateProp "countryName" (fun x -> (x :?> StringSchema).SetEnum names)
        act

    let sendTrojanPlane<'T> (game: Game<'T>) =
        let act = game.Action SendTrojanPlane
        let names = countryNames () |> Array.ofSeq
        act.MutateProp "countryName" (fun x -> (x :?> StringSchema).SetEnum names)
        act

    let sendZombieHordeTo<'T> (game: Game<'T>) =
        let act = game.Action SendZombieHordeTo
        let names = countryNames () |> Array.ofSeq
        act.MutateProp "countryName" (fun x -> (x :?> StringSchema).SetEnum names)
        act

    let choosePlague<'T> (game: Game<'T>) (ctx: PlagueCtx array) =
        let act = game.Action ChoosePlague

        act.MutateProp "plagueName" (fun x ->
            (x :?> StringSchema).SetEnum(ctx |> Array.filter _.unlocked |> Array.map _.name))

        act

    let chooseGenes<'T> (game: Game<'T>) (c: GenesCtx) =
        let act = game.Action ChooseGenes

        let setProp x y =
            act.MutateProp x (fun x ->
                (x :?> StringSchema)
                    .SetEnum(c |> y |> _.options |> List.filter _.unlocked |> List.map _.name |> Array.ofList))

        setProp "gene1" _.gene1
        setProp "gene2" _.gene2
        setProp "gene3" _.gene3
        setProp "gene4" _.gene4
        setProp "gene5" _.gene5
        act

    let chooseStart<'T> (game: Game<'T>) =
        let act = game.Action ChooseStart
        let names = countryNames () |> Array.ofSeq
        act.MutateProp "countryName" (fun x -> (x :?> StringSchema).SetEnum names)
        act

    let setGameSpeed<'T> (game: Game<'T>) =
        let act = game.Action SetGameSpeed

        act.MutateProp "speed" (fun x ->
            let x = x :?> IntegerSchema
            x.Minimum <- Some 0
            x.Maximum <- Some 3)

        act

    let queryCountries<'T> (update: bool) (game: Game<'T>) =
        if update then
            let countries = map () |> Seq.map (_.GetCountry() >> Context.country)

            let tags =
                Set.ofSeq (countries |> Seq.collect (_.tags >> Option.defaultValue []))
                |> Set.map game.Serialize

            let i2l n x = if n > 0f then [ x ] else []

            let sortBy =
                Set.ofSeq (
                    countries
                    |> Seq.collect (fun x ->
                        i2l x.healthyPercent ``Healthy%``
                        @ i2l x.infectedPercent ``Infected%``
                        @ i2l x.deadPercent ``Dead%``
                        @ i2l x.cureInvestmentDollars CureInvestment
                        @ i2l x.publicOrderPercent ``PublicOrder%``
                        @ i2l x.zombiePercent ``Zombie%``
                        @ i2l x.apeHealthyPercent ``ApeHealthy%``
                        @ i2l x.apeInfectedPercent ``ApeInfected%``
                        @ i2l x.apeDeadPercent ``ApeDead%``
                        @ i2l x.authorityLossPercent ``AuthorityLoss%``
                        @ i2l x.noncompliancePercent ``Noncompliance%``)
                )
                |> Set.map game.Serialize

            let act = game.Action QueryCountries

            act.MutateProp "tags" (fun x ->
                ((x :?> ArraySchema).Items :?> StringSchema).RetainEnum(game.Serialize >> tags.Contains))

            act.MutateProp "tagsNot" (fun x ->
                ((x :?> ArraySchema).Items :?> StringSchema).RetainEnum(game.Serialize >> tags.Contains))

            act.MutateProp "sortBy" (fun x -> (x :?> StringSchema).RetainEnum(game.Serialize >> sortBy.Contains))

            act
        else
            game.Action QueryCountries

    let focusCountry<'T> (forceUpdate: bool) (game: Game<'T>) =
        let act = game.Action FocusCountry

        act.MutateProp "countryName" (fun x ->
            let s = x :?> StringSchema

            if forceUpdate || Option.isNone s.Enum then
                countryNames () |> Array.ofSeq |> s.SetEnum)

        act

    let clickBubble<'T> (game: Game<'T>) (ctx: Context) : Action option =
        ctx.bubbles
        |> Option.orElseWith (fun () ->
            let act = game.Action ClickBubble
            act.MutateProp "countryName" (fun x -> (x :?> StringSchema).SetEnum [||])
            act.MutateProp "bubbleName" (fun x -> (x :?> StringSchema).SetEnum [||])
            None)
        |> Option.map (fun bubbles ->
            let act = game.Action ClickBubble

            let countries =
                bubbles
                |> List.distinctBy _.countryName
                |> List.map _.countryName
                |> Array.ofList

            let types =
                bubbles |> List.distinctBy _.bubbleName |> List.map _.bubbleName |> Array.ofList

            act.MutateProp "countryName" (fun x -> (x :?> StringSchema).SetEnum countries)
            act.MutateProp "bubbleName" (fun x -> (x :?> StringSchema).SetEnum types)
            act)

    let aa1<'T>
        act
        (validate: (Country * LocalDisease) -> bool)
        (forceUpdate: bool)
        (game: Game<'T>)
        (aa: AbilityButton)
        : Action option =
        let d = CGameManager.localPlayerInfo.disease
        let cost = CGameManager.GetActiveAbilityCost(aa.abilityType, d)

        if aa.button.isEnabled && d.evoPoints >= cost then

            let act = game.Action act
            let mutable isOk = true

            act.MutateProp "countryName" (fun x ->
                let s = x :?> StringSchema

                if Option.isNone s.Enum || forceUpdate then
                    let names =
                        map ()
                        |> Seq.map _.GetCountry()
                        |> Seq.filter (fun c -> validate (c, c.GetLocalDisease d))
                        |> Seq.map (_.name >> CLocalisationManager.GetText)
                        |> Array.ofSeq

                    s.SetEnum names

                isOk <- Array.isEmpty s.Enum.Value |> not)

            if isOk then
                if cost > 0 then
                    act.Description <- act.InitialDescription + $" Costs {cost} DNA points."
                else
                    act.Description <- act.InitialDescription + " Does not cost DNA points."

                Some act
            else
                None
        else
            None

    let aa2<'T>
        act
        (validateSrc: (Country * LocalDisease) -> bool)
        (forceUpdate: bool)
        (game: Game<'T>)
        (aa: AbilityButton)
        : Action option =
        let d = CGameManager.localPlayerInfo.disease
        let cost = CGameManager.GetActiveAbilityCost(aa.abilityType, d)

        if aa.button.isEnabled && d.evoPoints >= cost then
            let act = game.Action act
            let mutable isOk = true

            act.MutateProp "sourceCountryName" (fun x ->
                let s = x :?> StringSchema

                if Option.isNone s.Enum || forceUpdate then
                    let names =
                        map ()
                        |> Seq.map _.GetCountry()
                        |> Seq.filter (fun c -> validateSrc (c, c.GetLocalDisease d))
                        |> Seq.map (_.name >> CLocalisationManager.GetText)
                        |> Array.ofSeq

                    s.SetEnum names

                isOk <- Array.isEmpty s.Enum.Value |> not)

            if isOk then
                act.MutateProp "destinationCountryName" (fun x ->
                    let s = x :?> StringSchema

                    if Option.isNone s.Enum || forceUpdate then
                        let names' = countryNames () |> Array.ofSeq
                        s.SetEnum names')

                if cost > 0 then
                    act.Description <- act.InitialDescription + $" Costs {cost} DNA points."
                else
                    act.Description <- act.InitialDescription + " Does not cost DNA points."

                Some act
            else
                None
        else
            None

    // most of this logic is from IsHoverStateValid
    let reanimate = aa1 Reanimate (snd >> _.GetReanimateValue() >> (<) 0)
    let zombieHorde = aa2 ZombieHorde (snd >> _.zombie >> (<) 0)
    let apeRampage = aa1 Rampage (snd >> _.apeInfectedPopulation >> (<) 0)
    let apeTravel = aa2 ApeTravel (snd >> _.apeInfectedPopulation >> (<) 0)

    let createColony =
        aa1 CreateColony (fun (c, ld) -> ld.apeInfectedPopulation > 0 && not c.hasApeColony)

    let unscheduledFlight =
        aa2 UnscheduledFlight (fun (c, ld) ->
            if CGameManager.IsCoopMPGame then
                c.localDiseases |> Seq.exists (_.infectedPopulation >> (<) 1000)
            else
                ld.infectedPopulation > 1000)

    let immuneShock =
        aa1 ImmuneShock (fun (_, ld) ->
            CGameManager.IsVersusMPGame
            && ld.uncontrolledInfected + ld.controlledInfected > 0
            && (ld :?> MPLocalDisease).immuneShockCounter <= 0)

    let benignMimic =
        aa1 BenignMimic (fun (c, ld) ->
            let ld2 =
                if World.instance.diseases.Count = 2 then
                    c.GetLocalDisease(
                        World.instance.diseases[if World.instance.diseases.[0] = ld.disease then 1 else 0]
                    )
                else
                    null

            CGameManager.IsVersusMPGame
            && ld.allInfected > 0
            && ld.allInfected + c.deadPopulation >= c.originalPopulation
            && (ld :?> MPLocalDisease).benignMimicCounter <= 0
            && ld2 <> null
            && (ld2 :?> MPLocalDisease).benignMimicCounter <= 0)

    let infectBoost =
        aa1 InfectBoost (fun (c, ld) ->
            CGameManager.IsCoopMPGame
            && c.totalInfected > 0
            && (ld :?> CoopLocalDisease).infectBoostCounter <= 0)

    let lethalBoost =
        aa1 LethalBoost (fun (c, ld) ->
            CGameManager.IsCoopMPGame
            && c.totalInfected > 0
            && (ld :?> CoopLocalDisease).lethalBoostCounter <= 0)

    let bloodRage = aa1 ToggleBloodRage (snd >> _.zombie >> (<) 0)
    let vampireFlight = aa2 VampireFlight (snd >> _.zombie >> (<) 0)

    let createLair =
        aa1 CreateLair (fun (_, ld) -> ld.zombie > 0 && ld.castleState = ECastleState.CASTLE_NONE)

    let fieldOperatives =
        aa1 FieldOperatives (fun (_, ld) ->
            not ld.hasTeam
            && ld.disease.vampires.Count > 0
            && ld.disease.vampires.[0].currentCountry <> null)

    let targetedQuarantine =
        aa1 TargetedQuarantine (fun (_, ld) -> ld.hasIntel && not ld.lockdownAAActive && ld.infectedPercent > 0f)

    let targetedEconomicAid =
        aa1 TargetedEconomicAid (fun (_, ld) -> ld.hasIntel && ld.compliance < 1f)

type Game(plugin: MainClass) =
    inherit Game<Actions>()

    let mutable forceActs: Action list option = None
    let mutable nextContextTime: DateTime = DateTime.MinValue
    let mutable newBubble: bool = false

    let map () =
        typeof<CInterfaceManager>.GetField("countryMap", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue
            CInterfaceManager.instance
        :?> Generic.IDictionary<string, CountryView>
        |> Seq.map _.Value

    let countryNames () : string seq =
        map () |> Seq.map (_.GetCountry().name >> CLocalisationManager.GetText)

    member _.NextContextTime = nextContextTime

    override _.ReregisterActions() =
        forceActs <- None
        nextContextTime <- DateTime.MinValue
        newBubble <- false

    override _.Name = "Plague Inc Evolved"

    override this.HandleAction(action: Actions) =
        let screen = CUIManager.instance.GetCurrentScreen()

        let cur =
            typeof<IGameScreen>.GetField("currentSubScreen", BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue
                screen
            :?> Generic.IDictionary<string, IGameSubScreen>
            |> List.ofSeq

        let d = CGameManager.localPlayerInfo.disease
        // just a good idea since this is checked all over the place
        UICamera.hoveredObject <- null

        let findTech f s t w =
            let sub = screen.GetSubScreen $"{w}_SubScreen" :?> CTechTreeSubScreen
            let techMap = d.technologies |> Seq.map (fun x -> x.id, x) |> Map.ofSeq

            match d.technologies |> Seq.tryFind (Context.tech techMap >> f >> (=) (Some s)) with
            | Some tech ->
                let hex =
                    typeof<CTechTreeSubScreen>
                        .GetField("hexes", BindingFlags.NonPublic ||| BindingFlags.Instance)
                        .GetValue
                        sub
                    :?> Generic.IDictionary<int, TechHex>
                    |> _.Values
                    |> Seq.filter _.visible
                    |> Seq.map (fun x ->
                        x,
                        typeof<TechHex>
                            .GetField("technology", BindingFlags.NonPublic ||| BindingFlags.Instance)
                            .GetValue
                            x
                        :?> Technology)
                    |> Seq.tryFind (snd >> (=) tech)

                match hex with
                | Some hex -> Ok(tech, fst hex, sub)
                | None -> Error(Some "Internal error, couldn't find the tech hex, this might not be evolvable")
            | None ->
                let opts =
                    d.technologies
                    |> Seq.toList
                    |> Seq.collect (Context.tech techMap >> f >> Option.toList)

                Error(Some $"{t} named {this.Serialize s} was not found, available options: {this.Serialize opts}")

        let costStr n =
            if n = 0 then ""
            else if n > 0 then $" for {n} DNA points"
            else $" giving you {-n} DNA points"

        let evolve (tech: Technology, hex: TechHex, sub: CTechTreeSubScreen) =
            let cost = d.GetEvolveCost tech
            let name = CLocalisationManager.GetText tech.name

            if d.IsTechEvolved tech then
                Error(Some $"{name} is already evolved!")
            elif d.GetEvolveCost tech > d.evoPoints then
                Error(Some $"{name} costs {cost} DNA points, while you only have {d.evoPoints} points!")
            elif d.CanEvolve tech then
                let screen = CUIManager.instance.GetScreen "DiseaseScreen" :?> CDiseaseScreen
                screen.EvolveDisease(hex, tech, false, CGameManager.localPlayerInfo.disease.evoPointsSpent)
                CGameManager.game.EvolveTech tech |> ignore
                sub.TechSelected(tech, hex)
                Ok(Some $"Evolved {name}{costStr cost}")
            elif tech.eventLocked then
                Error(Some $"{name} is currently locked and can't be evolved")
            else
                let techMap = d.technologies |> Seq.map (fun x -> x.id, x) |> Map.ofSeq

                let allNot =
                    tech.notTechAND.Count <> 0 && tech.notTechAND |> Seq.forall d.IsTechEvolved

                let orNot = tech.notTechOR |> Seq.exists d.IsTechEvolved
                let allReq = tech.requiredTechAND |> Seq.forall d.IsTechEvolved

                let orReq =
                    tech.requiredTechOR.Count = 0
                    || tech.requiredTechOR |> Seq.exists d.IsTechEvolved

                let techNames =
                    Seq.map (fun x -> CLocalisationManager.GetText (Map.find x techMap).name)

                let text =
                    String.Join(
                        "; ",
                        (if allNot then
                             [ $"the following must *not* be evolved at the same time: {techNames tech.notTechAND}" ]
                         else
                             [])
                        @ (if orNot then
                               [ $"*none* of the following must be evolved: {techNames tech.notTechOR}" ]
                           else
                               [])
                        @ (if allReq then
                               []
                           else
                               [ $"*all* of the following must be evolved: {techNames tech.requiredTechAND}" ])
                        @ (if orReq then
                               []
                           else
                               [ $"*any* of the following must be evolved: {techNames tech.requiredTechOR}" ])
                    )

                Error(Some $"{name} can't be researched because some of the prerequisites were not met: {text}")

        let devolve (tech: Technology, hex: TechHex, sub: CTechTreeSubScreen) =
            let cost = d.GetDeEvolveCost tech
            let name = CLocalisationManager.GetText tech.name

            if not (d.IsTechEvolved tech) then
                Error(Some $"{name} is not yet evolved!")
            elif d.GetDeEvolveCost tech > d.evoPoints then
                Error(
                    Some
                        $"Deevolving {name} costs {d.GetDeEvolveCost tech} DNA points, while you only have {d.evoPoints} points!"
                )
            elif d.CanDeEvolve tech then
                let screen = CUIManager.instance.GetScreen "DiseaseScreen" :?> CDiseaseScreen
                screen.EvolveDisease(hex, tech, false, CGameManager.localPlayerInfo.disease.evoPointsSpent)
                d.DeEvolveTech(tech, false)
                CGameManager.game.DeEvolveTech tech |> ignore
                screen.PreviewTechDevolve null
                sub.TechSelected(tech, hex)
                Ok(Some $"Devolved {name}{costStr cost}")
            else
                Error(Some $"{name} can't be devolved!")

        let postBubbleDifferentTargetAction act hudMode countryName =
            if CHUDScreen.instance.HudInterfaceMode = hudMode then
                match
                    map ()
                    |> Seq.tryFind (_.GetCountry() >> _.name >> CLocalisationManager.GetText >> (=) countryName)
                with
                | Some country when CInterfaceManager.instance.mpTargetBubbleStart = country ->
                    Error(Some "The target country can't be the same as the source country")
                | Some country ->
                    let screen = CUIManager.instance.GetCurrentScreen() :?> BaseMapScreen
                    let pos = country.GetRandomPositionInsideCountry()
                    screen.OnCountryHover(country, pos)
                    screen.OnCountryClick(country, pos, true)
                    CGameManager.SetPaused(false, true)
                    // fallback in case the above fails for some reason
                    CHUDScreen.instance.Default()
                    Ok None
                | None -> Error(Some "This country doesn't exist!")
            elif forceActs |> Option.exists (List.exists (_.Name >> (=) (this.Action act).Name)) then
                // send ok to prevent retrying
                CGameManager.SetPaused(false, true)
                Ok(Some "Error: this can't be done anymore for some reason")
            else
                Error(Some "This can't be done anymore")

        let execAction2
            (checks: (((Country * LocalDisease) -> (Country * LocalDisease) -> bool) * string) list)
            (ty: EAbilityType)
            src
            dst
            =
            let chk2 f s =
                Result.bind (fun (x, y) -> if f x y then Ok(x, y) else Error(Some s))

            let map = map ()

            let src =
                map
                |> Seq.tryFind (_.GetCountry() >> _.name >> CLocalisationManager.GetText >> (=) src)

            let dst =
                map
                |> Seq.tryFind (_.GetCountry() >> _.name >> CLocalisationManager.GetText >> (=) dst)

            let sub = CHUDScreen.instance.abilitySubscreen

            let btns =
                typeof<CHUDAbilitySubScreen>
                    .GetField("abilitiesButtonMap", BindingFlags.NonPublic ||| BindingFlags.Instance)
                    .GetValue
                    sub
                :?> Generic.Dictionary<EAbilityType, AbilityButton>

            let cost = CGameManager.GetActiveAbilityCost(ty, d)

            match btns |> Seq.tryFind (_.Key >> (=) ty) |> Option.map _.Value with
            | None -> Error(Some "You haven't researched this ability!")
            | Some btn when not btn.button.isEnabled -> Error(Some "The ability is on cooldown")
            | Some _ when d.evoPoints < cost ->
                Error(Some $"You must have at least {cost} DNA points to use this ability; you only have {d.evoPoints}")
            | Some btn ->
                match src, dst with
                | Some src, Some dst ->
                    let src' = src.GetCountry()
                    let dst' = dst.GetCountry()
                    let src'' = src'.GetLocalDisease d
                    let dst'' = dst'.GetLocalDisease d
                    let state = Ok((src', src''), (dst', dst''))

                    List.fold (fun state x -> state |> chk2 (fst x) (snd x)) state checks
                    |> Result.bind (fun _ ->
                        if CHUDScreen.instance.HudInterfaceMode = EHudMode.Normal then
                            Ok()
                        else
                            Error(Some "Unknown mod error: HUD mode isn't normal"))
                    |> Result.bind (fun () ->
                        let screen = CUIManager.instance.GetCurrentScreen() :?> BaseMapScreen
                        let srcPos = src.GetRandomPositionInsideCountry()
                        let dstPos = dst.GetRandomPositionInsideCountry()
                        sub.StartAbility btn

                        Ok()
                        |> Result.bind (fun () ->
                            if CHUDScreen.instance.HudInterfaceMode = EHudMode.Normal then
                                Error(Some "Unknown mod error: HUD mode didn't change after ability activation")
                            else
                                Ok())
                        |> Result.bind (fun () ->
                            let mode = CHUDScreen.instance.HudInterfaceMode
                            screen.OnCountryHover(src, srcPos)

                            if CInterfaceManager.instance.HoverCountry <> src then
                                CHUDScreen.instance.Default()
                                Error(Some "Unknown mod error: hover country didn't match expected source country")
                            else
                                Ok mode)
                        |> Result.bind (fun mode ->
                            screen.OnCountryClick(src, srcPos, true)

                            if CHUDScreen.instance.HudInterfaceMode = mode then
                                CHUDScreen.instance.Default()

                                Error(
                                    Some
                                        "Unknown mod error: HUD mode didn't change after clicking on the initial country"
                                )
                            elif CHUDScreen.instance.HudInterfaceMode = EHudMode.Normal then
                                Error(Some "Unknown mod error: HUD mode got reset to normal")
                            else
                                Ok())
                        |> Result.bind (fun () ->
                            screen.OnCountryHover(dst, dstPos)

                            if CInterfaceManager.instance.HoverCountry <> dst then
                                CHUDScreen.instance.Default()

                                Error(
                                    Some
                                        "Couldn't perform action, the destination country is probably too far from the source country"
                                )
                            else
                                Ok())
                        |> Result.bind (fun () ->
                            screen.OnCountryClick(dst, dstPos, true)

                            if CHUDScreen.instance.HudInterfaceMode <> EHudMode.Normal then
                                CHUDScreen.instance.Default()

                                Error(
                                    Some
                                        "Unknown mod error: HUD mode didn't get reset to normal after clicking on the destination country"
                                )
                            else
                                Ok None))
                | None, None -> Error(Some "Both source and destination countries were not found!")
                | None, Some _ -> Error(Some "Source country was not found.")
                | Some _, None -> Error(Some "Destination country was not found.")

        let execAction1 (checks: (((Country * LocalDisease) -> bool) * string) list) (ty: EAbilityType) country =
            let chk f s =
                Result.bind (fun x -> if f x then Ok x else Error(Some s))

            let map = map ()

            let country =
                map
                |> Seq.tryFind (_.GetCountry() >> _.name >> CLocalisationManager.GetText >> (=) country)

            let sub = CHUDScreen.instance.abilitySubscreen

            let btns =
                typeof<CHUDAbilitySubScreen>
                    .GetField("abilitiesButtonMap", BindingFlags.NonPublic ||| BindingFlags.Instance)
                    .GetValue
                    sub
                :?> Generic.Dictionary<EAbilityType, AbilityButton>

            let cost = CGameManager.GetActiveAbilityCost(ty, d)

            match btns |> Seq.tryFind (_.Key >> (=) ty) |> Option.map _.Value with
            | None -> Error(Some "You haven't researched this ability!")
            | Some btn when not btn.button.isEnabled -> Error(Some "The ability is on cooldown")
            | Some _ when d.evoPoints < cost ->
                Error(Some $"You must have at least {cost} DNA points to use this ability; you only have {d.evoPoints}")
            | Some btn ->
                match country with
                | Some country ->
                    let country' = country.GetCountry()
                    let country'' = country'.GetLocalDisease d
                    let state = Ok((country', country''))

                    List.fold (fun state x -> state |> chk (fst x) (snd x)) state checks
                    |> Result.bind (fun _ ->
                        if CHUDScreen.instance.HudInterfaceMode = EHudMode.Normal then
                            Ok()
                        else
                            Error(Some "Unknown mod error: HUD mode isn't normal"))
                    |> Result.bind (fun () ->
                        let screen = CUIManager.instance.GetCurrentScreen() :?> BaseMapScreen
                        let countryPos = country.GetRandomPositionInsideCountry()
                        sub.StartAbility btn

                        Ok()
                        |> Result.bind (fun () ->
                            if CHUDScreen.instance.HudInterfaceMode = EHudMode.Normal then
                                Error(Some "Unknown mod error: HUD mode didn't change after ability activation")
                            else
                                Ok())
                        |> Result.bind (fun () ->
                            screen.OnCountryHover(country, countryPos)

                            if CInterfaceManager.instance.HoverCountry <> country then
                                CHUDScreen.instance.Default()
                                Error(Some "Unknown mod error: hover country didn't match expected country")
                            else
                                Ok())
                        |> Result.bind (fun () ->
                            screen.OnCountryClick(country, countryPos, true)

                            if CHUDScreen.instance.HudInterfaceMode <> EHudMode.Normal then
                                CHUDScreen.instance.Default()

                                Error(
                                    Some
                                        "Unknown mod error: HUD mode didn't get reset to normal after clicking on the destination country"
                                )
                            else
                                Ok None))
                | None -> Error(Some "This country doesn't exist!")

        match action with
        | ZombieHorde(src, dst) ->
            execAction2
                [ (fun (_, ld) _ -> ld.zombie > 0), "Source country must have zombies"
                  (fun (c1, _) (c2, _) -> c1 <> c2), "Source country must differ from destination country" ]
                EAbilityType.zombie_horde
                src
                dst
        // TODO: properly do flight distance checks
        | VampireFlight(src, dst) ->
            execAction2
                [ (fun (_, ld) _ -> ld.zombie > 0), "Source country must have a vampire"
                  (fun (c1, _) (c2, _) -> c1 <> c2), "Source country must differ from destination country" ]
                EAbilityType.vampiretravel
                src
                dst
        | UnscheduledFlight(src, dst) ->
            execAction2
                [ (fun (c, ld) _ ->
                      if CGameManager.IsCoopMPGame then
                          c.localDiseases |> Seq.exists (_.infectedPopulation >> (<) 1000)
                      else
                          ld.infectedPopulation > 1000),
                  "Source country must have over 1000 infected people"
                  (fun (c1, _) (c2, _) -> c1 <> c2), "Source country must differ from destination country" ]
                EAbilityType.unscheduled_flight
                src
                dst
        | ApeTravel(src, dst) ->
            execAction2
                [ (fun (_, ld) _ -> ld.apeInfectedPopulation > 0), "Source country must have infected apes"
                  (fun (c1, _) (c2, _) -> c1 <> c2), "Source country must differ from destination country" ]
                EAbilityType.move
                src
                dst
        | TargetedQuarantine countryName ->
            execAction1
                [ snd >> _.hasIntel, "You don't have intel on target country"
                  snd >> _.lockdownAAActive >> not, "The country is already on lockdown"
                  snd >> _.infectedPercent >> (<) 0f, "The country doesn't have any infected people" ]
                EAbilityType.raise_priority
                countryName
        | TargetedEconomicAid countryName ->
            execAction1
                [ snd >> _.hasIntel, "You don't have intel on target country"
                  snd >> _.compliance >> (>) 1f, "The country is already fully compliant" ]
                EAbilityType.economic_support
                countryName
        | Reanimate countryName ->
            execAction1
                [ snd >> _.GetReanimateValue() >> (<) 0, "The country must have dead people" ]
                EAbilityType.reanimate
                countryName
        | Rampage countryName ->
            execAction1
                [ snd >> _.apeInfectedPopulation >> (<) 0, "The country must have infected apes" ]
                EAbilityType.rampage
                countryName
        | LethalBoost countryName ->
            execAction1
                [ (fun _ -> CGameManager.IsCoopMPGame), "The game must be coop multiplayer"
                  (fun (_, ld) -> (ld :?> CoopLocalDisease).lethalBoostCounter <= 0),
                  "The ability is on cooldown in that country" ]
                EAbilityType.lethal_boost
                countryName
        | InfectBoost countryName ->
            execAction1
                [ (fun _ -> CGameManager.IsCoopMPGame), "The game must be coop multiplayer"
                  (fun (_, ld) -> (ld :?> CoopLocalDisease).infectBoostCounter <= 0),
                  "The ability is on cooldown in that country" ]
                EAbilityType.infect_boost
                countryName
        | ImmuneShock countryName ->
            execAction1
                [ (fun _ -> CGameManager.IsVersusMPGame), "The game must be versus multiplayer"
                  (fun (_, ld) -> ld.uncontrolledInfected + ld.controlledInfected > 0),
                  "The target country must have infected people"
                  (fun (_, ld) -> (ld :?> MPLocalDisease).immuneShockCounter <= 0),
                  "The ability is on cooldown in that country" ]
                EAbilityType.immune_shock
                countryName
        | FieldOperatives countryName ->
            execAction1
                [ snd >> _.hasTeam >> not, "The field operatives are already in that country"
                  snd >> _.disease.vampires.Count >> (<) 0, "You don't have any field operatives"
                  snd >> _.disease.vampires.[0].currentCountry >> (<>) null,
                  "Unknown error: field operatives exist, but aren't present in the world" ]
                EAbilityType.investigation_team
                countryName
        | CreateLair countryName ->
            execAction1
                [ snd >> _.zombie >> (<) 0, "The country must have vampires"
                  snd >> _.castleState >> (=) ECastleState.CASTLE_NONE, "The country already has a lair" ]
                EAbilityType.castle
                countryName
        | CreateColony countryName ->
            execAction1
                [ snd >> _.apeInfectedPopulation >> (<) 0, "The country must have infected apes"
                  fst >> _.hasApeColony >> not, "The country already has an ape colony" ]
                EAbilityType.bloodrage
                countryName
        | ToggleBloodRage countryName ->
            execAction1
                [ snd >> _.zombie >> (<) 0, "The country must have vampires" ]
                EAbilityType.bloodrage
                countryName
        | BenignMimic countryName ->
            let ld2 ((c, ld): Country * LocalDisease) =
                if World.instance.diseases.Count = 2 then
                    c.GetLocalDisease(
                        World.instance.diseases[if World.instance.diseases.[0] = ld.disease then 1 else 0]
                    )
                else
                    null

            execAction1
                [ (fun _ -> CGameManager.IsVersusMPGame), "The game must be versus multiplayer"
                  snd >> _.allInfected >> (<) 0, "The country must have infected people"
                  (fun (c, ld) -> ld.allInfected + c.deadPopulation >= c.originalPopulation),
                  "You haven't infected enough people in this country to use this ability"
                  (fun (_, ld) -> (ld :?> MPLocalDisease).benignMimicCounter <= 0),
                  "The ability is on cooldown in that country"
                  (ld2
                   >> fun ld2 -> ld2 <> null && (ld2 :?> MPLocalDisease).benignMimicCounter <= 0),
                  "The ability is on cooldown in that country" ]
                EAbilityType.benign_mimic
                countryName
        | Nuke countryName -> postBubbleDifferentTargetAction Nuke EHudMode.NukeStrike countryName
        | SendTrojanPlane countryName -> postBubbleDifferentTargetAction SendTrojanPlane EHudMode.Neurax countryName
        | SendZombieHordeTo countryName ->
            postBubbleDifferentTargetAction SendZombieHordeTo EHudMode.SendHorde countryName
        | ClickBubble(countryName, bubbleName) ->
            let ub =
                typeof<CInterfaceManager>
                    .GetField("mpUserBubble", BindingFlags.NonPublic ||| BindingFlags.Instance)
                    .GetValue
                    CInterfaceManager.instance
                :?> BonusObject

            let allBubbles =
                (CInterfaceManager.instance.mpBonuses |> List.ofSeq)
                @ if ub = null then [] else [ ub ]
                |> List.filter (fun bubble ->
                    let getBool name =
                        typeof<BonusObject>.GetField(name, BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue
                            bubble
                        :?> bool

                    bubble <> null
                    && bubble.mpCountry <> null
                    && not (getBool "mbUnclickable")
                    && not (getBool "mbBlocked")
                    && not (getBool "mbClicked")
                    && not (getBool "mbDead")
                    && bubble.isVisible)
                |> List.map (fun bubble -> bubble, Context.bubble bubble)

            if
                not (CInterfaceManager.instance.CanClickBubble())
                || CUIManager.instance.IsHovering()
                || CGameManager.game.CurrentGameState = IGame.GameState.EndGame
            then
                Error(Some "You can't currently click bubbles")
            elif List.isEmpty allBubbles then
                Error(Some "There aren't any bubbles to click")
            else
                match
                    allBubbles
                    |> List.tryFind (snd >> fun x -> x.bubbleName = bubbleName && x.countryName = countryName)
                with
                | Some(bubble, _) ->
                    UICamera.hoveredObject <- null
                    bubble.OnMouseDown()

                    match CHUDScreen.instance.HudInterfaceMode with
                    | EHudMode.Normal -> Ok None
                    | EHudMode.Neurax ->
                        CGameManager.SetPaused(true, true)
                        Ok(Some "You will now have to select the infected plane destination")
                    | EHudMode.SendHorde ->
                        CGameManager.SetPaused(true, true)
                        Ok(Some "You will now have to select the zombie horde destination")
                    | EHudMode.NukeStrike ->
                        CGameManager.SetPaused(true, true)
                        Ok(Some "You will now have to select the nuke target")
                    | x ->
                        this.LogError $"Unknown HUD mode after clicking bubble: {x}"
                        Ok None
                | None ->
                    Error(Some $"Bubble not found! Available bubbles: {allBubbles |> List.map snd |> this.Serialize}")
        | CloseGameEndScreen ->
            let mutable over = false

            while not over do
                match CUIManager.instance.GetCurrentScreen() with
                | :? CResultScreen as screen -> screen.GoToEndScreen()
                | :? CEndGameScreen as screen -> screen.OnClickExit()
                | _ -> over <- true

            Ok None
        | OpenTransmissionScreen ->
            let screen = CUIManager.instance.GetScreen "DiseaseScreen" :?> CDiseaseScreen
            screen.diseaseToggles.[1].value <- true
            Ok(Some "Currently viewing transmissions")
        | OpenSymptomsScreen ->
            let screen = CUIManager.instance.GetScreen "DiseaseScreen" :?> CDiseaseScreen
            screen.diseaseToggles.[2].value <- true
            Ok(Some "Currently viewing symptoms")
        | OpenAbilitiesScreen ->
            let screen = CUIManager.instance.GetScreen "DiseaseScreen" :?> CDiseaseScreen
            screen.diseaseToggles.[3].value <- true
            Ok(Some "Currently viewing abilities")
        | EvolveAbility abilityName -> findTech _.abilityName abilityName "Ability" "Abilities" |> Result.bind evolve
        | EvolveSymptom symptomName -> findTech _.symptomName symptomName "Symptom" "Symptoms" |> Result.bind evolve
        | EvolveTransmission transmissionName ->
            findTech _.transmissionName transmissionName "Transmission" "Transmission"
            |> Result.bind evolve
        | DevolveAbility abilityName -> findTech _.abilityName abilityName "Ability" "Abilities" |> Result.bind devolve
        | DevolveSymptom symptomName -> findTech _.symptomName symptomName "Symptom" "Symptoms" |> Result.bind devolve
        | DevolveTransmission transmissionName ->
            findTech _.transmissionName transmissionName "Transmission" "Transmission"
            |> Result.bind devolve
        | CloseEvolutionScreen ->
            CUIManager.instance.HideOverlay "Red_Confirm_Overlay_Devolve"
            CUIManager.instance.HideOverlay "Red_Confirm_Overlay_Devolve_Cure"
            CUIManager.instance.SetActiveScreen "HUDScreen"
            CGameManager.SetPaused(false, true)
            Ok(Some "Returned to the game")
        | OpenEvolutionScreen ->
            CUIManager.instance.SetActiveScreen "DiseaseScreen"
            CGameManager.SetPaused(true, true)
            let screen = CUIManager.instance.GetScreen "DiseaseScreen" :?> CDiseaseScreen
            screen.diseaseToggles.[0].value <- true
            Ok None
        | QueryCountries(tags, tagsNot, count, sortBy, sortDescending) ->
            let map = map ()

            let countries =
                map
                |> Seq.map (_.GetCountry() >> Context.country)
                |> Seq.filter (fun country ->
                    tags
                    |> Option.defaultValue List.empty
                    |> List.forall (fun tag -> country.tags |> Option.exists (List.contains tag))
                    && tagsNot
                       |> Option.defaultValue List.empty
                       |> List.forall (fun tag -> country.tags |> Option.forall (List.contains tag >> not) |> not))

            let countries =
                match sortBy with
                | None -> countries
                | Some sortBy ->
                    countries
                    |> Seq.sortBy (
                        match sortBy with
                        | ``Healthy%`` -> _.healthyPercent
                        | ``Infected%`` -> _.infectedPercent
                        | ``Dead%`` -> _.deadPercent
                        | CureInvestment -> _.cureInvestmentDollars
                        | ``PublicOrder%`` -> _.publicOrderPercent
                        | ``Zombie%`` -> _.zombiePercent
                        | ``ApeHealthy%`` -> _.apeHealthyPercent
                        | ``ApeInfected%`` -> _.apeInfectedPercent
                        | ``ApeDead%`` -> _.apeDeadPercent
                        | ``AuthorityLoss%`` -> _.authorityLossPercent
                        | ``Noncompliance%`` -> _.noncompliancePercent
                    )

            let countries =
                if Option.defaultValue false sortDescending then
                    countries |> Seq.rev |> Array.ofSeq
                else
                    countries |> Array.ofSeq

            let countries =
                match count with
                | Some x when Array.length countries > x -> Array.take x countries
                | _ -> countries

            Ok(Some $"{this.Serialize countries}")
        | FocusCountry countryName ->
            let map = map ()

            match countryName with
            | Some countryName when countryName <> "" ->
                match
                    map
                    |> Seq.tryFind (_.GetCountry() >> _.name >> CLocalisationManager.GetText >> (=) countryName)
                with
                | Some country ->
                    WorldMapController.instance.SetSelectedCountry country
                    Ok None
                | None -> Error(Some "This country doesn't exist!")
            | _ ->
                WorldMapController.instance.SetSelectedCountry()
                Ok None
        | SetGameSpeed speed ->
            if speed < 0 then
                Error(Some "Can't travel back in time!")
            elif speed > 3 then
                Error(Some "Max game speed is 3")
            else
                typeof<CHUDScreen>
                    .GetMethod(
                        "SetSpeed",
                        BindingFlags.NonPublic ||| BindingFlags.Instance,
                        null,
                        [| typeof<int> |],
                        null
                    )
                    .Invoke(CHUDScreen.instance, [| speed |])
                |> ignore

                if CGameManager.lastSpeed = 0 then
                    Ok(Some "Paused the game")
                else
                    Ok(Some $"New game speed is {CGameManager.lastSpeed}00%%")
        | ChooseStart countryName ->
            let screen = screen :?> CCountrySelect

            let map = map ()

            match
                map
                |> Seq.tryFind (_.GetCountry() >> _.name >> CLocalisationManager.GetText >> (=) countryName)
            with
            | Some country ->
                let pos = country.GetRandomPositionInsideCountry()

                let allowI =
                    typeof<CCountrySelect>
                        .GetField("mbAllowInteraction", BindingFlags.NonPublic ||| BindingFlags.Instance)
                        .GetValue
                        screen
                    :?> bool

                typeof<Camera_Zoom>
                    .GetField("mbIsScrolling", BindingFlags.NonPublic ||| BindingFlags.Instance)
                    .SetValue(Camera_Zoom.instance, false)

                this.LogDebug $"{Camera_Zoom.instance.IsScrolling} {allowI}"
                nextContextTime <- DateTime.MinValue
                newBubble <- false
                screen.OnCountryClick(country, pos, true)
                Ok(Some "You will now have to click the `Country Select` bubble")
            | None -> Error(Some "This country doesn't exist!")
        | ClosePopup ->
            cur
            |> List.iter (fun x ->
                match x.Value with
                | :? CHUDPopupSubScreen as x -> x.OnMessageClose()
                | _ -> ())

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
                Ok(Some $"Your plague is now named {name}")
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
                                Error(Some $"Gene {i + 1} not found, available options: {this.Serialize names}"))
                (Ok 0)
            |> Result.map (fun _ ->
                screen.OnClickNext()
                None)
        | ChoosePlague diseaseName ->
            let unlAll =
                CGameManager.localPlayerInfo.GetUnlockedDiseases()
                |> Seq.filter (fun x -> x <> Disease.EDiseaseType.SIMIAN_FLU && x <> Disease.EDiseaseType.VAMPIRE)
                |> Seq.map (fun x -> x, CGameManager.GetDiseaseNameLoc x)
                |> List.ofSeq

            match List.tryFind (snd >> (=) diseaseName) unlAll with
            | Some(x, _) ->
                let screen = CUIManager.instance.GetCurrentScreen() :?> CGSScreen
                let sub = screen.GetActiveSubScreen() :?> CGSDiseaseSubScreen
                sub.SetDiseaseType(x, true)
                screen.OnClickNext()
                Ok(Some $"You will play as {diseaseName}")
            | None ->
                $"This plague doesn't exist, or isn't unlocked! Available plagues: {unlAll |> List.map snd |> this.Serialize}"
                |> (Some >> Error)
        |> Result.map (fun x ->
            forceActs
            |> Option.iter (fun acts -> this.UnregisterActions(acts |> List.map _.Name))

            forceActs <- None
            x)

    override _.LogError error =
        let fff = "fff"
        plugin.Logger.LogError $"{DateTime.UtcNow}.{DateTime.UtcNow.ToString fff} {error}"

    override _.LogDebug error =
        let fff = "fff"
        plugin.Logger.LogInfo $"{DateTime.UtcNow}.{DateTime.UtcNow.ToString fff} {error}"

    member this.DoForce<'T> (ephemeral: bool) (ctx: 'T) (prompt: string) (acts: Action list) =
        let acts = acts |> List.filter _.Valid
        forceActs <- Some acts

        this.RetainActions(acts |> List.map (fun x -> x))

        this.Force
            { state = Some(this.Serialize ctx)
              query = prompt
              ephemeral_context = ephemeral
              action_names = acts |> List.map _.Name }

    member _.NewBubble() = newBubble <- true

    member this.News(text: string) =
        this.Context false $"Breaking news: {text}"

    member this.BubblePopped(bubble: BonusIcon) =
        // let ctx = this.Context true

        match bubble.``type`` with
        // | BonusIcon.EBonusIconType.DNA -> ctx "Clicked a DNA bubble, got some DNA"
        // | BonusIcon.EBonusIconType.CURE -> ctx "Clicked a cure bubble, slowed down cure research"
        // | BonusIcon.EBonusIconType.INFECT -> ctx "Clicked an infection bubble, got some DNA"
        // | BonusIcon.EBonusIconType.DEATH -> ctx "Clicked a death bubble, got some DNA"
        // | BonusIcon.EBonusIconType.NEURAX -> ctx "Clicked a Neurax bubble"
        // | BonusIcon.EBonusIconType.APE_COLONY -> ctx "Clicked an ape colony bubble, got some DNA"
        // | BonusIcon.EBonusIconType.CASTLE -> ctx "Clicked a castle bubble, got some DNA"
        | _ -> ()

    member this.Update() =
        COptionsManager.instance.BackgroundPause <- EState.Off
        UnityEngine.Application.runInBackground <- true
        COptionsManager.instance.mbPityMode <- true
        UnityEngine.PlayerPrefs.SetInt("isRunInBG", 1)
        // let sc = UnityEngine.Object.FindObjectOfType<CMainStartSubScreen>()
        let screen = CUIManager.instance.GetCurrentScreen() //.GetScreen("MainMenuScreen")

        if screen <> null && Option.isNone forceActs then
            let cur =
                typeof<IGameScreen>
                    .GetField("currentSubScreen", BindingFlags.NonPublic ||| BindingFlags.Instance)
                    .GetValue
                    screen
                :?> Generic.IDictionary<string, IGameSubScreen>

            let openSub = Seq.tryHead cur |> Option.map (fun x -> x.Key, x.Value)

            match screen.GetType().Name with
            | "IGameScreen"
            | "CSplashLoadingScreen" -> ()
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
                printfn $"{cur.Keys |> List.ofSeq}"

                match openSub with
                | Some("main", (:? CGSDiseaseSubScreen as x)) ->
                    let opts = x.allDiseases

                    if not (Array.isEmpty opts) then
                        let ctx = opts |> Array.map Context.disease
                        this.DoForce true ctx "Please pick a plague to play as." [ Actions.choosePlague this ctx ]
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

                        this.DoForce true c "Please pick genes for your plague." [ Actions.chooseGenes this c ]
                | Some("main", (:? CGSDifficultySubScreen as x)) ->
                    // hardcode easy difficulty
                    x.ChooseDifficulty(x.buttons.[0], true)
                | Some("main", (:? CGSNameSubScreen as x)) ->
                    let iv = x.input.value

                    if iv <> "" && not x.setupComplete then
                        let c = { previousName = iv }
                        this.DoForce true c "Please pick a name for your plague." [ this.Action ChooseName ]
                | Some(k, v) -> this.LogDebug $"CGS / {k} / {v.GetType().Name}"
                | _ -> ()
            | "CCountrySelect" ->
                // let screen = screen :?> CCountrySelect

                match openSub with
                | Some("popup", (:? CHUDPopupSubScreen as x)) ->
                    let ctx =
                        { title = x.title.text
                          description = x.description.text }

                    this.DoForce false ctx "Close the popup when you're ready to continue." [ this.Action ClosePopup ]
                | Some(k, v) -> this.LogDebug $"CCS / {k} / {v.GetType().Name}"
                | None ->
                    let ub =
                        typeof<CInterfaceManager>
                            .GetField("mpUserBubble", BindingFlags.NonPublic ||| BindingFlags.Instance)
                            .GetValue
                            CInterfaceManager.instance
                        :?> BonusObject

                    let getBool name =
                        typeof<BonusObject>.GetField(name, BindingFlags.NonPublic ||| BindingFlags.Instance).GetValue ub
                        :?> bool

                    if
                        (ub = null || not (getBool "mbClicked"))
                        && CGameManager.localPlayerInfo.disease.nexus = null
                    then
                        let ctx = Context.context ()
                        Actions.focusCountry true this |> ignore

                        this.DoForce
                            false
                            ctx
                            "Please first pick the starting country and then click the start bubble to confirm your selection."
                            ([ Actions.chooseStart this; Actions.queryCountries true this ]
                             @ Option.toList (Actions.clickBubble this ctx))
            | "CHUDScreen" ->
                let allActions: Action list =
                    [ Actions.setGameSpeed this
                      this.Action OpenEvolutionScreen
                      Actions.queryCountries false this
                      Actions.focusCountry false this ]
                    @ (cur
                       |> Seq.map (fun sub ->
                           match sub.Key, sub.Value with
                           | "popup", (:? CHUDPopupSubScreen as x) ->
                               let ctx =
                                   { title = x.title.text
                                     description = x.description.text }

                               this.DoForce
                                   false
                                   ctx
                                   "Close the popup when you're ready to continue."
                                   [ this.Action ClosePopup; this.Action OpenEvolutionScreen ]

                               []
                           | "Context", :? CHUDContextSubScreen -> []
                           | "ability", (:? CHUDAbilitySubScreen as sub) ->
                               let d = CGameManager.localPlayerInfo.disease
                               let curDate = (DateTime World.instance.startDate).AddDays d.turnNumber
                               let forceUpdate = curDate >= nextContextTime

                               let btns =
                                   typeof<CHUDAbilitySubScreen>
                                       .GetField(
                                           "abilitiesButtonMap",
                                           BindingFlags.NonPublic ||| BindingFlags.Instance
                                       )
                                       .GetValue
                                       sub
                                   :?> Generic.Dictionary<EAbilityType, AbilityButton>

                               btns
                               |> Seq.collect (fun item ->
                                   let aab = item.Value

                                   match item.Key with
                                   // necroa: Active ability enables neurological regeneration in the brains of infected corpses - effectively re-animating some of the dead
                                   // Reanimate: Cellular regeneration makes some dead zombies rise again. Requires 'dead' population. Press button then click on country to select target. Target country must have dead within. (Cost: %s DNA)
                                   | EAbilityType.reanimate -> Actions.reanimate forceUpdate this aab |> Option.toList
                                   // necroa: Active ability lets Zombies form hordes to travel across land and water and attack new lands
                                   // Zombie Horde: Gather a horde of zombies to send to another country. Requires 'zombie' population. Press button and click on a start country. Then select destination country. Start country must have zombies in. (Cost: %s DNA)
                                   | EAbilityType.zombie_horde ->
                                       Actions.zombieHorde forceUpdate this aab |> Option.toList
                                   // simian flu: Apes gain ability to rampage against humans - targeting labs and rescuing captive apes
                                   // Rampage: Intelligent apes come together to build a community, rescue others and protect themselves
                                   | EAbilityType.rampage -> Actions.apeRampage forceUpdate this aab |> Option.toList
                                   // simian flu: Apes able to plan and co-ordinate travel together - allowing movement of ape groups over long distances
                                   // Travel: Intelligent apes leave their homes and move to a new location
                                   | EAbilityType.move -> Actions.apeTravel forceUpdate this aab |> Option.toList
                                   // simian flu: Intelligent apes can come together to build a colony which increases ape transmission, protects and generate DNA
                                   // Create Colony: Intelligent apes come together to build a community, rescue others and protect themselves
                                   | EAbilityType.create_colony ->
                                       Actions.createColony forceUpdate this aab |> Option.toList
                                   // mp: Unlock an Active Ability that lets you send an infected person to a country of your choice
                                   // Unscheduled Flight: Infected people travel to another country. Can only be sent from a country with at least 1,000 infected population
                                   | EAbilityType.unscheduled_flight ->
                                       Actions.unscheduledFlight forceUpdate this aab |> Option.toList
                                   // mp: Unlock an Active Ability that makes it significantly easier to kill people infected with both diseases
                                   // Immune Shock: Increase your infectivity and make it significantly easier to kill people infected by both diseases in a target country
                                   | EAbilityType.immune_shock ->
                                       Actions.immuneShock forceUpdate this aab |> Option.toList
                                   // mp: Unlock an Active Ability that forces a country to focus cure efforts on the other disease, whilst your disease hides
                                   // Benign Mimic: Target country gains significant boost to research of opponent's disease. (Target must be fully infected and your disease goes dormant there)
                                   | EAbilityType.benign_mimic ->
                                       Actions.benignMimic forceUpdate this aab |> Option.toList
                                   // mp: Unlock an Active Ability that lets you boost infectivity in a country
                                   // Infect Boost: Increase infectivity of both diseases in a target country
                                   | EAbilityType.infect_boost ->
                                       Actions.infectBoost forceUpdate this aab |> Option.toList
                                   // mp: Unlock an Active Ability that lets you boost lethality in a country
                                   // Lethal Boost: Increase lethality of both diseases in a target country
                                   | EAbilityType.lethal_boost ->
                                       Actions.lethalBoost forceUpdate this aab |> Option.toList
                                   // vampire: Vampire gains ability to enter a Blood Rage - will attack research/military facilities if present, otherwise consuming people for DNA
                                   // Blood Rage: Vampire attacks research/military facilities if present, otherwise goes on killing spree to get DNA
                                   | EAbilityType.bloodrage -> Actions.bloodRage forceUpdate this aab |> Option.toList
                                   // vampire: Vampire gains ability to temporarily mutate into a vast, winged bat-like creature. Allows travel to nearby countries
                                   // Vampire Flight: Send a vampire to another country
                                   | EAbilityType.vampiretravel ->
                                       Actions.vampireFlight forceUpdate this aab |> Option.toList
                                   // vampire: Vampire able to create a lair for it to rest in and heal. Also generates DNA (Infected population increases DNA)
                                   // Create Lair: Create a lair for the vampire to heal in. Also generates DNA based on infected population
                                   | EAbilityType.castle -> Actions.createLair forceUpdate this aab |> Option.toList
                                   // Not technically unlocked by any tech, instead this aa ability is unlocked internally by native code
                                   // Field Operatives: Send the Field Operatives to a target country.
                                   | EAbilityType.investigation_team ->
                                       Actions.fieldOperatives forceUpdate this aab |> Option.toList
                                   // cure: Force a target country to shut border connections and lock down for 1 month.\nNon-Compliance will increase if the public thinks it is unnecessary.
                                   // Targeted Quarantine: Force a target country to shut border connections and lock down for 1 month.
                                   | EAbilityType.raise_priority ->
                                       Actions.targetedQuarantine forceUpdate this aab |> Option.toList
                                   // cure: Send significant financial aid package the targeted country.
                                   // Targeted Economic Aid: Send significant financial aid package to help a country struggling with Quarantine measures.\nDecrease Non-Compliance in target country
                                   | EAbilityType.economic_support ->
                                       Actions.targetedEconomicAid forceUpdate this aab |> Option.toList
                                   // never unlocked, but usable, might be unlockable with scenarios? probably not
                                   // this is also used internally when a nuke bubble is spawned
                                   | EAbilityType.nuke ->
                                       this.LogError $"Warning: nuke ability"
                                       []
                                   // never unlocked, but used internally
                                   | EAbilityType.neurax ->
                                       this.LogError $"Warning: neurax ability"
                                       []
                                   // cut content?
                                   | EAbilityType.bubble ->
                                       this.LogError $"Warning: bubble ability"
                                       []
                                   | EAbilityType.help ->
                                       this.LogError $"Warning: help ability"
                                       []
                                   | x ->
                                       this.LogError $"Warning: unknown ability type {x}"
                                       [])
                               |> List.ofSeq
                           | key, value ->
                               this.LogDebug $"CHUDS / {key} / {value.GetType().Name}"
                               [])
                       |> Seq.concat
                       |> List.ofSeq)

                if Option.isNone forceActs then
                    match CHUDScreen.instance.HudInterfaceMode with
                    | EHudMode.Disabled -> ()
                    | EHudMode.Normal ->
                        let d = CGameManager.localPlayerInfo.disease
                        let curDate = (DateTime World.instance.startDate).AddDays d.turnNumber

                        let allActions =
                            if curDate >= nextContextTime || newBubble then
                                newBubble <- false
                                // send context every 5 seconds
                                nextContextTime <-
                                    curDate.AddDays(
                                        match CGameManager.localPlayerInfo.gameSpeed with
                                        | 0 -> 1
                                        | x -> float (x * 5)
                                    )

                                let ctx = Context.context ()
                                this.Context true (this.Serialize ctx)
                                Option.toList (Actions.clickBubble this ctx) @ allActions
                            else
                                // don't update bubble action if context isn't being refreshed
                                this.Action ClickBubble :: allActions
                            |> List.map (fun x -> x :> obj)

                        this.RetainActions allActions
                    | EHudMode.Neurax ->
                        CGameManager.SetPaused(true, true)

                        this.DoForce
                            false
                            ""
                            "Please pick a destination for the trojan plane"
                            [ Actions.sendTrojanPlane this; Actions.queryCountries true this ]
                    | EHudMode.NukeStrike ->
                        CGameManager.SetPaused(true, true)

                        this.DoForce
                            false
                            ""
                            "Please pick a destination for the nuke"
                            [ Actions.nuke this; Actions.queryCountries true this ]
                    | EHudMode.SendHorde ->
                        CGameManager.SetPaused(true, true)

                        this.DoForce
                            false
                            ""
                            "Please pick a destination for the zombie horde"
                            [ Actions.sendZombieHordeTo this; Actions.queryCountries true this ]
                    | EHudMode.EconomicSupport
                    | EHudMode.RaisePriority
                    | EHudMode.SendInvestigationTeam
                    | EHudMode.CreateCastle
                    | EHudMode.SendVampire
                    | EHudMode.SelectVampire
                    | EHudMode.BloodRage
                    | EHudMode.LethalBoost
                    | EHudMode.InfectBoost
                    | EHudMode.BenignMimic
                    | EHudMode.ImmuneShock
                    | EHudMode.SendUnscheduledFlight
                    | EHudMode.SelectUnscheduledFlight
                    | EHudMode.MoveGem
                    | EHudMode.PlaceGem
                    | EHudMode.ApeCreateColony
                    | EHudMode.SendApeColony
                    | EHudMode.SendApeHorde
                    | EHudMode.SelectApeHorde
                    | EHudMode.ApeRampage
                    | EHudMode.SelectHorde
                    | EHudMode.Reanimate ->
                        this.LogError
                            "Invalid HUD mode, this should never happen! Active actions have to be triggered with the API"

                        CHUDScreen.instance.Default()
                    | _ ->
                        this.LogError "Unknown HUD mode, resetting to default"
                        CHUDScreen.instance.Default()


            | "CDiseaseScreen" ->
                let screen = screen :?> CDiseaseScreen

                let tab =
                    typeof<CDiseaseScreen>
                        .GetField("currentSubscreen", BindingFlags.NonPublic ||| BindingFlags.Instance)
                        .GetValue
                        screen
                    :?> int

                let d = CGameManager.localPlayerInfo.disease
                let techMap = d.technologies |> Seq.map (fun x -> x.id, x) |> Map.ofSeq

                let techs =
                    d.technologies
                    |> Seq.map (Context.tech techMap)
                    |> Seq.filter (
                        (match tab with
                         | 1 -> _.transmissionName
                         | 2 -> _.symptomName
                         | 3 -> _.abilityName
                         | _ -> fun _ -> None)
                        >> Option.isSome
                    )
                    |> List.ofSeq

                let evolve, devolve, name, nameFn =
                    match tab with
                    | 1 -> Some EvolveTransmission, Some DevolveTransmission, "transmissionName", _.transmissionName
                    | 2 -> Some EvolveSymptom, Some DevolveSymptom, "symptomName", _.symptomName
                    | 3 -> Some EvolveAbility, Some DevolveAbility, "abilityName", _.abilityName
                    | _ -> None, None, "", (fun _ -> None)

                let devolve = if Context.enableDevolves then devolve else None

                let acts =
                    (if tab = 1 then
                         []
                     else
                         [ this.Action OpenTransmissionScreen ])
                    @ (if tab = 2 then [] else [ this.Action OpenSymptomsScreen ])
                    @ (if tab = 3 then [] else [ this.Action OpenAbilitiesScreen ])
                    @ [ this.Action CloseEvolutionScreen ]
                    @ match evolve with
                      | Some act ->
                          let act = this.Action act

                          act.MutateProp name (fun x ->
                              (x :?> StringSchema)
                                  .SetEnum(
                                      techs
                                      |> List.filter (_.canEvolve >> Option.exists id)
                                      |> List.filter (_.evolutionCost.Value >> (>=) d.evoPoints)
                                      |> List.collect (nameFn >> Option.toList)
                                      |> Array.ofList
                                  ))

                          [ act ]
                      | None -> []
                    @ match devolve with
                      | Some act ->
                          let act = this.Action act

                          act.MutateProp name (fun x ->
                              (x :?> StringSchema)
                                  .SetEnum(
                                      techs
                                      |> List.filter (_.canDevolve >> Option.exists id)
                                      |> List.filter (_.devolutionCost.Value >> (>=) d.evoPoints)
                                      |> List.collect (nameFn >> Option.toList)
                                      |> Array.ofList
                                  ))

                          [ act ]
                      | None -> []

                let ctx =
                    { dnaPoints = d.evoPoints
                      ``globalInfectivity%`` = int d.globalInfectiousnessMax
                      ``globalSeverity%`` = int d.globalSeverityMax
                      ``globalLethality%`` = int d.globalLethalityMax
                      transmissions = if tab = 1 then Some techs else None
                      symptoms = if tab = 2 then Some techs else None
                      abilities = if tab = 3 then Some techs else None }

                this.DoForce
                    (tab = 0)
                    ctx
                    (if tab = 0 then
                         "Choose a screen. You can switch to a different screen at any time."
                     else
                         "Please pick something to evolve, switch to a different screen, or close the evolution screen to go back to the game.")
                    acts

                ()
            | "CEndGameScreen"
            | "CResultScreen" ->
                let title, desc =
                    match screen with
                    | :? CEndGameScreen as screen -> screen.resultTitle.text, screen.resultDescription.text
                    | :? CResultScreen as screen -> screen.resultTitle.text, screen.resultDescription.text
                    | _ -> "", ""

                if DateTime.MaxValue > nextContextTime then
                    nextContextTime <- DateTime.MaxValue

                    let won =
                        if World.instance.gameWon then
                            "You win! "
                        else
                            "Game over: "

                    $"{won} {title} - {desc}" |> this.Context false

                this.RetainActions [ this.Action CloseGameEndScreen ]
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
                    let game = Game this
                    game.Start(None, cts.Token) |> ignore
                    game
                )

            this.Logger.LogInfo $"Plugin Nuerax is loaded with {cnt} patches!"
        with exc ->
            this.Logger.LogError $"ERROR {exc}"

    member _.LateUpdate() = game |> Option.iter _.Update()
