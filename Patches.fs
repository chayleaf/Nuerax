namespace Nuerax

open System.Reflection
open HarmonyLib

[<HarmonyPatch>]
type public Patches() =
    static let autoClick = false

    [<HarmonyPatch(typeof<BonusObject>, nameof Unchecked.defaultof<BonusObject>.Initialise)>]
    [<HarmonyPostfix>]
    static member public InitBubble(__instance: NewsItemObject) = MainClass.Instance.Game.NewBubble()

    [<HarmonyPatch(typeof<BonusObject>, "Update")>]
    [<HarmonyPostfix>]
    static member public BonusUpdate(__instance: BonusObject) =
        if autoClick then
            __instance.OnMouseDown()

    [<HarmonyPatch(typeof<NewsItemObject>, nameof Unchecked.defaultof<NewsItemObject>.SetNewsItem)>]
    [<HarmonyPostfix>]
    static member public News(__instance: NewsItemObject) =
        let field =
            typeof<NewsItemObject>
                .GetField("mpNewsText", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .GetValue(__instance)
            :?> UILabel

        if field <> null then
            MainClass.Instance.Game.News field.text

    [<HarmonyPatch(typeof<SPDisease>, nameof Unchecked.defaultof<SPDisease>.OnBonusIconClicked)>]
    [<HarmonyPostfix>]
    static member public BonusClicked(bonusIcon: BonusIcon) =
        if autoClick then
            MainClass.Instance.Game.BubblePopped bonusIcon

    (*[<HarmonyPatch(typeof<CPlayerInfoSteam>, nameof Unchecked.defaultof<CPlayerInfoSteam>.GetDiseaseUnlocked)>]
    [<HarmonyPostfix>]
    static member public DiseaseUnlock(__result: bool byref) = __result <- true

    [<HarmonyPatch(typeof<CPlayerInfoSteam>, nameof Unchecked.defaultof<CPlayerInfoSteam>.GetCureUnlocked)>]
    [<HarmonyPostfix>]
    static member public CureUnlock(__result: bool byref) = __result <- true

    [<HarmonyPatch(typeof<CPlayerInfoSteam>, nameof Unchecked.defaultof<CPlayerInfoSteam>.GetScenarioUnlocked)>]
    [<HarmonyPostfix>]
    static member public ScenarioUnlock(__result: bool byref) = __result <- true

    [<HarmonyPatch(typeof<CPlayerInfoSteam>, nameof Unchecked.defaultof<CPlayerInfoSteam>.GetGeneUnlocked)>]
    [<HarmonyPostfix>]
    static member public GeneUnlock(__result: bool byref) = __result <- true

    [<HarmonyPatch(typeof<CPlayerInfoSteam>, nameof Unchecked.defaultof<CPlayerInfoSteam>.GetCheatUnlocked)>]
    [<HarmonyPostfix>]
    static member public CheatUnlock(__result: bool byref) = __result <- true*)

    (*[<HarmonyPatch(typeof<CNetworkSteam>, "UserStatsReceived")>]
    [<HarmonyPostfix>]
    static member public ResetStats() = CPlayerInfoSteam.ClearAllSteamStats()*)

    [<HarmonyPatch(typeof<CUIManager>, nameof Unchecked.defaultof<CUIManager>.LocalisedConfirmOverlay)>]
    [<HarmonyPostfix>]
    static member public DontShowPopups(__result: CConfirmOverlay byref) =
        CUIManager.instance.HideOverlay __result

    [<HarmonyPatch(typeof<CMainMenuScreen>, "ShowDynamicPopup")>]
    [<HarmonyPrefix>]
    static member public DontShowDynamicPopups() = false

    [<HarmonyPatch(typeof<CUIManager>, nameof Unchecked.defaultof<CUIManager>.ShowUnlockPopup)>]
    [<HarmonyPrefix>]
    static member public DontShowUnlockPopups() = false
