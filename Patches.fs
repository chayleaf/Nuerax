namespace Nuerax

open System
open System.Reflection
open HarmonyLib

[<HarmonyPatch>]
type public Patches() =
    static let justClickTheStupidThing (this: BonusObject) =
        let mbUnclickable =
            typeof<BonusObject>
                .GetField("mbUnclickable", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .GetValue(this)
            :?> bool

        let mbBlocked =
            typeof<BonusObject>
                .GetField("mbBlocked", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .GetValue(this)
            :?> bool

        let mbClicked =
            typeof<BonusObject>
                .GetField("mbClicked", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .GetValue(this)
            :?> bool

        let mbDead =
            typeof<BonusObject>
                .GetField("mbDead", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .GetValue(this)
            :?> bool

        let isVisible = this.isVisible
        let ty = this.``type``

        if
            not (CInterfaceManager.instance.CanClickBubble())
            || mbUnclickable
            || mbBlocked
            || mbClicked
            || mbDead
            || not isVisible
            || CGameManager.game.CurrentGameState = IGame.GameState.EndGame
        then
            ()
        else
            typeof<BonusObject>
                .GetField("mbBlocked", BindingFlags.NonPublic ||| BindingFlags.Instance)
                .SetValue(this, ty <> BonusIcon.EBonusIconType.NUKE)

            if not (String.IsNullOrEmpty(this.bubbleInteractFX)) then
                CSoundManager.instance.PlaySFX(this.bubbleInteractFX) |> ignore

            if ty = BonusIcon.EBonusIconType.COUNTRY_SELECT then
                this.DoBubblePop(CNetworkManager.network.LocalPlayerInfo.disease)
                CCountrySelect.instance.CountrySelected(this.mpCountry, this.transform.position)
            else
                if ty = BonusIcon.EBonusIconType.NEURAX || ty = BonusIcon.EBonusIconType.NECROA then
                    this.bonusCollider.enabled <- false
                    this.bonusAnimator.SetTrigger("Pulse")
                    this.bonusAnimator.speed <- 1f

                if (ty = BonusIcon.EBonusIconType.NUKE) then
                    CInterfaceManager.instance.nukeBubble <- this
                else
                    this.DoBubblePop(
                        CNetworkManager.network.LocalPlayerInfo.disease,
                        ty <> BonusIcon.EBonusIconType.NEURAX
                    )

                CInterfaceManager.instance.RegisterBubbleClick(this)
                CGameManager.game.ClickBonusIcon(this.mpBonus) |> ignore

            CInterfaceManager.instance.mbBubbleClick <- true

    [<HarmonyPatch(typeof<BonusObject>, "Update")>]
    [<HarmonyPostfix>]
    static member public BonusUpdate(__instance: BonusObject) = justClickTheStupidThing __instance
(*
    [<HarmonyPatch(typeof<CPlayerInfoSteam>, nameof Unchecked.defaultof<CPlayerInfoSteam>.GetDiseaseUnlocked)>]
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
    static member public CheatUnlock(__result: bool byref) = __result <- true
*)
