package mrtjp.projectred.transmission

import codechicken.lib.datagen.ItemModelProvider
import codechicken.lib.datagen.recipe.RecipeProvider
import codechicken.lib.gui.SimpleItemGroup
import codechicken.lib.util.{CCLTags, CrashLock}
import codechicken.multipart.api.part.TMultiPart
import codechicken.multipart.api.{MultiPartType, SimpleMultiPartType}
import mrtjp.projectred.ProjectRedTransmission.MOD_ID
import mrtjp.projectred.core.CoreContent._
import mrtjp.projectred.transmission.TransmissionContent._
import net.minecraft.data.{BlockTagsProvider, DataGenerator, ItemTagsProvider}
import net.minecraft.item.ItemStack
import net.minecraft.tags.ItemTags
import net.minecraft.util.ResourceLocation
import net.minecraftforge.common.Tags.{Items => ForgeItemTags}
import net.minecraftforge.common.data.ExistingFileHelper
import net.minecraftforge.eventbus.api.{IEventBus, SubscribeEvent}
import net.minecraftforge.fml.event.lifecycle.GatherDataEvent
import net.minecraftforge.registries.{DeferredRegister, ForgeRegistries}

import java.util.function.Supplier

object TransmissionContent {

    private val LOCK = new CrashLock("Already Initialized.")
    private val ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, MOD_ID)
    private val PARTS = DeferredRegister.create(classOf[MultiPartType[_]], MOD_ID)

    val transmissionItemGroup = new SimpleItemGroup(MOD_ID, () => new ItemStack(itemRedAlloyWire.get()))

    /** Items */
    val itemRedAlloyWire = ITEMS.register("red_alloy_wire", itemPartWire(WireType.RED_ALLOY))

    val itemInsulatedWhiteWire = ITEMS.register("white_insulated_wire", itemPartWire(WireType.INSULATED_WHITE))
    val itemInsulatedOrangeWire = ITEMS.register("orange_insulated_wire", itemPartWire(WireType.INSULATED_ORANGE))
    val itemInsulatedMagentaWire = ITEMS.register("magenta_insulated_wire", itemPartWire(WireType.INSULATED_MAGENTA))
    val itemInsulatedLightBlueWire = ITEMS.register("light_blue_insulated_wire", itemPartWire(WireType.INSULATED_LIGHT_BLUE))
    val itemInsulatedYellowWire = ITEMS.register("yellow_insulated_wire", itemPartWire(WireType.INSULATED_YELLOW))
    val itemInsulatedLimeWire = ITEMS.register("lime_insulated_wire", itemPartWire(WireType.INSULATED_LIME))
    val itemInsulatedPinkWire = ITEMS.register("pink_insulated_wire", itemPartWire(WireType.INSULATED_PINK))
    val itemInsulatedGrayWire = ITEMS.register("gray_insulated_wire", itemPartWire(WireType.INSULATED_GRAY))
    val itemInsulatedLightGrayWire = ITEMS.register("light_gray_insulated_wire", itemPartWire(WireType.INSULATED_LIGHT_GRAY))
    val itemInsulatedCyanWire = ITEMS.register("cyan_insulated_wire", itemPartWire(WireType.INSULATED_CYAN))
    val itemInsulatedPurpleWire = ITEMS.register("purple_insulated_wire", itemPartWire(WireType.INSULATED_PURPLE))
    val itemInsulatedBlueWire = ITEMS.register("blue_insulated_wire", itemPartWire(WireType.INSULATED_BLUE))
    val itemInsulatedBrownWire = ITEMS.register("brown_insulated_wire", itemPartWire(WireType.INSULATED_BROWN))
    val itemInsulatedGreenWire = ITEMS.register("green_insulated_wire", itemPartWire(WireType.INSULATED_GREEN))
    val itemInsulatedRedWire = ITEMS.register("red_insulated_wire", itemPartWire(WireType.INSULATED_RED))
    val itemInsulatedBlackWire = ITEMS.register("black_insulated_wire", itemPartWire(WireType.INSULATED_BLACK))

    val itemBundledNeutralWire = ITEMS.register("neutral_bundled_wire", itemPartWire(WireType.BUNDLED_NEUTRAL))
    val itemBundledWhiteWire = ITEMS.register("white_bundled_wire", itemPartWire(WireType.BUNDLED_WHITE))
    val itemBundledOrangeWire = ITEMS.register("orange_bundled_wire", itemPartWire(WireType.BUNDLED_ORANGE))
    val itemBundledMagentaWire = ITEMS.register("magenta_bundled_wire", itemPartWire(WireType.BUNDLED_MAGENTA))
    val itemBundledLightBlueWire = ITEMS.register("light_blue_bundled_wire", itemPartWire(WireType.BUNDLED_LIGHT_BLUE))
    val itemBundledYellowWire = ITEMS.register("yellow_bundled_wire", itemPartWire(WireType.BUNDLED_YELLOW))
    val itemBundledLimeWire = ITEMS.register("lime_bundled_wire", itemPartWire(WireType.BUNDLED_LIME))
    val itemBundledPinkWire = ITEMS.register("pink_bundled_wire", itemPartWire(WireType.BUNDLED_PINK))
    val itemBundledGrayWire = ITEMS.register("gray_bundled_wire", itemPartWire(WireType.BUNDLED_GRAY))
    val itemBundledLightGrayWire = ITEMS.register("light_gray_bundled_wire", itemPartWire(WireType.BUNDLED_LIGHT_GRAY))
    val itemBundledCyanWire = ITEMS.register("cyan_bundled_wire", itemPartWire(WireType.BUNDLED_CYAN))
    val itemBundledPurpleWire = ITEMS.register("purple_bundled_wire", itemPartWire(WireType.BUNDLED_PURPLE))
    val itemBundledBlueWire = ITEMS.register("blue_bundled_wire", itemPartWire(WireType.BUNDLED_BLUE))
    val itemBundledBrownWire = ITEMS.register("brown_bundled_wire", itemPartWire(WireType.BUNDLED_BROWN))
    val itemBundledGreenWire = ITEMS.register("green_bundled_wire", itemPartWire(WireType.BUNDLED_GREEN))
    val itemBundledRedWire = ITEMS.register("red_bundled_wire", itemPartWire(WireType.BUNDLED_RED))
    val itemBundledBlackWire = ITEMS.register("black_bundled_wire", itemPartWire(WireType.BUNDLED_BLACK))

    val itemPowerLowLoadWire = ITEMS.register("low_load_power_wire", itemPartWire(WireType.POWER_LOWLOAD))

    val itemFramedRedAlloyWire = ITEMS.register("framed_red_alloy_wire", itemPartFramedWire(WireType.FRAMED_RED_ALLOY))

    val itemFramedInsulatedWhiteWire = ITEMS.register("white_framed_insulated_wire", itemPartFramedWire(WireType.FRAMED_INSULATED_WHITE))
    val itemFramedInsulatedOrangeWire = ITEMS.register("orange_framed_insulated_wire", itemPartFramedWire(WireType.FRAMED_INSULATED_ORANGE))
    val itemFramedInsulatedMagentaWire = ITEMS.register("magenta_framed_insulated_wire", itemPartFramedWire(WireType.FRAMED_INSULATED_MAGENTA))
    val itemFramedInsulatedLightBlueWire = ITEMS.register("light_blue_framed_insulated_wire", itemPartFramedWire(WireType.FRAMED_INSULATED_LIGHT_BLUE))
    val itemFramedInsulatedYellowWire = ITEMS.register("yellow_framed_insulated_wire", itemPartFramedWire(WireType.FRAMED_INSULATED_YELLOW))
    val itemFramedInsulatedLimeWire = ITEMS.register("lime_framed_insulated_wire", itemPartFramedWire(WireType.FRAMED_INSULATED_LIME))
    val itemFramedInsulatedPinkWire = ITEMS.register("pink_framed_insulated_wire", itemPartFramedWire(WireType.FRAMED_INSULATED_PINK))
    val itemFramedInsulatedGrayWire = ITEMS.register("gray_framed_insulated_wire", itemPartFramedWire(WireType.FRAMED_INSULATED_GRAY))
    val itemFramedInsulatedLightGrayWire = ITEMS.register("light_gray_framed_insulated_wire", itemPartFramedWire(WireType.FRAMED_INSULATED_LIGHT_GRAY))
    val itemFramedInsulatedCyanWire = ITEMS.register("cyan_framed_insulated_wire", itemPartFramedWire(WireType.FRAMED_INSULATED_CYAN))
    val itemFramedInsulatedPurpleWire = ITEMS.register("purple_framed_insulated_wire", itemPartFramedWire(WireType.FRAMED_INSULATED_PURPLE))
    val itemFramedInsulatedBlueWire = ITEMS.register("blue_framed_insulated_wire", itemPartFramedWire(WireType.FRAMED_INSULATED_BLUE))
    val itemFramedInsulatedBrownWire = ITEMS.register("brown_framed_insulated_wire", itemPartFramedWire(WireType.FRAMED_INSULATED_BROWN))
    val itemFramedInsulatedGreenWire = ITEMS.register("green_framed_insulated_wire", itemPartFramedWire(WireType.FRAMED_INSULATED_GREEN))
    val itemFramedInsulatedRedWire = ITEMS.register("red_framed_insulated_wire", itemPartFramedWire(WireType.FRAMED_INSULATED_RED))
    val itemFramedInsulatedBlackWire = ITEMS.register("black_framed_insulated_wire", itemPartFramedWire(WireType.FRAMED_INSULATED_BLACK))

    val itemFramedBundledNeutralWire = ITEMS.register("neutral_framed_bundled_wire", itemPartFramedWire(WireType.FRAMED_BUNDLED_NEUTRAL))
    val itemFramedBundledWhiteWire = ITEMS.register("white_framed_bundled_wire", itemPartFramedWire(WireType.FRAMED_BUNDLED_WHITE))
    val itemFramedBundledOrangeWire = ITEMS.register("orange_framed_bundled_wire", itemPartFramedWire(WireType.FRAMED_BUNDLED_ORANGE))
    val itemFramedBundledMagentaWire = ITEMS.register("magenta_framed_bundled_wire", itemPartFramedWire(WireType.FRAMED_BUNDLED_MAGENTA))
    val itemFramedBundledLightBlueWire = ITEMS.register("light_blue_framed_bundled_wire", itemPartFramedWire(WireType.FRAMED_BUNDLED_LIGHT_BLUE))
    val itemFramedBundledYellowWire = ITEMS.register("yellow_framed_bundled_wire", itemPartFramedWire(WireType.FRAMED_BUNDLED_YELLOW))
    val itemFramedBundledLimeWire = ITEMS.register("lime_framed_bundled_wire", itemPartFramedWire(WireType.FRAMED_BUNDLED_LIME))
    val itemFramedBundledPinkWire = ITEMS.register("pink_framed_bundled_wire", itemPartFramedWire(WireType.FRAMED_BUNDLED_PINK))
    val itemFramedBundledGrayWire = ITEMS.register("gray_framed_bundled_wire", itemPartFramedWire(WireType.FRAMED_BUNDLED_GRAY))
    val itemFramedBundledLightGrayWire = ITEMS.register("light_gray_framed_bundled_wire", itemPartFramedWire(WireType.FRAMED_BUNDLED_LIGHT_GRAY))
    val itemFramedBundledCyanWire = ITEMS.register("cyan_framed_bundled_wire", itemPartFramedWire(WireType.FRAMED_BUNDLED_CYAN))
    val itemFramedBundledPurpleWire = ITEMS.register("purple_framed_bundled_wire", itemPartFramedWire(WireType.FRAMED_BUNDLED_PURPLE))
    val itemFramedBundledBlueWire = ITEMS.register("blue_framed_bundled_wire", itemPartFramedWire(WireType.FRAMED_BUNDLED_BLUE))
    val itemFramedBundledBrownWire = ITEMS.register("brown_framed_bundled_wire", itemPartFramedWire(WireType.FRAMED_BUNDLED_BROWN))
    val itemFramedBundledGreenWire = ITEMS.register("green_framed_bundled_wire", itemPartFramedWire(WireType.FRAMED_BUNDLED_GREEN))
    val itemFramedBundledRedWire = ITEMS.register("red_framed_bundled_wire", itemPartFramedWire(WireType.FRAMED_BUNDLED_RED))
    val itemFramedBundledBlackWire = ITEMS.register("black_framed_bundled_wire", itemPartFramedWire(WireType.FRAMED_BUNDLED_BLACK))

    val itemFramedPowerLowLoadWire = ITEMS.register("low_load_framed_power_wire", itemPartFramedWire(WireType.FRAMED_POWER_LOWLOAD))

    /** Item Groups */
    lazy val insulatedWires = List(
        itemInsulatedWhiteWire.get,
        itemInsulatedOrangeWire.get,
        itemInsulatedMagentaWire.get,
        itemInsulatedLightBlueWire.get,
        itemInsulatedYellowWire.get,
        itemInsulatedLimeWire.get,
        itemInsulatedPinkWire.get,
        itemInsulatedGrayWire.get,
        itemInsulatedLightGrayWire.get,
        itemInsulatedCyanWire.get,
        itemInsulatedPurpleWire.get,
        itemInsulatedBlueWire.get,
        itemInsulatedBrownWire.get,
        itemInsulatedGreenWire.get,
        itemInsulatedRedWire.get,
        itemInsulatedBlackWire.get
    )

    lazy val bundledWires = List(
        itemBundledWhiteWire.get,
        itemBundledOrangeWire.get,
        itemBundledMagentaWire.get,
        itemBundledLightBlueWire.get,
        itemBundledYellowWire.get,
        itemBundledLimeWire.get,
        itemBundledPinkWire.get,
        itemBundledGrayWire.get,
        itemBundledLightGrayWire.get,
        itemBundledCyanWire.get,
        itemBundledPurpleWire.get,
        itemBundledBlueWire.get,
        itemBundledBrownWire.get,
        itemBundledGreenWire.get,
        itemBundledRedWire.get,
        itemBundledBlackWire.get
    )

    lazy val framedInsulatedWires = List(
        itemFramedInsulatedWhiteWire.get,
        itemFramedInsulatedOrangeWire.get,
        itemFramedInsulatedMagentaWire.get,
        itemFramedInsulatedLightBlueWire.get,
        itemFramedInsulatedYellowWire.get,
        itemFramedInsulatedLimeWire.get,
        itemFramedInsulatedPinkWire.get,
        itemFramedInsulatedGrayWire.get,
        itemFramedInsulatedLightGrayWire.get,
        itemFramedInsulatedCyanWire.get,
        itemFramedInsulatedPurpleWire.get,
        itemFramedInsulatedBlueWire.get,
        itemFramedInsulatedBrownWire.get,
        itemFramedInsulatedGreenWire.get,
        itemFramedInsulatedRedWire.get,
        itemFramedInsulatedBlackWire.get
    )

    lazy val framedBundledWires = List(
        itemFramedBundledWhiteWire.get,
        itemFramedBundledOrangeWire.get,
        itemFramedBundledMagentaWire.get,
        itemFramedBundledLightBlueWire.get,
        itemFramedBundledYellowWire.get,
        itemFramedBundledLimeWire.get,
        itemFramedBundledPinkWire.get,
        itemFramedBundledGrayWire.get,
        itemFramedBundledLightGrayWire.get,
        itemFramedBundledCyanWire.get,
        itemFramedBundledPurpleWire.get,
        itemFramedBundledBlueWire.get,
        itemFramedBundledBrownWire.get,
        itemFramedBundledGreenWire.get,
        itemFramedBundledRedWire.get,
        itemFramedBundledBlackWire.get
    )

    /** Parts */
    val partRedAlloyWire = PARTS.register("red_alloy_wire", wirePart(WireType.RED_ALLOY))

    val partInsulatedWhiteWire = PARTS.register("white_insulated_wire", wirePart(WireType.INSULATED_WHITE))
    val partInsulatedOrangeWire = PARTS.register("orange_insulated_wire", wirePart(WireType.INSULATED_ORANGE))
    val partInsulatedMagentaWire = PARTS.register("magenta_insulated_wire", wirePart(WireType.INSULATED_MAGENTA))
    val partInsulatedLightBlueWire = PARTS.register("light_blue_insulated_wire", wirePart(WireType.INSULATED_LIGHT_BLUE))
    val partInsulatedYellowWire = PARTS.register("yellow_insulated_wire", wirePart(WireType.INSULATED_YELLOW))
    val partInsulatedLimeWire = PARTS.register("lime_insulated_wire", wirePart(WireType.INSULATED_LIME))
    val partInsulatedPinkWire = PARTS.register("pink_insulated_wire", wirePart(WireType.INSULATED_PINK))
    val partInsulatedGrayWire = PARTS.register("gray_insulated_wire", wirePart(WireType.INSULATED_GRAY))
    val partInsulatedLightGrayWire = PARTS.register("light_gray_insulated_wire", wirePart(WireType.INSULATED_LIGHT_GRAY))
    val partInsulatedCyanWire = PARTS.register("cyan_insulated_wire", wirePart(WireType.INSULATED_CYAN))
    val partInsulatedPurpleWire = PARTS.register("purple_insulated_wire", wirePart(WireType.INSULATED_PURPLE))
    val partInsulatedBlueWire = PARTS.register("blue_insulated_wire", wirePart(WireType.INSULATED_BLUE))
    val partInsulatedBrownWire = PARTS.register("brown_insulated_wire", wirePart(WireType.INSULATED_BROWN))
    val partInsulatedGreenWire = PARTS.register("green_insulated_wire", wirePart(WireType.INSULATED_GREEN))
    val partInsulatedRedWire = PARTS.register("red_insulated_wire", wirePart(WireType.INSULATED_RED))
    val partInsulatedBlackWire = PARTS.register("black_insulated_wire", wirePart(WireType.INSULATED_BLACK))

    val partBundledNeutralWire = PARTS.register("neutral_bundled_wire", wirePart(WireType.BUNDLED_NEUTRAL))
    val partBundledWhiteWire = PARTS.register("white_bundled_wire", wirePart(WireType.BUNDLED_WHITE))
    val partBundledOrangeWire = PARTS.register("orange_bundled_wire", wirePart(WireType.BUNDLED_ORANGE))
    val partBundledMagentaWire = PARTS.register("magenta_bundled_wire", wirePart(WireType.BUNDLED_MAGENTA))
    val partBundledLightBlueWire = PARTS.register("light_blue_bundled_wire", wirePart(WireType.BUNDLED_LIGHT_BLUE))
    val partBundledYellowWire = PARTS.register("yellow_bundled_wire", wirePart(WireType.BUNDLED_YELLOW))
    val partBundledLimeWire = PARTS.register("lime_bundled_wire", wirePart(WireType.BUNDLED_LIME))
    val partBundledPinkWire = PARTS.register("pink_bundled_wire", wirePart(WireType.BUNDLED_PINK))
    val partBundledGrayWire = PARTS.register("gray_bundled_wire", wirePart(WireType.BUNDLED_GRAY))
    val partBundledLightGrayWire = PARTS.register("light_gray_bundled_wire", wirePart(WireType.BUNDLED_LIGHT_GRAY))
    val partBundledCyanWire = PARTS.register("cyan_bundled_wire", wirePart(WireType.BUNDLED_CYAN))
    val partBundledPurpleWire = PARTS.register("purple_bundled_wire", wirePart(WireType.BUNDLED_PURPLE))
    val partBundledBlueWire = PARTS.register("blue_bundled_wire", wirePart(WireType.BUNDLED_BLUE))
    val partBundledBrownWire = PARTS.register("brown_bundled_wire", wirePart(WireType.BUNDLED_BROWN))
    val partBundledGreenWire = PARTS.register("green_bundled_wire", wirePart(WireType.BUNDLED_GREEN))
    val partBundledRedWire = PARTS.register("red_bundled_wire", wirePart(WireType.BUNDLED_RED))
    val partBundledBlackWire = PARTS.register("black_bundled_wire", wirePart(WireType.BUNDLED_BLACK))

    val partPowerLowLoadWire = PARTS.register("low_load_power_wire", wirePart(WireType.POWER_LOWLOAD))

    val partFramedRedAlloyWire = PARTS.register("framed_red_alloy_wire", wirePart(WireType.FRAMED_RED_ALLOY))

    val partFramedInsulatedWhiteWire = PARTS.register("white_framed_insulated_wire", wirePart(WireType.FRAMED_INSULATED_WHITE))
    val partFramedInsulatedOrangeWire = PARTS.register("orange_framed_insulated_wire", wirePart(WireType.FRAMED_INSULATED_ORANGE))
    val partFramedInsulatedMagentaWire = PARTS.register("magenta_framed_insulated_wire", wirePart(WireType.FRAMED_INSULATED_MAGENTA))
    val partFramedInsulatedLightBlueWire = PARTS.register("light_blue_framed_insulated_wire", wirePart(WireType.FRAMED_INSULATED_LIGHT_BLUE))
    val partFramedInsulatedYellowWire = PARTS.register("yellow_framed_insulated_wire", wirePart(WireType.FRAMED_INSULATED_YELLOW))
    val partFramedInsulatedLimeWire = PARTS.register("lime_framed_insulated_wire", wirePart(WireType.FRAMED_INSULATED_LIME))
    val partFramedInsulatedPinkWire = PARTS.register("pink_framed_insulated_wire", wirePart(WireType.FRAMED_INSULATED_PINK))
    val partFramedInsulatedGrayWire = PARTS.register("gray_framed_insulated_wire", wirePart(WireType.FRAMED_INSULATED_GRAY))
    val partFramedInsulatedLightGrayWire = PARTS.register("light_gray_framed_insulated_wire", wirePart(WireType.FRAMED_INSULATED_LIGHT_GRAY))
    val partFramedInsulatedCyanWire = PARTS.register("cyan_framed_insulated_wire", wirePart(WireType.FRAMED_INSULATED_CYAN))
    val partFramedInsulatedPurpleWire = PARTS.register("purple_framed_insulated_wire", wirePart(WireType.FRAMED_INSULATED_PURPLE))
    val partFramedInsulatedBlueWire = PARTS.register("blue_framed_insulated_wire", wirePart(WireType.FRAMED_INSULATED_BLUE))
    val partFramedInsulatedBrownWire = PARTS.register("brown_framed_insulated_wire", wirePart(WireType.FRAMED_INSULATED_BROWN))
    val partFramedInsulatedGreenWire = PARTS.register("green_framed_insulated_wire", wirePart(WireType.FRAMED_INSULATED_GREEN))
    val partFramedInsulatedRedWire = PARTS.register("red_framed_insulated_wire", wirePart(WireType.FRAMED_INSULATED_RED))
    val partFramedInsulatedBlackWire = PARTS.register("black_framed_insulated_wire", wirePart(WireType.FRAMED_INSULATED_BLACK))

    val partFramedBundledNeutralWire = PARTS.register("neutral_framed_bundled_wire", wirePart(WireType.FRAMED_BUNDLED_NEUTRAL))
    val partFramedBundledWhiteWire = PARTS.register("white_framed_bundled_wire", wirePart(WireType.FRAMED_BUNDLED_WHITE))
    val partFramedBundledOrangeWire = PARTS.register("orange_framed_bundled_wire", wirePart(WireType.FRAMED_BUNDLED_ORANGE))
    val partFramedBundledMagentaWire = PARTS.register("magenta_framed_bundled_wire", wirePart(WireType.FRAMED_BUNDLED_MAGENTA))
    val partFramedBundledLightBlueWire = PARTS.register("light_blue_framed_bundled_wire", wirePart(WireType.FRAMED_BUNDLED_LIGHT_BLUE))
    val partFramedBundledYellowWire = PARTS.register("yellow_framed_bundled_wire", wirePart(WireType.FRAMED_BUNDLED_YELLOW))
    val partFramedBundledLimeWire = PARTS.register("lime_framed_bundled_wire", wirePart(WireType.FRAMED_BUNDLED_LIME))
    val partFramedBundledPinkWire = PARTS.register("pink_framed_bundled_wire", wirePart(WireType.FRAMED_BUNDLED_PINK))
    val partFramedBundledGrayWire = PARTS.register("gray_framed_bundled_wire", wirePart(WireType.FRAMED_BUNDLED_GRAY))
    val partFramedBundledLightGrayWire = PARTS.register("light_gray_framed_bundled_wire", wirePart(WireType.FRAMED_BUNDLED_LIGHT_GRAY))
    val partFramedBundledCyanWire = PARTS.register("cyan_framed_bundled_wire", wirePart(WireType.FRAMED_BUNDLED_CYAN))
    val partFramedBundledPurpleWire = PARTS.register("purple_framed_bundled_wire", wirePart(WireType.FRAMED_BUNDLED_PURPLE))
    val partFramedBundledBlueWire = PARTS.register("blue_framed_bundled_wire", wirePart(WireType.FRAMED_BUNDLED_BLUE))
    val partFramedBundledBrownWire = PARTS.register("brown_framed_bundled_wire", wirePart(WireType.FRAMED_BUNDLED_BROWN))
    val partFramedBundledGreenWire = PARTS.register("green_framed_bundled_wire", wirePart(WireType.FRAMED_BUNDLED_GREEN))
    val partFramedBundledRedWire = PARTS.register("red_framed_bundled_wire", wirePart(WireType.FRAMED_BUNDLED_RED))
    val partFramedBundledBlackWire = PARTS.register("black_framed_bundled_wire", wirePart(WireType.FRAMED_BUNDLED_BLACK))

    val partFramedPowerLowLoadWire = PARTS.register("low_load_framed_power_wire", wirePart(WireType.FRAMED_POWER_LOWLOAD))

    /** Item Tags */
    val tagItemInsulatedWire = ItemTags.bind(new ResourceLocation(MOD_ID, "insulated_wire"))
    val tagItemBundledWire = ItemTags.bind(new ResourceLocation(MOD_ID, "bundled_wire"))
    val tagItemInsulatedFramedWire = ItemTags.bind(new ResourceLocation(MOD_ID, "framed_insulated_wire"))
    val tagItemBundledFramedWire = ItemTags.bind(new ResourceLocation(MOD_ID, "framed_bundled_wire"))

    private def itemPartWire(wireType: WireType): Supplier[ItemPartWire] = () => new ItemPartWire(wireType)

    private def itemPartFramedWire(wireType: WireType): Supplier[ItemPartFramedWire] = () => new ItemPartFramedWire(wireType)

    private def wirePart(wireType: WireType): Supplier[MultiPartType[_]] = () => new SimpleMultiPartType[TMultiPart](wireType)

    def register(bus: IEventBus) {
        LOCK.lock()
        ITEMS.register(bus)
        PARTS.register(bus)
        bus.register(DataGen)
    }

}

private object DataGen {

    @SubscribeEvent
    def gatherDataGenerators(event: GatherDataEvent) {
        val gen = event.getGenerator
        val helper = event.getExistingFileHelper
        if (event.includeClient()) {
            gen.addProvider(new ItemModels(gen, helper))
        }
        if (event.includeServer()) {
            gen.addProvider(new ItemTags(gen, helper))
            gen.addProvider(new Recipes(gen))
        }
        //        gen.addProvider(new BlockTags(gen))
        //        gen.addProvider(new BlockLootTables(gen))
    }
}

private class ItemModels(gen: DataGenerator, fileHelper:ExistingFileHelper) extends ItemModelProvider(gen, MOD_ID, fileHelper) {

    override def getName = "ProjectRed-Transmission Item Models."

    override protected def registerModels() {
        val wire = getExistingFile(new ResourceLocation(MOD_ID, "item/wire"))
        val framedWire = getExistingFile(new ResourceLocation(MOD_ID, "item/framed_wire"))

        getSimple(itemRedAlloyWire).texture(null).parent(wire)

        getSimple(itemInsulatedWhiteWire).texture(null).parent(wire)
        getSimple(itemInsulatedOrangeWire).texture(null).parent(wire)
        getSimple(itemInsulatedMagentaWire).texture(null).parent(wire)
        getSimple(itemInsulatedLightBlueWire).texture(null).parent(wire)
        getSimple(itemInsulatedYellowWire).texture(null).parent(wire)
        getSimple(itemInsulatedLimeWire).texture(null).parent(wire)
        getSimple(itemInsulatedPinkWire).texture(null).parent(wire)
        getSimple(itemInsulatedGrayWire).texture(null).parent(wire)
        getSimple(itemInsulatedLightGrayWire).texture(null).parent(wire)
        getSimple(itemInsulatedCyanWire).texture(null).parent(wire)
        getSimple(itemInsulatedPurpleWire).texture(null).parent(wire)
        getSimple(itemInsulatedBlueWire).texture(null).parent(wire)
        getSimple(itemInsulatedBrownWire).texture(null).parent(wire)
        getSimple(itemInsulatedGreenWire).texture(null).parent(wire)
        getSimple(itemInsulatedRedWire).texture(null).parent(wire)
        getSimple(itemInsulatedBlackWire).texture(null).parent(wire)

        getSimple(itemBundledNeutralWire).texture(null).parent(wire)
        getSimple(itemBundledWhiteWire).texture(null).parent(wire)
        getSimple(itemBundledOrangeWire).texture(null).parent(wire)
        getSimple(itemBundledMagentaWire).texture(null).parent(wire)
        getSimple(itemBundledLightBlueWire).texture(null).parent(wire)
        getSimple(itemBundledYellowWire).texture(null).parent(wire)
        getSimple(itemBundledLimeWire).texture(null).parent(wire)
        getSimple(itemBundledPinkWire).texture(null).parent(wire)
        getSimple(itemBundledGrayWire).texture(null).parent(wire)
        getSimple(itemBundledLightGrayWire).texture(null).parent(wire)
        getSimple(itemBundledCyanWire).texture(null).parent(wire)
        getSimple(itemBundledPurpleWire).texture(null).parent(wire)
        getSimple(itemBundledBlueWire).texture(null).parent(wire)
        getSimple(itemBundledBrownWire).texture(null).parent(wire)
        getSimple(itemBundledGreenWire).texture(null).parent(wire)
        getSimple(itemBundledRedWire).texture(null).parent(wire)
        getSimple(itemBundledBlackWire).texture(null).parent(wire)
        getSimple(itemPowerLowLoadWire).texture(null).parent(wire)

        getSimple(itemFramedRedAlloyWire).texture(null).parent(framedWire)

        getSimple(itemFramedInsulatedWhiteWire).texture(null).parent(framedWire)
        getSimple(itemFramedInsulatedOrangeWire).texture(null).parent(framedWire)
        getSimple(itemFramedInsulatedMagentaWire).texture(null).parent(framedWire)
        getSimple(itemFramedInsulatedLightBlueWire).texture(null).parent(framedWire)
        getSimple(itemFramedInsulatedYellowWire).texture(null).parent(framedWire)
        getSimple(itemFramedInsulatedLimeWire).texture(null).parent(framedWire)
        getSimple(itemFramedInsulatedPinkWire).texture(null).parent(framedWire)
        getSimple(itemFramedInsulatedGrayWire).texture(null).parent(framedWire)
        getSimple(itemFramedInsulatedLightGrayWire).texture(null).parent(framedWire)
        getSimple(itemFramedInsulatedCyanWire).texture(null).parent(framedWire)
        getSimple(itemFramedInsulatedPurpleWire).texture(null).parent(framedWire)
        getSimple(itemFramedInsulatedBlueWire).texture(null).parent(framedWire)
        getSimple(itemFramedInsulatedBrownWire).texture(null).parent(framedWire)
        getSimple(itemFramedInsulatedGreenWire).texture(null).parent(framedWire)
        getSimple(itemFramedInsulatedRedWire).texture(null).parent(framedWire)
        getSimple(itemFramedInsulatedBlackWire).texture(null).parent(framedWire)

        getSimple(itemFramedBundledNeutralWire).texture(null).parent(framedWire)
        getSimple(itemFramedBundledWhiteWire).texture(null).parent(framedWire)
        getSimple(itemFramedBundledOrangeWire).texture(null).parent(framedWire)
        getSimple(itemFramedBundledMagentaWire).texture(null).parent(framedWire)
        getSimple(itemFramedBundledLightBlueWire).texture(null).parent(framedWire)
        getSimple(itemFramedBundledYellowWire).texture(null).parent(framedWire)
        getSimple(itemFramedBundledLimeWire).texture(null).parent(framedWire)
        getSimple(itemFramedBundledPinkWire).texture(null).parent(framedWire)
        getSimple(itemFramedBundledGrayWire).texture(null).parent(framedWire)
        getSimple(itemFramedBundledLightGrayWire).texture(null).parent(framedWire)
        getSimple(itemFramedBundledCyanWire).texture(null).parent(framedWire)
        getSimple(itemFramedBundledPurpleWire).texture(null).parent(framedWire)
        getSimple(itemFramedBundledBlueWire).texture(null).parent(framedWire)
        getSimple(itemFramedBundledBrownWire).texture(null).parent(framedWire)
        getSimple(itemFramedBundledGreenWire).texture(null).parent(framedWire)
        getSimple(itemFramedBundledRedWire).texture(null).parent(framedWire)
        getSimple(itemFramedBundledBlackWire).texture(null).parent(framedWire)

        getSimple(itemFramedPowerLowLoadWire).texture(null).parent(framedWire)
    }
}

private class ItemTags(gen: DataGenerator, fileHelper:ExistingFileHelper) extends ItemTagsProvider(gen, new BlockTagsProvider(gen, MOD_ID, fileHelper), MOD_ID, fileHelper) {
    override def getName = "ProjectRed-Transmission Item Tags."

    override protected def addTags() {
        tag(tagItemInsulatedWire)
            .add(itemInsulatedWhiteWire)
            .add(itemInsulatedOrangeWire)
            .add(itemInsulatedMagentaWire)
            .add(itemInsulatedLightBlueWire)
            .add(itemInsulatedYellowWire)
            .add(itemInsulatedLimeWire)
            .add(itemInsulatedPinkWire)
            .add(itemInsulatedGrayWire)
            .add(itemInsulatedLightGrayWire)
            .add(itemInsulatedCyanWire)
            .add(itemInsulatedPurpleWire)
            .add(itemInsulatedBlueWire)
            .add(itemInsulatedBrownWire)
            .add(itemInsulatedGreenWire)
            .add(itemInsulatedRedWire)
            .add(itemInsulatedBlackWire)

        tag(tagItemBundledWire)
            .add(itemBundledWhiteWire)
            .add(itemBundledOrangeWire)
            .add(itemBundledMagentaWire)
            .add(itemBundledLightBlueWire)
            .add(itemBundledYellowWire)
            .add(itemBundledLimeWire)
            .add(itemBundledPinkWire)
            .add(itemBundledGrayWire)
            .add(itemBundledLightGrayWire)
            .add(itemBundledCyanWire)
            .add(itemBundledPurpleWire)
            .add(itemBundledBlueWire)
            .add(itemBundledBrownWire)
            .add(itemBundledGreenWire)
            .add(itemBundledRedWire)
            .add(itemBundledBlackWire)

        tag(tagItemInsulatedFramedWire)
            .add(itemFramedInsulatedWhiteWire)
            .add(itemFramedInsulatedOrangeWire)
            .add(itemFramedInsulatedMagentaWire)
            .add(itemFramedInsulatedLightBlueWire)
            .add(itemFramedInsulatedYellowWire)
            .add(itemFramedInsulatedLimeWire)
            .add(itemFramedInsulatedPinkWire)
            .add(itemFramedInsulatedGrayWire)
            .add(itemFramedInsulatedLightGrayWire)
            .add(itemFramedInsulatedCyanWire)
            .add(itemFramedInsulatedPurpleWire)
            .add(itemFramedInsulatedBlueWire)
            .add(itemFramedInsulatedBrownWire)
            .add(itemFramedInsulatedGreenWire)
            .add(itemFramedInsulatedRedWire)
            .add(itemFramedInsulatedBlackWire)

        tag(tagItemBundledFramedWire)
            .add(itemFramedBundledWhiteWire)
            .add(itemFramedBundledOrangeWire)
            .add(itemFramedBundledMagentaWire)
            .add(itemFramedBundledLightBlueWire)
            .add(itemFramedBundledYellowWire)
            .add(itemFramedBundledLimeWire)
            .add(itemFramedBundledPinkWire)
            .add(itemFramedBundledGrayWire)
            .add(itemFramedBundledLightGrayWire)
            .add(itemFramedBundledCyanWire)
            .add(itemFramedBundledPurpleWire)
            .add(itemFramedBundledBlueWire)
            .add(itemFramedBundledBrownWire)
            .add(itemFramedBundledGreenWire)
            .add(itemFramedBundledRedWire)
            .add(itemFramedBundledBlackWire)
    }
}

private class Recipes(gen: DataGenerator) extends RecipeProvider(gen) {

    override def getName = "ProjectRed-Transmission Recipes."

    override protected def registerRecipes() {

        shapedRecipe(itemRedAlloyWire, 12)
            .key('R', itemRedIngot)
            .patternLine(" R ")
            .patternLine(" R ")
            .patternLine(" R ")

        //Insulated wires.
        for (w <- insulatedWires) {
            shapedRecipe(w, 12)
                .key('W', ItemTags.bind(w.wireType.getColour.getWoolTagName))
                .key('R', itemRedIngot)
                .patternLine("WRW")
                .patternLine("WRW")
                .patternLine("WRW")

            shapelessRecipe(w, 1, new ResourceLocation(w.getRegistryName + "_re_color"))
                .addIngredient(tagItemInsulatedWire)
                .addIngredient(ItemTags.bind(w.wireType.getColour.getDyeTagName))
        }

        //Bundled wires
        shapedRecipe(itemBundledNeutralWire)
            .key('S', ForgeItemTags.STRING)
            .key('W', tagItemInsulatedWire)
            .patternLine("SWS")
            .patternLine("WWW")
            .patternLine("SWS")

        for (w <- bundledWires) {
            shapelessRecipe(w, 1, new ResourceLocation(w.getRegistryName + "_re_color"))
                .addIngredient(tagItemBundledWire)
                .addIngredient(ItemTags.bind(w.wireType.getColour.getDyeTagName))
        }

        //Framed wires
        shapedRecipe(itemFramedRedAlloyWire)
            .key('S', ForgeItemTags.RODS_WOODEN)
            .key('I', itemRedAlloyWire)
            .patternLine("SSS")
            .patternLine("SIS")
            .patternLine("SSS")

        shapedRecipe(itemFramedBundledNeutralWire)
            .key('S', ForgeItemTags.RODS_WOODEN)
            .key('I', itemBundledNeutralWire)
            .patternLine("SSS")
            .patternLine("SIS")
            .patternLine("SSS")

        shapedRecipe(itemFramedPowerLowLoadWire)
            .key('S', ForgeItemTags.RODS_WOODEN)
            .key('I', itemPowerLowLoadWire)
            .patternLine("SSS")
            .patternLine("SIS")
            .patternLine("SSS")

        for (i <- insulatedWires.indices) {
            val w = framedInsulatedWires(i);
            shapedRecipe(w)
                .key('S', ForgeItemTags.RODS_WOODEN)
                .key('I', insulatedWires(i))
                .patternLine("SSS")
                .patternLine("SIS")
                .patternLine("SSS")

            shapelessRecipe(w, 1, new ResourceLocation(w.getRegistryName + "_re_color"))
                .addIngredient(tagItemInsulatedFramedWire)
                .addIngredient(ItemTags.bind(w.wireType.getColour.getDyeTagName))
        }

        for (i <- bundledWires.indices) {
            val w = framedBundledWires(i);
            shapedRecipe(w)
                    .key('S', ForgeItemTags.RODS_WOODEN)
                    .key('I', bundledWires(i))
                    .patternLine("SSS")
                    .patternLine("SIS")
                    .patternLine("SSS")

            shapelessRecipe(w, 1, new ResourceLocation(w.getRegistryName + "_re_color"))
                    .addIngredient(tagItemBundledFramedWire)
                    .addIngredient(ItemTags.bind(w.wireType.getColour.getDyeTagName))
        }

        shapedRecipe(itemPowerLowLoadWire, 12)
            .key('I', itemElectrotineIngot)
            .key('B', CCLTags.Items.WOOL_BLUE)
            .key('Y', CCLTags.Items.WOOL_YELLOW)
            .patternLine("BIB")
            .patternLine("YIY")
            .patternLine("BIB")

        shapedRecipe(itemWiredPlate, 1, new ResourceLocation(MOD_ID, itemWiredPlate.getRegistryName.getPath))
            .key('R', itemRedAlloyWire)
            .key('P', itemPlate)
            .patternLine("R")
            .patternLine("P")

        shapedRecipe(itemBundledPlate, 1, new ResourceLocation(MOD_ID, itemBundledPlate.getRegistryName.getPath))
            .key('R', itemBundledNeutralWire)
            .key('P', itemPlate)
            .patternLine("R")
            .patternLine("P")
    }


}