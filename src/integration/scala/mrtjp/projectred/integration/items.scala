/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.integration

import codechicken.multipart.api.ItemMultiPart
import mrtjp.projectred.core.PRLib
import net.minecraft.client.util.ITooltipFlag
import net.minecraft.item.{Item, ItemStack, ItemUseContext}
import net.minecraft.util.text.ITextComponent
import net.minecraft.world.World
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}

import java.util.{List => JList}

class ItemPartGate(val gateType:GateType) extends ItemMultiPart(new Item.Properties().group(IntegrationContent.integrationItemGroup))
{
    var infoBuilderFunc = {(stack:ItemStack, l:JList[ITextComponent]) => }

    override def newPart(context:ItemUseContext) = {
        val side = context.getFace
        val onPos = context.getPos.offset(side.getOpposite)
        if (!PRLib.canPlaceGateOnSide(context.getWorld, onPos, side)) {
            null
        } else {
            val gatePart = gateType.getPartType.createPartServer(null).asInstanceOf[GatePart]
            gatePart.preparePlacement(context.getPlayer, onPos, side.ordinal())
            gatePart
        }
    }

    @OnlyIn(Dist.CLIENT)
    override def addInformation(stack:ItemStack, worldIn:World, tooltip:JList[ITextComponent], flagIn:ITooltipFlag) {
        infoBuilderFunc(stack, tooltip)
    }
}

//object GateDefinition extends ItemDefinition
//{
//    override type EnumVal = GateDef
//    override def getItem = ProjectRedIntegration.itemPartGate
//
//    val typeSimpleGate = new ResourceLocation("projectred-integration:simple_gate")
//    val typeComplexGate = new ResourceLocation("projectred-integration:complex_gate")
//    val typeArrayGate = new ResourceLocation("projectred-integration:array_gate")
//    val typeBundledGate = new ResourceLocation("projectred-integration:bundled_gate")
//    val typeNeighborGate = new ResourceLocation("projectred-integration:neighbor_gate")
//    val typeICGate = new ResourceLocation("projectred-fabrication:ic_gate") //used by fabrication module
//
//    val OR = new GateDef(typeSimpleGate)
//    val NOR = new GateDef(typeSimpleGate)
//    val NOT = new GateDef(typeSimpleGate)
//    val AND = new GateDef(typeSimpleGate)
//    val NAND = new GateDef(typeSimpleGate)
//    val XOR = new GateDef(typeSimpleGate)
//    val XNOR = new GateDef(typeSimpleGate)
//    val Buffer = new GateDef(typeSimpleGate)
//    val Multiplexer = new GateDef(typeSimpleGate)
//    val Pulse = new GateDef(typeSimpleGate)
//    val Repeater = new GateDef(typeSimpleGate)
//    val Randomizer = new GateDef(typeSimpleGate)
//    val SRLatch = new GateDef(typeComplexGate)
//    val ToggleLatch = new GateDef(typeComplexGate)
//    val TransparentLatch = new GateDef(typeSimpleGate)
//    val LightSensor = new GateDef(typeSimpleGate)
//    val RainSensor = new GateDef(typeSimpleGate)
//    val Timer = new GateDef(typeComplexGate)
//    val Sequencer = new GateDef(typeComplexGate)
//    val Counter = new GateDef(typeComplexGate)
//    val StateCell = new GateDef(typeComplexGate)
//    val Synchronizer = new GateDef(typeComplexGate)
//    val BusTransceiver = new GateDef(typeBundledGate)
//    val NullCell = new GateDef(typeArrayGate)
//    val InvertCell = new GateDef(typeArrayGate)
//    val BufferCell = new GateDef(typeArrayGate)
//    val Comparator = new GateDef(typeNeighborGate)
//    val ANDCell = new GateDef(typeArrayGate)
//    val BusRandomizer = new GateDef(typeBundledGate)
//    val BusConverter = new GateDef(typeBundledGate)
//    val BusInputPanel = new GateDef(typeBundledGate)
//    val StackingLatch = new GateDef(typeArrayGate)
//    val SegmentDisplay = new GateDef(typeBundledGate)
//    val DecRandomizer = new GateDef(typeSimpleGate)
//    val ICGate = new GateDef(typeICGate, true) //fabrication module
//
//    class GateDef(val partname:ResourceLocation, val hidden:Boolean = false) extends ItemDef(partname.toString)
//    {
//        def implemented = partname != null
//    }
//}


