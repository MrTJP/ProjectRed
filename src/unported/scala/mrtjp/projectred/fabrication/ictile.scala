/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.render.CCRenderState
import codechicken.lib.vec.Transformation
import mrtjp.core.util.Enum
import mrtjp.core.vec.Point
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

import scala.collection.mutable.ListBuffer

object ICTileDefs extends Enum
{
    type EnumVal = ICTileDef

//    val Torch = CircuitPartDef(() => new TorchICPart)
    val Lever = ICTileDef(() => new LeverICTile)
    val Button = ICTileDef(() => new ButtonICTile)

    val AlloyWire = ICTileDef(() => new AlloyWireICTile)
    val InsulatedWire = ICTileDef(() => new InsulatedWireICTile)
    val BundledCable = ICTileDef(() => new BundledCableICTile)

    val IOGate = ICTileDef(() => new IOGateICTile)
    val SimpleGate = ICTileDef(() => new ComboGateICTile)
    val ComplexGate = ICTileDef(() => new SequentialGateICTile)
    val ArrayGate = ICTileDef(() => new ArrayGateICTile)

    case class ICTileDef(factory:() => ICTile) extends Value
    {
        def id = ordinal
        override def name = s"$id"

        def createPart = factory.apply()
    }
}

object ICTile
{
    def createTile(id:Int) = ICTileDefs(id).createPart
}

abstract class ICTile extends ISETile
{
    var editor:ICTileMapEditor = null
    var tileMap:ICTileMapContainer = null
    var pos:Point = null

    def bindEditor(ic:ICTileMapEditor)
    {
        editor = ic
        bindTileMap(ic.tileMapContainer)
    }

    def bindTileMap(tm:ICTileMapContainer)
    {
        tileMap = tm
    }

    def bindPos(p:Point)
    {
        pos = p
    }

    def unbind()
    {
        editor = null
        tileMap = null
        pos = null
    }

    def id = getPartType.id

    def getPartType:ICTileDefs.ICTileDef

    def save(tag:NBTTagCompound){}
    def load(tag:NBTTagCompound){}

    def writeDesc(out:MCDataOutput){}
    def readDesc(in:MCDataInput){}

    def writeStreamOf(key:Int):MCDataOutput = editor.network.getTileStream(pos).writeByte(key)
    def read(in:MCDataInput) { read(in, in.readUByte()) }
    def read(in:MCDataInput, key:Int) = key match {
        case 0 => readDesc(in)
        case _ =>
    }

    def sendDescUpdate() { writeDesc(writeStreamOf(0)) }

    def update(){}
    def scheduledTick(){}
    def scheduleTick(ticks:Int){ editor.scheduleTick(pos, ticks) }

    def onAdded(){}
    def onRemoved(){}

    def onNeighborChanged(){}

    def onRegistersChanged(regIDs:Set[Int])//alerts part if any register in the circuit has changed.

    @SideOnly(Side.CLIENT)
    def onClicked(){}
    @SideOnly(Side.CLIENT)
    def onActivated(){}

    @SideOnly(Side.CLIENT)
    def getPartName:String
    @SideOnly(Side.CLIENT)
    def getPickOp:TileEditorOp = null
    @SideOnly(Side.CLIENT)
    def buildRolloverData(buffer:ListBuffer[String])
    {
        buffer += getPartName
    }

    @SideOnly(Side.CLIENT)
    def renderDynamic(ccrs:CCRenderState, t:Transformation, ortho:Boolean, frame:Float){}
}

trait TClientNetICTile extends ICTile
{
    def readClientPacket(in:MCDataInput)

    @SideOnly(Side.CLIENT)
    def sendClientPacket(writer:MCDataOutput => Unit = {_ => })
    {
        editor.sendClientPacket(this, writer)
    }
}

trait IGuiICTile extends TClientNetICTile
{
    @SideOnly(Side.CLIENT)
    def createGui:ICTileGui
}