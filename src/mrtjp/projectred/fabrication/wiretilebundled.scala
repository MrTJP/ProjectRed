/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.fabrication

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.render.CCRenderState
import codechicken.lib.vec.Transformation
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

trait IICBundledEmitter

trait IBundledCableICPart extends IICBundledEmitter
{
    def getBundledColour:Int
}

class BundledCableICTile extends WireICTile with IBundledCableICPart
{
    var colour:Byte = -1

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setByte("colour", colour)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        colour = tag.getByte("colour")
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeByte(colour)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        colour = in.readByte()
    }

    override def getPartType = ICTileDefs.BundledCable

    override def canConnectTile(part:ICTile, r:Int) = part match
    {
        case b:IBundledCableICPart => b.getBundledColour == -1 || colour == -1 || b.getBundledColour == colour
        case ins:IInsulatedRedwireICPart => true
        case be:IICBundledEmitter => true
        case _ => false
    }

    override def getBundledColour = colour

    override def isNetOutput = false

    override def isNetInput = false

    override def getTravelMask = 0xFFFF

    override def getMixerMask = 0

    override def cacheStateRegisters(linker:ISELinker){}
    override def onRegistersChanged(regIDs:Set[Int]){}

    @SideOnly(Side.CLIENT)
    override def renderDynamic(ccrs:CCRenderState, t:Transformation, ortho:Boolean, frame:Float)
    {
        RenderTileBundledCable.prepairDynamic(this)
        RenderTileBundledCable.render(ccrs, t, ortho)
    }

    @SideOnly(Side.CLIENT)
    override def getPartName = (if (colour != -1) EnumColour.values()(colour&0xFF).name+" " else "")+"Bundled cable"

    @SideOnly(Side.CLIENT)
    override def getPickOp = TileEditorOpDefs.values(TileEditorOpDefs.NeutralBundledCable.ordinal+colour+1).getOp
}