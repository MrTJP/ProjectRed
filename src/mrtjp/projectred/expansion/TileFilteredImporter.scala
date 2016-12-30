/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.colour.EnumColour
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.gui.GuiDraw
import codechicken.lib.model.blockbakery.SimpleBlockRenderer
import codechicken.lib.texture.TextureUtils
import codechicken.lib.vec.uv.{MultiIconTransformation, UVTransformation}
import mrtjp.core.gui._
import mrtjp.core.inventory.{InvWrapper, TInventory}
import mrtjp.core.item.ItemKey
import mrtjp.core.vec.{Point, Size}
import mrtjp.projectred.transportation.PressurePayload
import net.minecraft.client.renderer.texture.{TextureAtlasSprite, TextureMap}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.{Container, ISidedInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.{EnumFacing, ResourceLocation}
import net.minecraft.world.IBlockAccess
import net.minecraftforge.common.property.IExtendedBlockState
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

class TileFilteredImporter extends TileItemImporter with TInventory with ISidedInventory
{
    var colour:Byte = -1

    override def size = 9
    override def name = "filtered importer"
    override def getDisplayName = super.getDisplayName

    override def canExtractItem(slot:Int, item:ItemStack, s:EnumFacing) = (if(s == null) 6 else s.ordinal()&6) != (side&6)
    override def canInsertItem(slot:Int, item:ItemStack, s:EnumFacing) = (if(s == null) 6 else s.ordinal()&6) != (side&6)
    override def getSlotsForFace(s:EnumFacing) = if ((if(s == null) 6 else s.ordinal()&6) != (side&6)) (0 to 9).toArray else Array.empty[Int]

    override def getExtractAmount = 64

    //side = out, side^1 = in
    override def canAcceptInput(item:ItemKey, side:Int):Boolean =
    {
        if (!super.canAcceptInput(item, side)) return false
        canImport(item)
    }

    override def canImport(item:ItemKey) =
    {
        val map = InvWrapper.wrap(this).getAllItemStacks
        map.isEmpty || map.contains(item)
    }

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        saveInv(tag)
        tag.setByte("col", colour)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        loadInv(tag)
        colour = tag.getByte("col")
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

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 6 => colour = in.readByte()
        case 7 =>
            if (colour == 15) colour = -1
            else colour = (colour+1).toByte
            sendColourUpdate()
        case _ => super.read(in, key)
    }

    def sendColourUpdate()
    {
        writeStream(6).writeByte(colour).sendToChunk(this)
    }

    def clientCycleColourUp()
    {
        writeStream(7).sendToServer()
    }

    override def exportPipe(r:PressurePayload) =
    {
        r.colour = colour
        super.exportPipe(r)
    }

    override def onBlockActivated(player:EntityPlayer, actside:Int):Boolean =
    {
        if (super.onBlockActivated(player, actside)) return true

        if (!world.isRemote)
            GuiFilteredImporter.open(player, createContainer(player), _.writePos(getPos))
        true
    }

    def createContainer(player:EntityPlayer):Container =
    {
        val cont = new NodeContainer
        var s = 0
        for ((x, y) <- GuiLib.createSlotGrid(62, 18, 3, 3, 0, 0))
        {
            cont.addSlotToContainer(new Slot3(this, s, x, y))
            s += 1
        }
        cont.addPlayerInv(player, 8, 86)
        cont
    }

    override def markDirty()
    {
        super.markDirty()
    }

    override def onBlockRemoval()
    {
        super.onBlockRemoval()
        dropInvContents(world, getPos)
    }
}

class GuiFilteredImporter(c:Container, tile:TileFilteredImporter) extends NodeGui(c, 176, 168)
{
    {
        val color = new IconButtonNode
        {
            override def drawButton(mouseover:Boolean)
            {
                if (tile.colour == -1)
                {
                    TextureUtils.changeTexture(GuiLib.guiExtras)
                    GuiDraw.drawTexturedModalRect(position.x, position.y, 40, 2, 11, 11)
                }
                else GuiDraw.drawRect(position.x+2, position.y+2, 8, 8, EnumColour.fromWoolID(tile.colour).argb)//TODO Maybe from dye id.
            }

            override def onButtonClicked()
            {
                tile.clientCycleColourUp()
            }
        }
        color.position = Point(133, 37)
        color.size = Size(13, 13)
        addChild(color)
    }

    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        TextureUtils.changeTexture(GuiFilteredImporter.background)
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, 176, 168)
        GuiDraw.drawString("Filtered Importer", 8, 6, EnumColour.GRAY.argb, false)
        GuiDraw.drawString("Inventory", 8, 75, EnumColour.GRAY.argb, false)
    }
}

object GuiFilteredImporter extends TGuiBuilder
{
    val background = new ResourceLocation("projectred", "textures/gui/filtered_importer.png")
    override def getID = ExpansionProxy.filteredImporterGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val t = player.worldObj.getTileEntity(data.readPos()) match {
            case tile: TileFilteredImporter => tile
            case _ => null
        }
        if (t != null) new GuiFilteredImporter(t.createContainer(player), t)
        else null
    }
}


object RenderFilteredImporter extends SimpleBlockRenderer
{
    import java.lang.{Boolean => JBool, Integer => JInt}

    import mrtjp.core.util.CCLConversions._
    import mrtjp.projectred.expansion.BlockProperties._
    var bottom:TextureAtlasSprite = _
    var side1:TextureAtlasSprite = _
    var top1:TextureAtlasSprite = _
    var side2:TextureAtlasSprite = _
    var top2:TextureAtlasSprite = _

    var iconT1:UVTransformation = _
    var iconT2:UVTransformation = _

    override def handleState(state: IExtendedBlockState, tileEntity: TileEntity): IExtendedBlockState = tileEntity match {
        case t:TActiveDevice => {
            var s = state
            s = s.withProperty(UNLISTED_SIDE_PROPERTY, t.side.asInstanceOf[JInt])
            s = s.withProperty(UNLISTED_ROTATION_PROPERTY, t.rotation.asInstanceOf[JInt])
            s = s.withProperty(UNLISTED_ACTIVE_PROPERTY, t.active.asInstanceOf[JBool])
            s.withProperty(UNLISTED_POWERED_PROPERTY, t.powered.asInstanceOf[JBool])
        }
        case _ => state
    }

    override def getWorldTransforms(state: IExtendedBlockState) = {
        val side = state.getValue(UNLISTED_SIDE_PROPERTY)
        val rotation = state.getValue(UNLISTED_ROTATION_PROPERTY)
        val active = state.getValue(UNLISTED_ACTIVE_PROPERTY).asInstanceOf[Boolean]
        val powered = state.getValue(UNLISTED_POWERED_PROPERTY).asInstanceOf[Boolean]
        createTriple(side, rotation, if (active || powered) iconT2 else iconT1)
    }

    override def getItemTransforms(stack: ItemStack) = createTriple(0, 0, iconT1)

    override def shouldCull() = true

    def getIcon(s:Int, meta:Int) = s match
    {
        case 0 => bottom
        case 1 => top1
        case _ => side1
    }

    override def registerIcons(reg:TextureMap)
    {
        bottom = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/fimporter/bottom"))
        top1 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/fimporter/top1"))
        side1 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/fimporter/side1"))
        top2 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/fimporter/top2"))
        side2 = reg.registerSprite(new ResourceLocation("projectred:blocks/mechanical/fimporter/side2"))

        iconT1 = new MultiIconTransformation(bottom, top1, side1, side1, side1, side1)
        iconT2 = new MultiIconTransformation(bottom, top2, side2, side2, side2, side2)
    }
}
