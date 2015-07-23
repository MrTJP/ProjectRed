/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.expansion

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.gui.GuiDraw
import codechicken.lib.render.uv.{MultiIconTransformation, UVTransformation}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.block.TInstancedBlockRender
import mrtjp.core.color.Colors
import mrtjp.core.gui._
import mrtjp.core.inventory.{InvWrapper, TInventory}
import mrtjp.core.item.ItemKey
import mrtjp.core.render.TCubeMapRender
import mrtjp.core.resource.ResourceLib
import mrtjp.core.vec.{Point, Size}
import mrtjp.core.world.WorldLib
import mrtjp.projectred.core.libmc.PRResources
import mrtjp.projectred.transportation.PressurePayload
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.{Container, ISidedInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.IIcon
import net.minecraft.world.IBlockAccess

class TileFilteredImporter extends TileItemImporter with TInventory with ISidedInventory
{
    var colour:Byte = -1

    override def size = 9
    override def name = "filtered importer"

    override def canExtractItem(slot:Int, item:ItemStack, s:Int) = (s&6) != (side&6)
    override def canInsertItem(slot:Int, item:ItemStack, s:Int) = (s&6) != (side&6)
    override def getAccessibleSlotsFromSide(s:Int) = if ((s&6) != (side&6)) (0 to 9).toArray else Array.empty[Int]

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
        writeStream(6).writeByte(colour).sendToChunk()
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
            GuiFilteredImporter.open(player, createContainer(player), _.writeCoord(x, y, z))
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
        dropInvContents(world, x, y, z)
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
                    ResourceLib.guiExtras.bind()
                    GuiDraw.drawTexturedModalRect(position.x, position.y, 40, 2, 11, 11)
                }
                else GuiDraw.drawRect(position.x+2, position.y+2, 8, 8, Colors(tile.colour).argb)
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
        PRResources.guiFilteredImporter.bind()
        GuiDraw.drawTexturedModalRect(0, 0, 0, 0, 176, 168)
        GuiDraw.drawString("Filtered Importer", 8, 6, Colors.GREY.argb, false)
        GuiDraw.drawString("Inventory", 8, 75, Colors.GREY.argb, false)
    }
}

object GuiFilteredImporter extends TGuiBuilder
{
    override def getID = ExpansionProxy.filteredImporterGui

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val t = WorldLib.getTileEntity(player.worldObj, data.readCoord(), classOf[TileFilteredImporter])
        if (t != null) new GuiFilteredImporter(t.createContainer(player), t)
        else null
    }
}


object RenderFilteredImporter extends TInstancedBlockRender with TCubeMapRender
{
    var bottom:IIcon = _
    var side1:IIcon = _
    var top1:IIcon = _
    var side2:IIcon = _
    var top2:IIcon = _

    var iconT1:UVTransformation = _
    var iconT2:UVTransformation = _

    override def getData(w:IBlockAccess, x:Int, y:Int, z:Int) =
    {
        val te = WorldLib.getTileEntity(w, x, y, z, classOf[TActiveDevice])
        if (te != null) (te.side, te.rotation, if (te.active || te.powered) iconT2 else iconT1)
        else (0, 0, iconT1)
    }

    override def getInvData = (0, 0, iconT1)

    override def getIcon(s:Int, meta:Int) = s match
    {
        case 0 => bottom
        case 1 => top1
        case _ => side1
    }

    override def registerIcons(reg:IIconRegister)
    {
        bottom = reg.registerIcon("projectred:mechanical/fimporter/bottom")
        top1 = reg.registerIcon("projectred:mechanical/fimporter/top1")
        side1 = reg.registerIcon("projectred:mechanical/fimporter/side1")
        top2 = reg.registerIcon("projectred:mechanical/fimporter/top2")
        side2 = reg.registerIcon("projectred:mechanical/fimporter/side2")

        iconT1 = new MultiIconTransformation(bottom, top1, side1, side1, side1, side1)
        iconT2 = new MultiIconTransformation(bottom, top2, side2, side2, side2, side2)
    }
}
