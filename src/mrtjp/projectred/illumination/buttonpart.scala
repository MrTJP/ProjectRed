package mrtjp.projectred.illumination

import codechicken.multipart.minecraft.{PartMetaAccess, ButtonPart}
import net.minecraft.item.ItemStack
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.util.{IIcon, MovingObjectPosition}
import codechicken.lib.data.{MCDataOutput, MCDataInput}
import net.minecraft.nbt.NBTTagCompound
import mrtjp.projectred.ProjectRedIllumination
import scala.collection.JavaConversions._
import codechicken.multipart.TileMultipart
import codechicken.lib.vec.Vector3
import cpw.mods.fml.relauncher.{SideOnly, Side}
import net.minecraft.client.renderer.RenderBlocks

class LightButtonPart(meta:Int) extends ButtonPart(meta) with ILight
{
    def this() = this(0)

    var colorMeta:Byte = 0
    var inverted = false

    def onPlaced(stack:ItemStack)
    {
        colorMeta = stack.getItemDamage.asInstanceOf[Byte]
    }

    override def activate(player:EntityPlayer, part:MovingObjectPosition, item:ItemStack) =
    {
        if (pressed) false
        else if (!world.isRemote)
        {
            if (player.isSneaking)
            {
                inverted = !inverted
                sendDescUpdate()
                true
            }
            else super.activate(player, part, item)
        }
        else true
    }

    override def isOn = pressed != inverted

    override def getColor = colorMeta

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setByte("colorMeta", colorMeta)
        tag.setBoolean("inv", inverted)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        colorMeta = tag.getByte("colorMeta")
        inverted = tag.getBoolean("inv")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeByte(colorMeta)
        packet.writeBoolean(inverted)
    }

    override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        colorMeta = packet.readByte
        inverted = packet.readBoolean
    }

    override def getType = "pr_lightbutton"

    def getItemStack = new ItemStack(ProjectRedIllumination.itemPartIllumarButton, 1, colorMeta)
    override def getDrops = Seq(getItemStack)
    override def pickItem(hit:MovingObjectPosition) = getItemStack

    override def drop()
    {
        TileMultipart.dropItem(getItemStack, world, Vector3.fromTileEntityCenter(tile))
        tile.remPart(this)
    }

    @SideOnly(Side.CLIENT)
    override def renderStatic(pos:Vector3, pass:Int) =
    {
        if (pass == 0)
        {
            val r = new RenderBlocks(new PartMetaAccess(this))
            r.renderBlockUsingTexture(getBlock, x, y, z, ItemPartButton.icons(colorMeta))
            true
        }
        else false
    }

    @SideOnly(Side.CLIENT)
    override def renderDynamic(pos:Vector3, frame:Float, pass:Int)
    {
        if (pass == 0 && isOn)
        {
            val box = getBounds.expand(0.025D)
            RenderHalo.addLight(x, y, z, colorMeta, box)
        }
    }

    @SideOnly(Side.CLIENT)
    override def getBrokenIcon(side:Int):IIcon = ItemPartButton.icons(colorMeta)

    @SideOnly(Side.CLIENT)
    override def getBreakingIcon(subPart:scala.Any, side:Int) = getBrokenIcon(side)
}
