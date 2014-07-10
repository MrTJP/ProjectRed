package mrtjp.projectred.illumination

import codechicken.multipart.minecraft.{PartMetaAccess, ButtonPart}
import mrtjp.projectred.core.TSwitchPacket
import net.minecraft.entity.projectile.EntityArrow
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.util.{IIcon, MovingObjectPosition}
import codechicken.lib.data.{MCDataOutput, MCDataInput}
import net.minecraft.nbt.NBTTagCompound
import mrtjp.projectred.ProjectRedIllumination
import scala.collection.JavaConversions._
import codechicken.multipart.{RedstoneInteractions, TileMultipart}
import codechicken.lib.vec.{BlockCoord, Vector3}
import cpw.mods.fml.relauncher.{SideOnly, Side}
import net.minecraft.client.renderer.RenderBlocks

class LightButtonPart(m:Int) extends ButtonPart(m) with ILight with TSwitchPacket
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
                sendInvUpdate()
            }
            else super.activate(player, part, item)
            true
        }
        else true
    }

    //hacked override point to remap description update to just a meta update.
    override def sendDescUpdate() {sendMetaUpdate()}

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

    def sendInvUpdate() {getWriteStreamOf(1).writeBoolean(inverted)}

    def sendMetaUpdate() {getWriteStreamOf(2).writeByte(meta)}

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 1 => inverted = packet.readBoolean()
        case 2 =>
            meta = packet.readByte()
            tile.markRender()
        case _ => super.read(packet, key)
    }

    override def getType = "pr_lightbutton"

    def getItem:Item = ProjectRedIllumination.itemPartIllumarButton
    def getItemStack = new ItemStack(getItem, 1, colorMeta)
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

class FLightButtonPart(m:Int) extends LightButtonPart(m)
{
    def this() = this(0)

    var powered = false

    override def isOn = powered != inverted

    override def onAdded()
    {
        super.onAdded()
        if (!world.isRemote) checkAndUpdatePower()
    }

    override def onNeighborChanged()
    {
        super.onNeighborChanged()
        if (world == null) return
        if (!world.isRemote) checkAndUpdatePower()
    }

    def checkAndUpdatePower()
    {
        val old = powered
        powered = isPowered
        if (old != powered) sendPowUpdate()

        def isPowered =
        {
            val side = sideForMeta(meta)
            if (0 until 6 contains side)
            {
                val pos = new BlockCoord(tile).offset(side)
                world.getBlockPowerInput(pos.x, pos.y, pos.z) != 0
            }
            else false
        }
    }

    override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeBoolean(powered)
    }

    override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        powered = packet.readBoolean()
    }

    def sendPowUpdate() {getWriteStreamOf(3).writeBoolean(powered)}

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 3 => powered = packet.readBoolean()
        case _ => super.read(packet, key)
    }

    override def getItem = ProjectRedIllumination.itemPartIllumarFButton

    override def getType = "pr_flightbutton"
}