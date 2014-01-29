package mrtjp.projectred.expansion

import codechicken.core.{IGuiPacketSender, ServerUtils}
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.packet.PacketCustom
import mrtjp.projectred.core.ItemScrewdriver
import mrtjp.projectred.core.blockutil.{BlockMulti, TileMulti}
import mrtjp.projectred.core.utils.TPortableInventory
import mrtjp.projectred.{ProjectRedExpansion, ExpansionSPH}
import net.minecraft.block.material.Material
import net.minecraft.entity.player.{EntityPlayerMP, EntityPlayer}
import net.minecraft.inventory.{Container, ISidedInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.world.World
import net.minecraftforge.common.ForgeDirection

class BlockMachine(id:Int) extends BlockMulti(id, Material.rock)
{
    setHardness(2)
    setCreativeTab(ProjectRedExpansion.tabExpansion)

    override def isOpaqueCube:Boolean = true

    override def renderAsNormalBlock:Boolean = true

    override def isBlockSolidOnSide(world:World, x:Int, y:Int, z:Int, side:ForgeDirection):Boolean = true
}

abstract class TileMachine extends TileMulti
{
    protected var rotation:Byte = 0

    override def onBlockPlaced(stack:ItemStack, side:Int, player:EntityPlayer)
    {
        rotation = resolveLook_6(player).asInstanceOf[Byte]
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeByte(rotation)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        rotation = in.readByte
    }

    override def save(tag:NBTTagCompound)
    {
        tag.setByte("rot", rotation)
    }

    override def load(tag:NBTTagCompound)
    {
        rotation = tag.getByte("rot")
    }

    override def read(in:MCDataInput, switchkey:Int) = switchkey match
    {
        case 1 => rotation = in.readByte()
        case _ => super.read(in, switchkey)
    }

    override def onBlockActivated(player:EntityPlayer, side:Int):Boolean =
    {
        val held:ItemStack = player.getHeldItem
        if (held != null && held.getItem.isInstanceOf[ItemScrewdriver])
        {
            if (worldObj.isRemote) return true
            val old = rotation
            do rotation = ((rotation + 1) % 6).asInstanceOf[Byte] while (!isRotationAllowed(rotation) && rotation != old)
            if (rotation != old) sendRotationUpdate()
            return true
        }
        return false
    }

    protected def isRotationAllowed(rot:Int):Boolean = rot!=0||rot!=1

    protected def sendRotationUpdate()
    {
        writeStreamSend(writeStream(1).writeByte(rotation))
    }

    def resolveLook_6(player:EntityPlayer):Int =
    {
        val rot = Math.floor(player.rotationYaw * 4.0F / 360.0F + 0.5D).asInstanceOf[Int] & 0x3
        if (Math.abs(player.posX-xCoord)<2D && Math.abs(player.posZ-zCoord)<2D)
        {
            val p = player.posY+1.82D-player.yOffset-yCoord
            if (p > 2.0D) return 0
            if (p < 0.0D) return 1
        }
        rot match
        {
            case 0 => return 3
            case 1 => return 4
            case 2 => return 2
            case 3 => return 5
        }
        1
    }

    def resolveLook_4(player:EntityPlayer) = Math.floor(player.rotationYawHead*4.0F/360.0F+0.5D).asInstanceOf[Int]&0x3
}

abstract class TileMachineIO extends TileMachine with TPortableInventory with ISidedInventory
{
    override def onBlockActivated(player:EntityPlayer, side:Int):Boolean =
        super.onBlockActivated(player, side)||openGui(player)

    def openGui(player:EntityPlayer):Boolean =
    {
        if (!worldObj.isRemote)
            ServerUtils.openSMPContainer(player.asInstanceOf[EntityPlayerMP], createContainer(player), new IGuiPacketSender
            {
                def sendPacket(player:EntityPlayerMP, windowId:Int)
                {
                    val p = new PacketCustom(ExpansionSPH.channel, ExpansionSPH.machine_gui_open)
                    p.writeCoord(xCoord, yCoord, zCoord)
                    p.writeByte(windowId).writeByte(getGuiID)
                    p.sendToPlayer(player)
                }
            })

        true
    }

    def createContainer(player:EntityPlayer):Container

    def getGuiID:Int

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        inv.save(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        inv.load(tag)
    }

    override def onInventoryChanged() = super.onInventoryChanged()
}