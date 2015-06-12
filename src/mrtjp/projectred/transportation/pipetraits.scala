package mrtjp.projectred.transportation

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.{Vector3, BlockCoord}
import codechicken.microblock.handler.MicroblockProxy
import codechicken.microblock.{BlockMicroMaterial, ItemMicroPart}
import codechicken.multipart.{IMaskedRedstonePart, RedstoneInteractions, TMultiPart}
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.inventory.InvWrapper
import mrtjp.core.world.{Messenger, WorldLib}
import mrtjp.projectred.ProjectRedCore
import mrtjp.projectred.api.{IConnectable, IScrewdriver}
import mrtjp.projectred.core.libmc.PRLib
import mrtjp.projectred.transmission.IWirePart._
import mrtjp.projectred.transmission._
import net.minecraft.block.Block
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.init.Blocks
import net.minecraft.inventory.{IInventory, ISidedInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.{ChatComponentText, IIcon, MovingObjectPosition}

import scala.collection.JavaConversions._


trait TRedstonePipe extends SubcorePipePart with TCenterRSAcquisitions with TCenterRSPropagation with IRedwirePart with IMaskedRedstonePart
{
    var signal:Byte = 0
    var material = false

    abstract override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setBoolean("mat", material)
        tag.setByte("signal", signal)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        material = tag.getBoolean("mat")
        signal = tag.getByte("signal")
    }

    abstract override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeBoolean(material)
        packet.writeByte(signal)
    }

    abstract override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        material = packet.readBoolean()
        signal = packet.readByte()
    }

    abstract override def read(packet:MCDataInput, key:Int) = key match
    {
        case 2 =>
            material = packet.readBoolean()
            tile.markRender()
        case 3 =>
            signal = packet.readByte
            tile.markRender()
        case _ => super.read(packet, key)
    }

    def sendMatUpdate()
    {
        if (!world.isRemote)
        {
            if (updateInward()) sendConnUpdate()
            WirePropagator.propagateTo(this, FORCE)
        }
        getWriteStreamOf(2).writeBoolean(material)
    }

    override def onSignalUpdate()
    {
        tile.markDirty()
        getWriteStreamOf(3).writeByte(signal)
    }

    override def onPartChanged(part:TMultiPart)
    {
        if (!world.isRemote)
        {
            WirePropagator.logCalculation()

            if (updateOutward())
            {
                sendConnUpdate()
                WirePropagator.propagateTo(this, FORCE)
            }
            else WirePropagator.propagateTo(this, RISING)
        }
    }

    override def onNeighborChanged()
    {
        if (!world.isRemote)
        {
            WirePropagator.logCalculation()
            if (updateExternalConns())
            {
                sendConnUpdate()
                WirePropagator.propagateTo(this, FORCE)
            }
            else WirePropagator.propagateTo(this, RISING)
        }
    }

    override def onAdded()
    {
        super.onAdded()
        if (!world.isRemote)
        {
            if (updateInward()) sendConnUpdate()
            WirePropagator.propagateTo(this, RISING)
        }
    }

    override def getDrops = if (material)
        super.getDrops :+ getMaterialStack else super.getDrops

    def getMaterialStack =
        ItemMicroPart.create(769, BlockMicroMaterial.materialKey(Blocks.redstone_block, 0))

    override def diminishOnSide(side:Int) = true

    override def strongPowerLevel(side:Int) = 0

    override def weakPowerLevel(side:Int) =
    {
        if (!maskConnects(side) || !material) 0
        else rsLevel
    }

    override def canConnectRedstone(side:Int) = material

    override def getConnectionMask(side:Int) = 0x10

    abstract override def canConnectPart(part:IConnectable, s:Int) = part match
    {
        case fr:FramedRedwirePart if material => true
        case _ => super.canConnectPart(part, s)
    }

    override def discoverStraightOverride(absDir:Int) =
    {
        if (material)
        {
            WirePropagator.setRedwiresConnectable(false)
            val b = (RedstoneInteractions.otherConnectionMask(world, x, y, z, absDir, false)&
                RedstoneInteractions.connectionMask(this, absDir)) != 0
            WirePropagator.setRedwiresConnectable(true)
            b
        }
        else false
    }

    def rsLevel =
    {
        if (WirePropagator.redwiresProvidePower) ((signal&0xFF)+16)/17
        else 0
    }

    override def getRedwireSignal(side:Int) = getSignal

    override def getSignal = signal&0xFF
    override def setSignal(sig:Int){ signal = sig.toByte }

    override def propagateOther(mode:Int)
    {
        for (s <- 0 until 6) if (!maskConnects(s))
            WirePropagator.addNeighborChange(new BlockCoord(tile).offset(s))
    }

    override def calculateSignal:Int =
    {
        if (!material) return 0
        WirePropagator.setDustProvidePower(false)
        WirePropagator.redwiresProvidePower = false
        var s = 0
        def raise(sig:Int) {if (sig > s) s = sig}

        for (s <- 0 until 6) if (maskConnectsOut(s))
            raise(calcStraightSignal(s))

        WirePropagator.setDustProvidePower(true)
        WirePropagator.redwiresProvidePower = true
        s
    }

    override def calcStraightSignal(s:Int) = getStraight(s) match
    {
        case p:TMultiPart => resolveSignal(p, s^1)
        case _ => calcStrongSignal(s)
    }

    override def resolveSignal(part:Any, s:Int) = part match
    {
        case rw:IRedwirePart if rw.diminishOnSide(s) => rw.getRedwireSignal(s)-1
        case re:IRedwireEmitter => re.getRedwireSignal(s)
        case _ => 0
    }

    abstract override def activate(player:EntityPlayer, hit:MovingObjectPosition, held:ItemStack):Boolean =
    {
        if (super.activate(player, hit, held)) return true

        //if (CommandDebug.WIRE_READING) debug(player) else
        if (held != null && held.getItem == ProjectRedCore.itemWireDebugger)
        {
            held.damageItem(1, player)
            test(player)
            return true
        }

        if (held == null && player.isSneaking && material)
        {
            if (!world.isRemote)
            {
                if (material && !player.capabilities.isCreativeMode)
                    PRLib.dropTowardsPlayer(world, x, y, z, getMaterialStack, player)
                material = false
                sendMatUpdate()
            }
            return true
        }

        if (held != null && !material && held.getItem == MicroblockProxy.itemMicro && held.getItemDamage == 769)
        {
            ItemMicroPart.getMaterial(held) match
            {
                case bm:BlockMicroMaterial if bm.block == Blocks.redstone_block =>
                    if (!world.isRemote)
                    {
                        material = true
                        world.playSoundEffect(x+0.5, y+0.5, z+0.5, Block.soundTypeGlass.func_150496_b(),
                            Block.soundTypeGlass.getVolume*5.0F, Block.soundTypeGlass.getPitch*0.9F)
                        sendMatUpdate()
                        if (!player.capabilities.isCreativeMode) held.stackSize-=1
                    }
                    return true
                case _ =>
            }
        }

        false
    }

    def debug(player:EntityPlayer) =
    {
        player.addChatComponentMessage(new ChatComponentText(
            (if (world.isRemote) "Client" else "Server")+" signal strength: "+getSignal))
        true
    }

    def test(player:EntityPlayer) =
    {
        if (world.isRemote) Messenger.addMessage(x, y+.5f, z, "/#f/#c[c] = "+getSignal)
        else
        {
            val packet = Messenger.createPacket
            packet.writeDouble(x+0.0D)
            packet.writeDouble(y+0.5D)
            packet.writeDouble(z+0.0D)
            packet.writeString("/#c[s] = "+getSignal)
            packet.sendToPlayer(player)
        }
        true
    }

    @SideOnly(Side.CLIENT)
    override def doStaticTessellation(pos:Vector3)
    {
        super.doStaticTessellation(pos)
        if (material) RenderPipe.renderRSWiring(this, pos, signal)
    }
}

trait TColourFilterPipe extends SubcorePipePart
{
    var colour:Byte = -1

    abstract override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setByte("colour", colour)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        colour = if (tag.hasKey("colour")) tag.getByte("colour") else -1
    }

    abstract override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeByte(colour)
    }

    abstract override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        colour = packet.readByte()
    }

    abstract override def read(packet:MCDataInput, key:Int) = key match
    {
        case 12 =>
            colour = packet.readByte()
            tile.markRender()
        case _ => super.read(packet, key)
    }

    def sendColourUpdate()
    {
        getWriteStreamOf(12).writeByte(colour)
    }

    abstract override def getDrops =
        if (colour > -1) super.getDrops:+getColourStack
        else super.getDrops

    def getColourStack =
        if (colour == -1) null
        else ItemMicroPart.create(769, BlockMicroMaterial.materialKey(Blocks.wool, colour))

    abstract override def activate(player:EntityPlayer, hit:MovingObjectPosition, held:ItemStack):Boolean =
    {
        if (super.activate(player, hit, held)) return true

        def dropMaterial()
        {
            if (colour > -1 && !player.capabilities.isCreativeMode)
                PRLib.dropTowardsPlayer(world, x, y, z, getColourStack, player)
        }

        if (held == null && player.isSneaking && colour > -1)
        {
            if (!world.isRemote)
            {
                dropMaterial()
                colour = -1
                sendColourUpdate()
            }
            return true
        }

        if (held != null && held.getItem == MicroblockProxy.itemMicro && held.getItemDamage == 769)
        {
            ItemMicroPart.getMaterial(held) match
            {
                case bm:BlockMicroMaterial if bm.block == Blocks.wool && bm.meta != colour =>
                    if (!world.isRemote)
                    {
                        dropMaterial()
                        colour = bm.meta.toByte
                        world.playSoundEffect(x+0.5, y+0.5, z+0.5, bm.getSound.func_150496_b(),
                            bm.getSound.getVolume*5.0F, bm.getSound.getPitch*0.9F)
                        sendColourUpdate()
                        if (!player.capabilities.isCreativeMode) held.stackSize-=1
                    }
                    return true
                case _ =>
            }
        }

        false
    }

    @SideOnly(Side.CLIENT)
    override def doStaticTessellation(pos:Vector3)
    {
        super.doStaticTessellation(pos)
        if (colour > -1) RenderPipe.renderColourWool(this, pos, colour)
    }
}

trait IInventoryProvider
{
    def getInventory:IInventory
    def getInterfacedSide:Int
}

trait TInventoryPipe[T <: AbstractPipePayload] extends PayloadPipePart[T] with IInventoryProvider
{
    var inOutSide:Byte = 0

    abstract override def read(packet:MCDataInput, key:Int) = key match
    {
        case 6 =>
            inOutSide = packet.readByte
            tile.markRender()
        case _ => super.read(packet, key)
    }

    def sendOrientUpdate()
    {
        getWriteStreamOf(6).writeByte(inOutSide)
    }

    abstract override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setByte("io", inOutSide)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        inOutSide = tag.getByte("io")
    }

    abstract override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeByte(inOutSide)
    }

    abstract override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        inOutSide = packet.readByte
    }

    abstract override def onNeighborChanged()
    {
        super.onNeighborChanged()
        shiftOrientation(false)
    }

    abstract override def onPartChanged(p:TMultiPart)
    {
        super.onPartChanged(p)
        shiftOrientation(false)
    }

    abstract override def onAdded()
    {
        super.onAdded()
        shiftOrientation(false)
    }

    abstract override def discoverStraightOverride(s:Int):Boolean =
    {
        WorldLib.getTileEntity(world, posOfStraight(s)) match
        {
            case sinv:ISidedInventory => sinv.getAccessibleSlotsFromSide(s^1).nonEmpty
            case inv:IInventory => true
            case _ => false
        }
    }

    def shiftOrientation(force:Boolean)
    {
        if (world.isRemote) return
        val invalid = force || inOutSide == 6 || !maskConnects(inOutSide) ||
            WorldLib.getTileEntity(world, new BlockCoord(tile).offset(inOutSide), classOf[IInventory]) == null
        if (!invalid) return
        var found = false
        val oldSide = inOutSide

        import scala.util.control.Breaks._
        breakable {
            for (i <- 0 until 6)
            {
                inOutSide = ((inOutSide+1)%6).toByte
                if (maskConnects(inOutSide))
                {
                    val bc = new BlockCoord(tile).offset(inOutSide)
                    val t = WorldLib.getTileEntity(world, bc)
                    if (t.isInstanceOf[IInventory])
                    {
                        found = true
                        break()
                    }
                }
            }
        }

        if (!found) inOutSide = 6
        if (oldSide != inOutSide) sendOrientUpdate()
    }

    override def getInventory =
    {
        if (0 until 6 contains inOutSide) InvWrapper.getInventory(world, new BlockCoord(tile).offset(inOutSide))
        else null
    }

    override def getInterfacedSide = if (!(0 to 5 contains inOutSide)) -1 else inOutSide^1

    abstract override def activate(player:EntityPlayer, hit:MovingObjectPosition, item:ItemStack):Boolean =
    {
        if (super.activate(player, hit, item)) return true

        if (item != null && item.getItem.isInstanceOf[IScrewdriver])
        {
            if (!world.isRemote)
            {
                shiftOrientation(true)
                item.getItem.asInstanceOf[IScrewdriver].damageScrewdriver(world, player)
            }
            return true
        }

        false
    }
}