package mrtjp.projectred.transportation

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.BlockCoord
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


trait TRedstonePipe extends SubcorePipePart with TCenterRSAcquisitions with TPropagationAcquisitions with IRedwirePart with IMaskedRedstonePart
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
        ItemMicroPart.create(769, Blocks.redstone_block.getUnlocalizedName)

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

    def getRedwireSignal = signal&0xFF

    override def getRedwireSignal(side:Int) = getRedwireSignal

    override def updateAndPropagate(prev:TMultiPart, mode:Int)
    {
        if (mode == DROPPING && signal == 0) return
        val newSignal = calculateSignal
        if (newSignal < getRedwireSignal)
        {
            if (newSignal > 0) WirePropagator.propagateAnalogDrop(this)
            signal = 0
            propagate(prev, DROPPING)
        }
        else if (newSignal > getRedwireSignal)
        {
            signal = newSignal.asInstanceOf[Byte]
            if (mode == DROPPING) propagate(null, RISING)
            else propagate(prev, RISING)
        }
        else if (mode == DROPPING) propagateTo(prev, RISING)
        else if (mode == FORCE) propagate(prev, FORCED)
    }

    def propagate(from:TMultiPart, mode:Int)
    {
        if (mode != FORCED) WirePropagator.addPartChange(this)
        for (s <- 0 until 6) if (maskConnectsOut(s))
            propagateExternal(getStraight(s), posOfStraight(s), from, mode)

        propagateOther(mode)
    }

    def propagateOther(mode:Int)
    {
        for (s <- 0 until 6) if (!maskConnects(s))
            WirePropagator.addNeighborChange(new BlockCoord(tile).offset(s))
    }

    def calculateSignal:Int =
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
        def dropMaterial()
        {
            if (material && !player.capabilities.isCreativeMode)
                PRLib.dropTowardsPlayer(world, x, y, z, getMaterialStack, player)
        }

        //if (CommandDebug.WIRE_READING) debug(player) else
        if (held != null && held.getItem == ProjectRedCore.itemWireDebugger)
        {
            held.damageItem(1, player)
            player.swingItem()
            test(player)
        }
        else if (held == null)
        {
            if (!world.isRemote && player.isSneaking && material)
            {
                dropMaterial()
                material = false
                sendMatUpdate()
            }
            false
        }
        else if (!material && held.getItem == MicroblockProxy.itemMicro && held.getItemDamage == 769)
        {
            ItemMicroPart.getMaterial(held) match
            {
                case bm:BlockMicroMaterial if bm.block == Blocks.redstone_block =>
                    if (!world.isRemote)
                    {
                        material = true
                        world.playSoundEffect(x+0.5, y+0.5, z+0.5, Block.soundTypeGlass.func_150496_b(),
                            Block.soundTypeGlass.getVolume*5.0F, Block.soundTypeGlass.getPitch*.9F)
                        sendMatUpdate()
                        if (!player.capabilities.isCreativeMode) held.stackSize-=1
                    }
                    true
                case _ => false
            }
        }
        else false
    }

    def debug(player:EntityPlayer) =
    {
        player.addChatComponentMessage(new ChatComponentText(
            (if (world.isRemote) "Client" else "Server")+" signal strength: "+getRedwireSignal))
        true
    }

    def test(player:EntityPlayer) =
    {
        if (world.isRemote) Messenger.addMessage(x, y+.5f, z, "/#f/#c[c] = "+getRedwireSignal)
        else
        {
            val packet = Messenger.createPacket
            packet.writeDouble(x+0.0D)
            packet.writeDouble(y+0.5D)
            packet.writeDouble(z+0.0D)
            packet.writeString("/#c[s] = "+getRedwireSignal)
            packet.sendToPlayer(player)
        }
        true
    }
}

trait IInventoryProvider
{
    def getInventory:IInventory
    def getInterfacedSide:Int
}

trait TInventoryPipe extends PayloadPipePart with IInventoryProvider
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
        val invalid = force || !maskConnects(inOutSide) ||
            WorldLib.getTileEntity(world, new BlockCoord(tile).offset(inOutSide), classOf[IInventory]) == null
        if (!invalid) return
        var found = false
        val oldSide = inOutSide

        import scala.util.control.Breaks._
        breakable {
            for (i <- 0 until 6)
            {
                inOutSide = ((inOutSide+1)%6).asInstanceOf[Byte]
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

        if (!found) inOutSide = -1
        if (oldSide != inOutSide) sendOrientUpdate()
    }

    override def getInventory =
    {
        if (0 until 6 contains inOutSide) InvWrapper.getInventory(world, new BlockCoord(tile).offset(inOutSide))
        else null
    }

    override def getInterfacedSide = if (inOutSide < 0 || inOutSide > 5) -1 else inOutSide^1

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

trait TSimpleLogicPipe extends PayloadPipePart
{
    var logic:PipeLogic = null

    override def preparePlacement(side:Int, meta:Int)
    {
        super.preparePlacement(side, meta)
        logic = PipeLogic.createPipeLogic(this, meta)
    }

    def getLogic = logic

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        getLogic.save(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        logic = PipeLogic.createPipeLogic(this, meta)
        getLogic.load(tag)
    }

    override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        getLogic.writeDesc(packet)
    }

    override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        if (getLogic == null) logic = PipeLogic.createPipeLogic(this, meta)
        getLogic.readDesc(packet)
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case k if k >= 50 => logic.read(packet, k)
        case _ => super.read(packet, key)
    }

    override def update()
    {
        super.update()
        getLogic.tick()
    }

    override def handleDrop(r:PipePayload)
    {
        if (getLogic.handleDrop(r)) return
        super.handleDrop(r)
    }

    override def resolveDestination(r:PipePayload)
    {
        if (getLogic.resolveDestination(r)) return
        super.handleDrop(r)
    }

    override def endReached(r:PipePayload)
    {
        if (getLogic.endReached(r)) return
        super.endReached(r)
    }

    override def centerReached(r:PipePayload)
    {
        if (getLogic.centerReached(r)) return
        super.centerReached(r)
    }

    @SideOnly(Side.CLIENT)
    override def getIcon(side:Int) =
    {
        val i = getLogic.getIcon(side)
        if (i != null) i
        else super.getIcon(side)
    }
}

object PipeLogic
{
    def createPipeLogic(p:PayloadPipePart, meta:Int) = meta match
    {
        case 0 => new NullPipeLogic(p)
        case _ => new NullPipeLogic(p)
    }

    def apply(p:PayloadPipePart, meta:Int) = createPipeLogic(p, meta)

    class NullPipeLogic(p:PayloadPipePart) extends PipeLogic(p)
    {
        def endReached(r:PipePayload) = false
        def centerReached(r:PipePayload) = false
        def handleDrop(r:PipePayload) = false
        def resolveDestination(r:PipePayload) = false
        def getIcon(i:Int) = null
    }
}

abstract class PipeLogic(p:PayloadPipePart)
{
    def save(tag:NBTTagCompound){}

    def load(tag:NBTTagCompound){}

    def readDesc(packet:MCDataInput){}

    def writeDesc(packet:MCDataOutput){}

    //key allocated >= 50
    def read(packet:MCDataInput, key:Int){}

    def tick(){}

    def endReached(r:PipePayload):Boolean

    def centerReached(r:PipePayload):Boolean

    def handleDrop(r:PipePayload):Boolean

    def resolveDestination(r:PipePayload):Boolean

    def getIcon(i:Int):IIcon
}

abstract class BasicPipeAbstraction extends PayloadPipePart with TRedstonePipe