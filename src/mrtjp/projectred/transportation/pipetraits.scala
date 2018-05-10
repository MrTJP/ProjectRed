package mrtjp.projectred.transportation

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.raytracer.CuboidRayTraceResult
import codechicken.lib.render.CCRenderState
import codechicken.lib.vec.Vector3
import codechicken.microblock.handler.MicroblockProxy
import codechicken.microblock.{BlockMicroMaterial, ItemMicroPart}
import codechicken.multipart.{IMaskedRedstonePart, IRedstonePart, RedstoneInteractions, TMultiPart}
import mrtjp.core.inventory.InvWrapper
import mrtjp.core.world.Messenger
import mrtjp.projectred.ProjectRedCore
import mrtjp.projectred.api.{IConnectable, IScrewdriver}
import mrtjp.projectred.core.IWirePart._
import mrtjp.projectred.core._
import net.minecraft.block.SoundType
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.init.Blocks
import net.minecraft.inventory.{IInventory, ISidedInventory}
import net.minecraft.item.ItemStack
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util._
import net.minecraft.util.text.TextComponentString
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

import scala.collection.JavaConversions._

trait TRedstonePipe extends SubcorePipePart with TCenterRSAcquisitions with TCenterRSPropagation with IRedwirePart with IMaskedRedstonePart
{
    var signal:Byte = 0
    var hasRedstone = false

    abstract override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setBoolean("mat", hasRedstone)
        tag.setByte("signal", signal)
    }

    abstract override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        hasRedstone = tag.getBoolean("mat")
        signal = tag.getByte("signal")
    }

    abstract override def writeDesc(packet:MCDataOutput)
    {
        super.writeDesc(packet)
        packet.writeBoolean(hasRedstone)
        packet.writeByte(signal)
    }

    abstract override def readDesc(packet:MCDataInput)
    {
        super.readDesc(packet)
        hasRedstone = packet.readBoolean()
        signal = packet.readByte()
    }

    abstract override def read(packet:MCDataInput, key:Int) = key match
    {
        case 2 =>
            hasRedstone = packet.readBoolean()
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
            if (updateInward()) onMaskChanged()
            WirePropagator.propagateTo(this, FORCE)
        }
        getWriteStreamOf(2).writeBoolean(hasRedstone)
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
                onMaskChanged()
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
                onMaskChanged()
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
            if (updateInward()) onMaskChanged()
            WirePropagator.propagateTo(this, RISING)
        }
    }

    override def getDrops = if (hasRedstone)
        super.getDrops :+ getMaterialStack else super.getDrops

    def getMaterialStack =
        ItemMicroPart.create(769, BlockMicroMaterial.materialKey(Blocks.REDSTONE_BLOCK.getDefaultState))

    override def diminishOnSide(side:Int) = true

    override def strongPowerLevel(side:Int) = 0

    override def weakPowerLevel(side:Int) =
    {
        if (!maskConnects(side) || !hasRedstone) 0
        else rsLevel
    }

    override def canConnectRedstone(side:Int) = hasRedstone

    override def getConnectionMask(side:Int) = 0x10

    abstract override def canConnectPart(part:IConnectable, s:Int) = part match
    {
        case rw:IRedwirePart with IMaskedRedstonePart
            if hasRedstone && (rw.getConnectionMask(s^1)&0x10) != 0 => true
        case _ => super.canConnectPart(part, s)
    }

    override def discoverStraightOverride(absDir:Int) =
    {
        if (hasRedstone)
        {
            WirePropagator.setRedwiresConnectable(false)
            val b = (RedstoneInteractions.otherConnectionMask(world, pos, absDir, false)&
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
            WirePropagator.addNeighborChange(posOfStraight(s))
    }

    override def calculateSignal:Int =
    {
        if (!hasRedstone) return 0
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

    abstract override def activate(player:EntityPlayer, hit:CuboidRayTraceResult, item:ItemStack, hand:EnumHand):Boolean =
    {
        if (super.activate(player, hit, item, hand)) return true

        //if (CommandDebug.WIRE_READING) debug(player) else
        if (!item.isEmpty && item.getItem == ProjectRedCore.itemMultimeter)
        {
            item.damageItem(1, player)
            test(player)
            return true
        }

        if (item.isEmpty && player.isSneaking && hasRedstone)
        {
            if (!world.isRemote)
            {
                if (hasRedstone && !player.capabilities.isCreativeMode)
                    PRLib.dropTowardsPlayer(world, pos, getMaterialStack, player)
                hasRedstone = false
                sendMatUpdate()
            }
            return true
        }

        if (!item.isEmpty && !hasRedstone && item.getItem == MicroblockProxy.itemMicro && item.getItemDamage == 769)
        {
            ItemMicroPart.getMaterial(item) match
            {
                case bm:BlockMicroMaterial if bm.state.getBlock == Blocks.REDSTONE_BLOCK =>
                    if (!world.isRemote)
                    {
                        hasRedstone = true
                        world.playSound(null, pos, SoundType.GLASS.getPlaceSound, SoundCategory.BLOCKS, SoundType.GLASS.getVolume, SoundType.GLASS.getPitch)
                        sendMatUpdate()
                        if (!player.capabilities.isCreativeMode) item.shrink(1)
                    }
                    return true
                case _ =>
            }
        }

        false
    }

    def debug(player:EntityPlayer) =
    {
        player.sendMessage(new TextComponentString(
            (if (world.isRemote) "Client" else "Server")+" signal strength: "+getSignal))
        true
    }

    def test(player:EntityPlayer) =
    {
        if (world.isRemote) Messenger.addMessage(pos.getX, pos.getY+.5f, pos.getZ, "/#f/#c[c] = "+getSignal)
        else
        {
            val packet = Messenger.createPacket
            packet.writeDouble(pos.getX+0.0D)
            packet.writeDouble(pos.getY+0.5D)
            packet.writeDouble(pos.getZ+0.0D)
            packet.writeString("/#c[s] = "+getSignal)
            packet.sendToPlayer(player)
        }
        true
    }

    @SideOnly(Side.CLIENT)
    override def doStaticTessellation(pos:Vector3, ccrs:CCRenderState)
    {
        super.doStaticTessellation(pos, ccrs)
        if (hasRedstone) RenderPipe.renderRSWiring(this, pos, signal, ccrs)
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
        if (colour == -1) ItemStack.EMPTY
        else ItemMicroPart.create(769, BlockMicroMaterial.materialKey(Blocks.WOOL.getStateFromMeta(colour)))

    abstract override def activate(player:EntityPlayer, hit:CuboidRayTraceResult, item:ItemStack, hand:EnumHand):Boolean =
    {
        if (super.activate(player, hit, item, hand)) return true

        def dropMaterial()
        {
            if (colour > -1 && !player.capabilities.isCreativeMode)
                PRLib.dropTowardsPlayer(world, pos, getColourStack, player)
        }

        if (item.isEmpty && player.isSneaking && colour > -1)
        {
            if (!world.isRemote)
            {
                dropMaterial()
                colour = -1
                sendColourUpdate()
            }
            return true
        }

        if (!item.isEmpty && item.getItem == MicroblockProxy.itemMicro && item.getItemDamage == 769)
        {
            ItemMicroPart.getMaterial(item) match
            {
                case bm:BlockMicroMaterial if bm.state.getBlock == Blocks.WOOL && bm.state.getBlock.getMetaFromState(bm.state) != colour =>
                    if (!world.isRemote) {
                        dropMaterial()
                        colour = bm.state.getBlock.getMetaFromState(bm.state).toByte
                        world.playSound(null, pos, bm.getSound.getPlaceSound, SoundCategory.BLOCKS, bm.getSound.getVolume, bm.getSound.getPitch)
                        sendColourUpdate()
                        if (!player.capabilities.isCreativeMode) item.shrink(1)
                    }
                    return true
                case _ =>
            }
        }

        false
    }

    @SideOnly(Side.CLIENT)
    override def doStaticTessellation(pos:Vector3, ccrs:CCRenderState)
    {
        super.doStaticTessellation(pos, ccrs)
        if (colour > -1) RenderPipe.renderColourWool(this, pos, colour, ccrs)
    }
}

trait IInventoryProvider
{
    def getInventory(extractSide:Int):InvWrapper
    def getInventory:InvWrapper
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
        world.getTileEntity(posOfStraight(s)) match
        {
            case sinv:ISidedInventory => sinv.getSlotsForFace(EnumFacing.getFront(s^1)).nonEmpty
            case inv:IInventory => true
            case _ => false
        }
    }

    def shiftOrientation(force:Boolean)
    {
        if (world.isRemote) return
        val invalid = force || inOutSide == 6 || !maskConnects(inOutSide) ||
                !world.getTileEntity(posOfStraight(inOutSide)).isInstanceOf[IInventory]
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
                    val t = world.getTileEntity(posOfStraight(inOutSide))
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

    override def getInventory(extractSide:Int) =
    {
        if ((0 until 6 contains inOutSide) && (0 until 6 contains extractSide))
            InvWrapper.wrap(world, posOfStraight(inOutSide), EnumFacing.VALUES(extractSide))
        else null
    }

    override def getInventory =
    {
        getInventory(getInterfacedSide)
    }

    override def getInterfacedSide = if (!(0 to 5 contains inOutSide)) -1 else inOutSide^1

    abstract override def activate(player:EntityPlayer, hit:CuboidRayTraceResult, item:ItemStack, hand:EnumHand):Boolean =
    {
        if (super.activate(player, hit, item, hand)) return true

        if (!item.isEmpty && item.getItem.isInstanceOf[IScrewdriver] && item.getItem.asInstanceOf[IScrewdriver].canUse(player, item))
        {
            if (!world.isRemote)
            {
                shiftOrientation(true)
                item.getItem.asInstanceOf[IScrewdriver].damageScrewdriver(player, item)
            }
            return true
        }

        false
    }
}
