package mrtjp.projectred.transportation

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.lighting.LazyLightMatrix
import codechicken.lib.packet.PacketCustom
import codechicken.lib.raytracer.IndexedCuboid6
import codechicken.lib.render.{CCRenderState, TextureUtils}
import codechicken.lib.vec.{Rotation, Cuboid6, BlockCoord, Vector3}
import codechicken.microblock.handler.MicroblockProxy
import codechicken.microblock.{BlockMicroMaterial, ItemMicroPart, IHollowConnect}
import codechicken.multipart._
import cpw.mods.fml.relauncher.{SideOnly, Side}
import mrtjp.projectred.ProjectRedCore
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core._
import mrtjp.projectred.core.inventory.InvWrapper
import mrtjp.projectred.transmission.IWirePart._
import mrtjp.projectred.transmission._
import mrtjp.projectred.transportation.SendPriority.SendPriority
import net.minecraft.block.Block
import net.minecraft.client.renderer.RenderBlocks
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.ItemStack
import net.minecraft.nbt.{NBTTagList, NBTTagCompound}
import net.minecraft.util.{Icon, ChatMessageComponent, MovingObjectPosition}
import net.minecraftforge.common.ForgeDirection
import scala.collection.JavaConversions._
import mrtjp.projectred.core.libmc.BasicUtils

abstract class SubcorePipePart extends TMultiPart with TCenterConnectable with TPropagationAcquisitions with TSwitchPacket with TNormalOcclusion with IHollowConnect
{
    var meta:Byte = 0
    var signal:Byte = 0
    var material = false

    def preparePlacement(side:Int, meta:Int)
    {
        this.meta = meta.asInstanceOf[Byte]
    }

    override def save(tag:NBTTagCompound)
    {
        tag.setInteger("connMap", connMap)
        tag.setBoolean("mat", material)
        tag.setByte("signal", signal)
        tag.setByte("meta", meta)
    }

    override def load(tag:NBTTagCompound)
    {
        connMap = tag.getInteger("connMap")
        material = tag.getBoolean("mat")
        signal = tag.getByte("signal")
        meta = tag.getByte("meta")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        packet.writeByte(clientConnMap)
        packet.writeBoolean(material)
        packet.writeByte(signal)
        packet.writeByte(meta)
    }

    override def readDesc(packet:MCDataInput)
    {
        connMap = packet.readUByte()
        material = packet.readBoolean()
        signal = packet.readByte
        meta = packet.readByte
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 0 =>
            connMap = packet.readUByte()
            tile.markRender()
        case 1 =>
            material = packet.readBoolean()
            tile.markRender()
        case 3 =>
            signal = packet.readByte
            tile.markRender()
        case _ =>
    }

    def clientConnMap = connMap&0x3F|connMap>>6&0x3F

    def sendConnUpdate()
    {
        getWriteStreamOf(0).writeByte(clientConnMap)
    }

    def sendMatUpdate()
    {
        if (!world.isRemote)
        {
            if (updateInward()) sendConnUpdate()
            WirePropagator.propagateTo(this, FORCE)
        }
        getWriteStreamOf(1).writeBoolean(material)
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

    override def onRemoved()
    {
        super.onRemoved()
        if (!world.isRemote) notifyAllExternals()
    }

    override def onChunkLoad()
    {
        if ((connMap&0x80000000) != 0) // converter flag
        {
            connMap = 0
            updateOutward()
            tile.markDirty()
        }
    }

    override def onMaskChanged()
    {
        sendConnUpdate()
    }

    def getItem = getPipeType.getItemStack
    def getPipeType = PipeDef.VALID_PIPE(meta)

    def getType = getPipeType.partname

    override def getStrength(hit:MovingObjectPosition, player:EntityPlayer)  = 2

    override def getDrops =
    {
        var drops = Seq(getItem)
        if (material) drops :+= createMaterialStack
        drops
    }

    def createMaterialStack = ItemMicroPart.create(769, Block.blockRedstone.getUnlocalizedName)

    override def pickItem(hit:MovingObjectPosition) = getItem

    override def getHollowSize = 8

    override def isWireSide(side:Int) = true

    def debug(player:EntityPlayer) = false

    def test(player:EntityPlayer) = false

    override def activate(player:EntityPlayer, hit:MovingObjectPosition, held:ItemStack) =
    {
        def dropMaterial()
        {
            if (material && !player.capabilities.isCreativeMode)
            {
                val drop = createMaterialStack
                BasicUtils.dropItemFromLocation(world, drop, false, player, -1, 0, new BlockCoord(tile))
            }
        }

        if (CommandDebug.WIRE_READING) debug(player)
        else if (held != null && held.itemID == ProjectRedCore.itemWireDebugger.itemID)
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
                case bm:BlockMicroMaterial if bm.block == Block.blockRedstone =>
                    if (!world.isRemote)
                    {
                        material = true
                        world.playSoundEffect(x+0.5, y+0.5, z+0.5, Block.soundGlassFootstep.getPlaceSound,
                            Block.soundGlassFootstep.getVolume*5.0F, Block.soundGlassFootstep.getPitch*.9F)
                        sendMatUpdate()
                        if (!player.capabilities.isCreativeMode) held.stackSize-=1
                    }
                    true
                case _ => false
            }
        }
        else false
    }
}

object PipeBoxes
{
    var oBounds =
    {
        val boxes = new Array[Cuboid6](7)
        val w = 2/8D
        boxes(6) = new Cuboid6(0.5-w, 0.5-w, 0.5-w, 0.5+w, 0.5+w, 0.5+w)
        for (s <- 0 until 6)
            boxes(s) = new Cuboid6(0.5-w, 0, 0.5-w, 0.5+w, 0.5-w, 0.5+w).apply(Rotation.sideRotations(s).at(Vector3.center))
        boxes
    }
    var expandBounds = -1
}

abstract class CorePipePart extends SubcorePipePart with TCenterRSAcquisitions with IRedwirePart with IMaskedRedstonePart
{
    override def strongPowerLevel(side:Int) = 0
    override def weakPowerLevel(side:Int) =
    {
        if (!maskConnects(side) || !material) 0
        else rsLevel
    }
    override def canConnectRedstone(side:Int) = material
    override def getConnectionMask(side:Int) = 0x10

    override def canConnectPart(part:IConnectable, s:Int) =
        part.isInstanceOf[FramedRedwirePart] && material

    override def discoverOpen(s:Int) = getInternal(s) match
    {
        case null => true
        case _ =>
            PipeBoxes.expandBounds = s
            val fits = tile.canReplacePart(this, this)
            PipeBoxes.expandBounds = -1
            fits
    }

    override def discoverInternal(s:Int) = false

    override def discoverStraightOverride(absDir:Int) =
    {
        if (material)
        {
            WirePropagator.setRedwiresConnectable(false)
            val b = (RedstoneInteractions.otherConnectionMask(world, x, y, z, absDir, false)&RedstoneInteractions.connectionMask(this, absDir)) != 0
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

    override def resolveSignal(part:TMultiPart, s:Int) = part match
    {
        case rw:IRedwirePart if rw.isWireSide(s) => rw.getRedwireSignal(s)-1
        case re:IRedwireEmitter => re.getRedwireSignal(s)
        case _ => 0
    }

    override def getSubParts =
    {
        val b = getCollisionBoxes
        var i = Seq[IndexedCuboid6]()
        for (c <- b) i :+= new IndexedCuboid6(0, c)
        i
    }

    override def getOcclusionBoxes =
    {
        import PipeBoxes._
        if (expandBounds >= 0) Seq(oBounds(expandBounds))
        else Seq(oBounds(6))
    }

    override def getCollisionBoxes =
    {
        import PipeBoxes._
        var boxes = Seq(oBounds(6))
        for (s <- 0 until 6) if (maskConnects(s)) boxes :+= oBounds(s)
        boxes
    }

    override def debug(player:EntityPlayer) =
    {
        player.sendChatToPlayer(ChatMessageComponent.createFromTranslationKey(
            (if (world.isRemote) "Client" else "Server")+" signal strength: "+getRedwireSignal))
        true
    }

    override def test(player:EntityPlayer) =
    {
        if (BasicUtils.isClient(world)) Messenger.addMessage(x, y+.5f, z, "/#f/#c[c] = "+getRedwireSignal)
        else
        {
            val packet = new PacketCustom(CoreSPH.channel, CoreSPH.messagePacket)
            packet.writeDouble(x+0.0D)
            packet.writeDouble(y+0.5D)
            packet.writeDouble(z+0.0D)
            packet.writeString("/#c[s] = "+getRedwireSignal)
            packet.sendToPlayer(player)
        }
        true
    }
}

class FlowingPipePart extends CorePipePart
{
    val itemFlow = new PayloadMovement
    var logic:PipeLogic = null
    var initialized = false

    override def preparePlacement(side:Int, meta:Int)
    {
        super.preparePlacement(side, meta)
        logic = PipeLogic.createPipeLogic(this, meta)
    }

    def getLogic = logic

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        val nbttaglist = new NBTTagList
        for (r <- itemFlow.it)
        {
            val payloadData = new NBTTagCompound
            nbttaglist.appendTag(payloadData)
            r.save(payloadData)
        }
        tag.setTag("itemFlow", nbttaglist)
        getLogic.save(tag)
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        val nbttaglist = tag.getTagList("itemFlow")

        for (j <- 0 until nbttaglist.tagCount)
        {
            try
            {
                val payloadData = nbttaglist.tagAt(j).asInstanceOf[NBTTagCompound]
                val r = RoutedPayload()
                r.bind(this)
                r.load(payloadData)
                if (!r.isCorrupted) itemFlow.scheduleLoad(r)
            }
            catch {case t:Throwable =>}
        }

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
        case 4 => handleItemUpdatePacket(packet)
        case k if k >= 50 => logic.read(packet, key)
        case _ => super.read(packet, key)
    }

    override def update()
    {
        super.update()
        if (!initialized) initialized = true
        pushItemFlow()
        getLogic.tick()
    }

    def pushItemFlow()
    {
        itemFlow.executeLoad()
        itemFlow.exececuteRemove()
        for (r <- itemFlow.it) if (r.isCorrupted) itemFlow.scheduleRemoval(r)
        else
        {
            r.moveProgress(r.speed)
            if (r.isEntering && hasReachedMiddle(r))
            {
                r.isEntering = false
                if (r.output == ForgeDirection.UNKNOWN) handleDrop(r)
                else centerReached(r)
            }
            else if (!r.isEntering && hasReachedEnd(r) && itemFlow.scheduleRemoval(r)) endReached(r)
        }
        itemFlow.exececuteRemove()
    }

    def handleDrop(r:RoutedPayload)
    {
        if (getLogic.handleDrop(r)) return
        if (itemFlow.scheduleRemoval(r)) if (!world.isRemote)
        {
            r.resetTrip
            world.spawnEntityInWorld(r.getEntityForDrop(x, y, z))
        }
    }

    def resolveDestination(r:RoutedPayload)
    {
        if (getLogic.resolveDestination(r)) return
        chooseRandomDestination(r)
    }

    def chooseRandomDestination(r:RoutedPayload)
    {
        var moves = Seq[ForgeDirection]()
        for (i <- 0 until 6) if((connMap&1<<i) != 0 && i != r.input.getOpposite.ordinal)
        {
            val t = getStraight(i)
            if (t.isInstanceOf[FlowingPipePart]) moves :+= ForgeDirection.getOrientation(i)
        }

        if (moves.isEmpty) r.output = r.input.getOpposite
        else r.output = moves(world.rand.nextInt(moves.size))
    }

    def endReached(r:RoutedPayload)
    {
        if (getLogic.endReached(r)) return
        if (!world.isRemote) if (!maskConnects(r.output.ordinal) || !passToNextPipe(r))
        {
            val inv = InvWrapper.getInventory(world, new BlockCoord(tile).offset(r.output.ordinal))
            if (inv != null)
            {
                val w = InvWrapper.wrap(inv).setSlotsFromSide(r.output.getOpposite.ordinal)
                r.payload.stackSize -= w.injectItem(r.payload.makeStack, true)
            }
            if (r.payload.stackSize > 0) bounceStack(r)
        }
    }

    def bounceStack(r:RoutedPayload)
    {
        itemFlow.unscheduleRemoval(r)
        r.isEntering = true
        r.input = r.output.getOpposite
        resolveDestination(r)
        adjustSpeed(r)
        sendItemUpdate(r)
    }

    def centerReached(r:RoutedPayload)
    {
        if (getLogic.centerReached(r)) return
        if (!maskConnects(r.output.ordinal)) resolveDestination(r)
    }

    def passToNextPipe(r:RoutedPayload) =
    {
        getStraight(r.output.ordinal()) match
        {
            case pipe:FlowingPipePart =>
                pipe.injectPayload(r, r.output)
                true
            case _ => false
        }
    }

    def adjustSpeed(r:RoutedPayload)
    {
        r.speed = Math.max(r.speed-0.01f, r.priority.speed)
    }

    protected def hasReachedMiddle(r:RoutedPayload) = r.progress >= 0.5F

    protected def hasReachedEnd(r:RoutedPayload) = r.progress >= 1.0F

    def injectPayload(r:RoutedPayload, in:ForgeDirection)
    {
        if (r.isCorrupted) return
        if (itemFlow.delegate.contains(r)) return
        r.bind(this)
        r.reset()
        r.input = in
        itemFlow.add(r)

        adjustSpeed(r)
        if (r.progress > 0.0F) r.progress = Math.max(0, r.progress-1.0F)

        if (!world.isRemote)
        {
            resolveDestination(r)
            sendItemUpdate(r)
        }
    }

    override def onNeighborChanged()
    {
        super.onNeighborChanged()
        val connCount = Integer.bitCount(connMap)

        if (connCount == 0) if (!world.isRemote) for (r <- itemFlow.it) if (itemFlow.scheduleRemoval(r))
        {
            r.resetTrip
            world.spawnEntityInWorld(r.getEntityForDrop(x, y, z))
        }
    }

    override def onRemoved()
    {
        super.onRemoved()
        if (!world.isRemote) for (r <- itemFlow.it)
        {
            r.resetTrip
            world.spawnEntityInWorld(r.getEntityForDrop(x, y, z))
        }
    }

    def sendItemUpdate(r:RoutedPayload)
    {
        val out = getWriteStreamOf(4)
        out.writeShort(r.payloadID)
        out.writeFloat(r.progress)
        out.writeItemStack(r.getItemStack)
        out.writeByte(r.input.ordinal.asInstanceOf[Byte])
        out.writeByte(r.output.ordinal.asInstanceOf[Byte])
        out.writeFloat(r.speed)
        out.writeByte(r.priority.ordinal)
    }

    def handleItemUpdatePacket(packet:MCDataInput)
    {
        val id = packet.readShort
        val progress = packet.readFloat
        var r = itemFlow.get(id)
        if (r == null)
        {
            r = RoutedPayload(id)
            r.progress = progress
            itemFlow.add(r)
        }
        r.setItemStack(packet.readItemStack)
        r.input = ForgeDirection.getOrientation(packet.readByte)
        r.output = ForgeDirection.getOrientation(packet.readByte)
        r.speed = packet.readFloat
        r.setPriority(SendPriority(packet.readUByte).asInstanceOf[SendPriority])
    }

    /**
     * Filter for items this pipe will except on the dir
     * @param inputDir Input dir
     * @return The path filter for this input dir
     */
    def routeFilter(inputDir:Int) = PathFilter.default

    def routeWeight = 1

    @SideOnly(Side.CLIENT)
    override def drawBreaking(r:RenderBlocks)
    {
        RenderPipe.renderBreakingOverlay(r.overrideBlockTexture, this)
    }

    @SideOnly(Side.CLIENT)
    override def renderStatic(pos:Vector3, olm:LazyLightMatrix, pass:Int)
    {
        if (pass == 0)
        {
            TextureUtils.bindAtlas(0)
            CCRenderState.setBrightness(world, x, y, z)
            RenderPipe.render(this, pos, olm)
            CCRenderState.setColour(-1)
        }
    }

    @SideOnly(Side.CLIENT)
    override def renderDynamic(pos:Vector3, frame:Float, pass:Int)
    {
        if (pass == 0)
        {
            TextureUtils.bindAtlas(0)
            CCRenderState.reset()
            CCRenderState.setBrightness(world, x, y, z)
            CCRenderState.useModelColours(true)
            RenderPipe.renderItemFlow(this, pos, frame)
            CCRenderState.setColour(-1)
        }
    }

    @SideOnly(Side.CLIENT)
    def getIcon(side:Int) =
    {
        val i = getLogic.getIcon(side)
        if (i != null) i
        else getPipeType.sprites(0)
    }

    override def canConnectPart(part:IConnectable, s:Int) = part match
    {
        case p:FlowingPipePart => true
        case _ => super.canConnectPart(part, s)
    }

    override def activate(player:EntityPlayer, hit:MovingObjectPosition, held:ItemStack) =
    {
        if (super.activate(player, hit, held)) true
        else false
    }
}

object PipeLogic
{
    def createPipeLogic(p:FlowingPipePart, meta:Int) = meta match
    {
        case 0 => new NullPipeLogic(p)
        case _ => new NullPipeLogic(p)
    }

    def apply(p:FlowingPipePart, meta:Int) = createPipeLogic(p, meta)

    class NullPipeLogic(p:FlowingPipePart) extends PipeLogic(p)
    {
        def endReached(r:RoutedPayload) = false
        def centerReached(r:RoutedPayload) = false
        def handleDrop(r:RoutedPayload) = false
        def resolveDestination(r:RoutedPayload) = false
        def getIcon(i:Int) = null
    }
}

abstract class PipeLogic(p:FlowingPipePart)
{
    def save(tag:NBTTagCompound) {}

    def load(tag:NBTTagCompound) {}

    def readDesc(packet:MCDataInput) {}

    def writeDesc(packet:MCDataOutput) {}

    /**
     *
     * @param packet
     * @param key allocated >= 50
     */
    def read(packet:MCDataInput, key:Int) {}

    def tick() {}

    def endReached(r:RoutedPayload):Boolean

    def centerReached(r:RoutedPayload):Boolean

    def handleDrop(r:RoutedPayload):Boolean

    def resolveDestination(r:RoutedPayload):Boolean

    def getIcon(i:Int):Icon
}