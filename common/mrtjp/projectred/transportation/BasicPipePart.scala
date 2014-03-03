package mrtjp.projectred.transportation

import codechicken.lib.data.MCDataInput
import codechicken.lib.data.MCDataOutput
import codechicken.lib.lighting.LazyLightMatrix
import codechicken.lib.render.CCRenderState
import codechicken.lib.render.TextureUtils
import codechicken.lib.vec.BlockCoord
import codechicken.lib.vec.Vector3
import cpw.mods.fml.relauncher.Side
import cpw.mods.fml.relauncher.SideOnly
import mrtjp.projectred.core.BasicUtils
import mrtjp.projectred.core.inventory.InvWrapper
import net.minecraft.client.renderer.RenderBlocks
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.nbt.NBTTagList
import net.minecraft.util.Icon
import net.minecraftforge.common.ForgeDirection

class BasicPipePart extends CorePipePart
{
    var itemFlow = new PayloadMovement
    var logic:PipeLogic = null
    var initialized = false

    override def preparePlacement(meta:Int)
    {
        super.preparePlacement(meta)
        logic = PipeLogic.createPipeLogic(this, meta)
    }

    def getLogic = logic

    override def update()
    {
        if (!initialized) initialized = true
        pushItemFlow()
        getLogic.tick()
    }

    protected def pushItemFlow()
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
                r.progress = 0.5F
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
            val bc = new BlockCoord(tile).offset(i)
            val t = BasicUtils.getMultiPart(world, bc, 6)
            if (t.isInstanceOf[BasicPipePart]) moves :+= ForgeDirection.getOrientation(i)
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

    def passToNextPipe(r:RoutedPayload):Boolean =
    {
        val p = BasicUtils.getMultiPart(world, new BlockCoord(tile).offset(r.output.ordinal), 6)
        if (p.isInstanceOf[BasicPipePart])
        {
            val pipe = p.asInstanceOf[BasicPipePart]
            pipe.injectPayload(r, r.output)
            return true
        }
        false
    }

    def adjustSpeed(r:RoutedPayload)
    {
        r.speed = Math.max(r.speed - 0.01f, r.priority.speed)
    }

    protected def hasReachedMiddle(r:RoutedPayload) = r.progress >= 0.5F

    protected def hasReachedEnd(r:RoutedPayload) = r.progress >= 1.0F

    def injectPayload(r:RoutedPayload, in:ForgeDirection)
    {
        if (r.isCorrupted) return
        r.bind(this)
        r.reset()
        r.input = in
        itemFlow.add(r)

        adjustSpeed(r)
        if (r.progress > 0.0F) r.progress = r.progress-1.0F

        if (!world.isRemote)
        {
            resolveDestination(r)
            sendItemUpdate(r)
        }
    }

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

    override def read(packet:MCDataInput, switch_key:Int)
    {
        super.read(packet, switch_key)
        if (switch_key == 1) handleItemUpdatePacket(packet)
        getLogic.read(packet, switch_key)
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
        val out = tile.getWriteStream(this).writeByte(1)
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
        r.setPriority(SendPriority.typeValues(packet.readUByte))
    }

    def getType = "pr_ptube"

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
            CCRenderState.reset()
            CCRenderState.setBrightness(world, x, y, z)
            CCRenderState.useModelColours(true)
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

    def getIcon(side:Int):Icon =
    {
        val i = getLogic.getIcon(side)
        if (i != null) return i
        EnumPipe.BASIC.sprites(0)
    }
}