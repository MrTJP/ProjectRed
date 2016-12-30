package mrtjp.projectred.transportation

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.raytracer.{CuboidRayTraceResult, IndexedCuboid6}
import codechicken.lib.render.CCRenderState
import codechicken.lib.texture.TextureUtils
import codechicken.lib.vec.{Cuboid6, Rotation, Vector3}
import codechicken.microblock.ISidedHollowConnect
import codechicken.multipart._
import mrtjp.core.inventory.InvWrapper
import mrtjp.core.item.ItemKey
import mrtjp.projectred.api.IConnectable
import mrtjp.projectred.core._
import net.minecraft.client.renderer.texture.TextureAtlasSprite
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.nbt.{NBTTagCompound, NBTTagList}
import net.minecraft.util.{BlockRenderLayer, ITickable}
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

import scala.collection.JavaConversions._

abstract class SubcorePipePart extends TMultiPart with TCenterConnectable with TSwitchPacket with TNormalOcclusionPart with ISidedHollowConnect
{
    var meta:Byte = 0

    def preparePlacement(side:Int, meta:Int)
    {
        this.meta = meta.asInstanceOf[Byte]
    }

    override def save(tag:NBTTagCompound)
    {
        tag.setInteger("connMap", connMap)
        tag.setByte("meta", meta)
    }

    override def load(tag:NBTTagCompound)
    {
        connMap = tag.getInteger("connMap")
        meta = tag.getByte("meta")
    }

    override def writeDesc(packet:MCDataOutput)
    {
        packet.writeByte(clientConnMap)
        packet.writeByte(meta)
    }

    override def readDesc(packet:MCDataInput)
    {
        connMap = packet.readUByte()
        meta = packet.readByte()
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 1 =>
            connMap = packet.readUByte()
            tile.markRender()
        case _ =>
    }

    def clientConnMap = connMap&0x3F|connMap>>6&0x3F

    def sendConnUpdate()
    {
        getWriteStreamOf(1).writeByte(clientConnMap)
    }

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

    override def onPartChanged(part:TMultiPart)
    {
        if (!world.isRemote) if (updateOutward()) onMaskChanged()//sendConnUpdate()
    }

    override def onNeighborChanged()
    {
        if (!world.isRemote) if (updateExternalConns()) onMaskChanged()//sendConnUpdate()
    }

    override def onAdded()
    {
        super.onAdded()
        if (!world.isRemote) if (updateInward()) onMaskChanged()//sendConnUpdate()
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

    def getItem = getPipeType.makeStack
    def getPipeType = PipeDefs.values(meta)

    def getType = getPipeType.partname

    override def getStrength(player:EntityPlayer, hit:CuboidRayTraceResult)  = 2

    override def getDrops = Seq(getItem)

    override def pickItem(hit:CuboidRayTraceResult) = getItem

    override def getHollowSize(side:Int) = 8

    override def getSubParts =
    {
        import mrtjp.projectred.transportation.PipeBoxes._
        var boxes = Seq(new IndexedCuboid6(-1, oBounds(6)))
        for (s <- 0 until 6) if (maskConnects(s)) boxes :+= new IndexedCuboid6(s, oBounds(s))
        boxes
    }

    override def getOcclusionBoxes =
    {
        import mrtjp.projectred.transportation.PipeBoxes._
        if (expandBounds >= 0) Seq(oBounds(expandBounds))
        else Seq(oBounds(6))
    }

    override def getCollisionBoxes =
    {
        import mrtjp.projectred.transportation.PipeBoxes._
        var boxes = Seq(oBounds(6))
        for (s <- 0 until 6) if (maskConnects(s)) boxes :+= oBounds(s)
        boxes
    }

    @SideOnly(Side.CLIENT)
    override def renderBreaking(pos:Vector3, texture:TextureAtlasSprite, ccrs:CCRenderState)
    {
        RenderPipe.renderBreakingOverlay(texture, this, ccrs)
    }

    override def renderStatic(pos:Vector3, layer:BlockRenderLayer, ccrs:CCRenderState) =
    {
        if (layer == BlockRenderLayer.CUTOUT) {
            ccrs.setBrightness(world, this.pos)
            doStaticTessellation(pos, ccrs)
            true
        }
        else false
    }

    @SideOnly(Side.CLIENT)
    override def renderDynamic(pos:Vector3, pass:Int, frame:Float)
    {
        if (pass == 0) {
            TextureUtils.bindBlockTexture()
            doDynamicTessellation(pos, frame, CCRenderState.instance())
        }
    }

    @SideOnly(Side.CLIENT)
    def getIcon(side:Int) = getPipeType.sprites(0)

    @SideOnly(Side.CLIENT)
    def doStaticTessellation(pos:Vector3, ccrs:CCRenderState)
    {
        RenderPipe.renderPipe(this, pos, ccrs)
    }

    @SideOnly(Side.CLIENT)
    def doDynamicTessellation(pos:Vector3, frame:Float, ccrs:CCRenderState){}
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

trait TPipeTravelConditions
{
    /**
     * 00FT
     * T - can travel to
     * F - can come from
     */
    def getPathFlags(input:Int, output:Int) = 0x3

    def getPathWeight = 1

    def itemsExclude = true
    def filteredItems:Set[ItemKey] = Set.empty

    def colorExclude = true
    def filteredColors = 0

    def pathFilter:PathFilter = pathFilter(-1, -1)
    def pathFilter(inputDir:Int, outputDir:Int):PathFilter =
    {
        val f = new PathFilter
        if (inputDir != -1 && outputDir != -1)
            f.pathFlags = getPathFlags(inputDir, outputDir)

        f.filterExclude = itemsExclude
        f.itemFilter = filteredItems

        f.colorExclude = colorExclude
        f.colors = filteredColors
        f
    }
}

abstract class PayloadPipePart[T <: AbstractPipePayload] extends SubcorePipePart with TPipeTravelConditions with ITickable
{
    val itemFlow = new PayloadMovement[T]
    var initialized = false

    private implicit def payloadToT(p:AbstractPipePayload):T = p.asInstanceOf[T]

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
    }

    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        val nbttaglist = tag.getTagList("itemFlow", 10)
        for (j <- 0 until nbttaglist.tagCount)
        {
            try
            {
                val payloadData = nbttaglist.getCompoundTagAt(j)
                val r = createNewPayload(AbstractPipePayload.claimID())
                r.bind(this)
                r.load(payloadData)
                if (!r.isCorrupted) itemFlow.scheduleLoad(r)
            }
            catch {case t:Throwable =>}
        }
    }

    override def read(packet:MCDataInput, key:Int) = key match
    {
        case 4 => handleItemUpdatePacket(packet)
        case _ => super.read(packet, key)
    }

    override def update()
    {
        if (!initialized) initialized = true
        pushItemFlow()
    }

    def pushItemFlow()
    {
        itemFlow.executeLoad()
        itemFlow.exececuteRemove()
        for (r <- itemFlow.it) if (r.isCorrupted) itemFlow.scheduleRemoval(r)
        else {
            r.moveProgress(r.speed)
            if (r.isEntering && hasReachedMiddle(r))
            {
                r.isEntering = false
                if (r.output == 6) handleDrop(r)
                else centerReached(r)
            }
            else if (!r.isEntering && hasReachedEnd(r))
                if (itemFlow.scheduleRemoval(r)) endReached(r)
        }
        itemFlow.exececuteRemove()
    }

    def handleDrop(r:T)
    {
        if (itemFlow.scheduleRemoval(r)) if (!world.isRemote)
        {
            r.preItemRemove()
            world.spawnEntityInWorld(r.getEntityForDrop(x, y, z))
        }
    }

    def resolveDestination(r:T)
    {
        chooseRandomDestination(r)
    }

    def chooseRandomDestination(r:T)
    {
        chooseRandomDestination(r, 0)
    }

    def chooseRandomDestination(r:T, mask:Int)
    {
        var moves = Seq[Int]()
        for (i <- 0 until 6)
            if((connMap&1<<i) != 0 && i != (r.input^1) && (mask&1<<i) == 0) moves :+= i
        if (moves.isEmpty) r.output = r.input^1
        else r.output = moves(world.rand.nextInt(moves.size))
    }

    def endReached(r:T)
    {
        if (!world.isRemote)
        {
            if(!(maskConnects(r.output) && passPayload(r)))
                if (r.payload.stackSize > 0) bounceStack(r)
        }
    }

    def passPayload(r:T):Boolean =
    {
        if (passToInventory(r)) return true

        if (passToNextPipe(r)) return true

        false
    }

    def passToNextPipe(r:T) =
    {
        getStraight(r.output) match
        {
            case pipe:PayloadPipePart[T] =>
                pipe.injectPayload(r, r.output)
                true
            case _ => false
        }
    }

    def passToInventory(r:T) =
    {
        val inv = InvWrapper.getInventory(world, posOfStraight(r.output))
        if (inv != null)
        {
            val w = InvWrapper.wrap(inv).setSlotsFromSide(r.output^1)
            r.payload.stackSize -= w.injectItem(r.payload.key, r.payload.stackSize)
            r.payload.stackSize == 0
        }
        else false
    }

    def bounceStack(r:T)
    {
        itemFlow.unscheduleRemoval(r)
        r.isEntering = true
        r.input = r.output^1
        r.progress = 0
        resolveDestination(r)
        adjustSpeed(r)
        if (!world.isRemote) sendItemUpdate(r)
    }

    def centerReached(r:T)
    {
        if (!maskConnects(r.output) && !world.isRemote)
        {
            resolveDestination(r)
            sendItemUpdate(r)
        }
    }

    def adjustSpeed(r:T){}

    protected def hasReachedMiddle(r:T) = r.progress >= 0.5F

    protected def hasReachedEnd(r:T) = r.progress >= 1.0F

    def injectPayload(r:T, in:Int)
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
            r.preItemRemove()
            world.spawnEntityInWorld(r.getEntityForDrop(x, y, z))
        }
    }

    override def onRemoved()
    {
        super.onRemoved()
        if (!world.isRemote) for (r <- itemFlow.it)
        {
            r.preItemRemove()
            world.spawnEntityInWorld(r.getEntityForDrop(x, y, z))
        }
    }

    def sendItemUpdate(r:T)
    {
        val out = getWriteStreamOf(4)
        out.writeShort(r.payloadID)
        r.writeDesc(out)
    }

    def handleItemUpdatePacket(packet:MCDataInput)
    {
        val id = packet.readShort()
        val r = itemFlow.getOrElseUpdate(id, _ => createNewPayload(id))
        r.readDesc(packet)
    }

    def createNewPayload(id:Int):T

    @SideOnly(Side.CLIENT)
    override def doDynamicTessellation(pos:Vector3, frame:Float, ccrs:CCRenderState)
    {
        super.doDynamicTessellation(pos, frame, ccrs)
        RenderPipe.renderItemFlow(this, pos, frame, ccrs)
    }

    override def canConnectPart(part:IConnectable, s:Int) = false
}
