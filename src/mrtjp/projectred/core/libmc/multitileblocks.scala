package mrtjp.projectred.core.libmc

import net.minecraft.block.{Block, BlockContainer}
import net.minecraft.block.material.Material
import net.minecraft.world.{EnumSkyBlock, IBlockAccess, World}
import net.minecraft.entity.player.EntityPlayer
import mrtjp.projectred.core.{ItemBlockCore, CoreSPH}
import cpw.mods.fml.relauncher.{SideOnly, Side}
import java.util.Random
import net.minecraft.tileentity.TileEntity
import net.minecraft.enchantment.EnchantmentHelper
import net.minecraft.item.ItemStack
import java.util
import codechicken.lib.vec.BlockCoord
import net.minecraft.entity.{Entity, EntityLivingBase}
import net.minecraftforge.common.util.ForgeDirection
import codechicken.lib.packet.{PacketCustom, ICustomPacketTile}
import net.minecraft.util.{MovingObjectPosition, AxisAlignedBB}
import codechicken.lib.data.{MCDataInput, MCDataOutput}
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.client.renderer.RenderBlocks
import scala.collection.mutable.ListBuffer
import scala.collection.JavaConversions._
import net.minecraft.init.Blocks
import cpw.mods.fml.common.registry.GameRegistry

class MultiTileBlock(name:String, mat:Material) extends BlockContainer(mat)
{
    setBlockName(name)
    GameRegistry.registerBlock(this, getItemBlockClass, name)
    def getItemBlockClass = classOf[ItemBlockCore]

    private var singleTile = false
    private val tiles = new Array[Class[_ <: TileEntity]](16)

    override def isOpaqueCube = false

    override def renderAsNormalBlock = false

    override def damageDropped(damage:Int) = damage

    override def harvestBlock(w:World, player:EntityPlayer, x:Int, y:Int, z:Int, l:Int){}

    override def getRenderType = BasicRenderUtils.coreRenderHandlerID

    @SideOnly(Side.CLIENT)
    override def randomDisplayTick(w:World, x:Int, y:Int, z:Int, rand:Random)
    {
        val md = w.getBlockMetadata(x, y, z)
        val r = BasicRenderUtils.getRenderer(this, md)
        if (r != null) r.randomDisplayTick(w, x, y, z, rand)
    }

    override def createNewTileEntity(var1:World, var2:Int) = null

    override def createTileEntity(world:World, meta:Int) =
    {
        try {if (singleTile) tiles(0).newInstance else tiles(meta).newInstance}
        catch {case e:Exception => e.printStackTrace()}
        null
    }

    def addTile[A <: TileEntity](t:Class[A], meta:Int)
    {
        tiles(meta) = t
        GameRegistry.registerTileEntity(t, getUnlocalizedName+"|"+meta)
    }
    def addSingleTile[A <: TileEntity](t:Class[A]){addTile(t, 0); singleTile = true}

    override def removedByPlayer(world:World, player:EntityPlayer, x:Int, y:Int, z:Int) =
    {
        if (world.isRemote) true
        else
        {
            val b = world.getBlock(x, y, z)
            val md = world.getBlockMetadata(x, y, z)
            if (b.canHarvestBlock(player, md) && !player.capabilities.isCreativeMode)
            {
                val il = getDrops(world, x, y, z, md, EnchantmentHelper.getFortuneModifier(player))
                for (it <- il) PRLib.dropItem(world, x, y, z, it)
            }
            world.setBlock(x, y, z, Blocks.air)
            true
        }
    }

    override def getDrops(w:World, x:Int, y:Int, z:Int, meta:Int, fortune:Int) =
    {
        val list = new ListBuffer[ItemStack]
        w.getTileEntity(x, y, z) match
        {
            case t:MultiTileTile => t.addHarvestContents(list)
            case _ =>
        }
        new util.ArrayList[ItemStack](list)
    }

    override def getPickBlock(target:MovingObjectPosition, w:World, x:Int, y:Int, z:Int) =  w.getTileEntity(x, y, z) match
    {
        case t:MultiTileTile => t.getPickBlock
        case _ => super.getPickBlock(target, w, x, y, z)
    }

    override def onNeighborBlockChange(w:World, x:Int, y:Int, z:Int, b:Block)
    {
        w.getTileEntity(x, y, z) match
        {
            case t:MultiTileTile => t.onNeighborChange(b)
            case _ =>
        }
    }

    override def onBlockPlaced(w:World, x:Int, y:Int, z:Int, side:Int, hx:Float, hy:Float, hz:Float, meta:Int) =
    {
        w.getTileEntity(x, y, z) match
        {
            case t:MultiTileTile => t.onBlockPlaced(side, meta)
            case _ =>
        }
        meta
    }

    override def onBlockPlacedBy(w:World, x:Int, y:Int, z:Int, player:EntityLivingBase, stack:ItemStack)
    {
        w.getTileEntity(x, y, z) match
        {
            case t:MultiTileTile => t.onBlockPlacedBy(stack, player)
            case _ =>
        }
    }

    override def breakBlock(w:World, x:Int, y:Int, z:Int, b:Block, meta:Int)
    {
        w.getTileEntity(x, y, z) match
        {
            case t:MultiTileTile => t.onBlockRemoval()
            case _ => super.breakBlock(w, x, y, z, b, meta)
        }
    }

    override def isProvidingStrongPower(w:IBlockAccess, x:Int, y:Int, z:Int, side:Int) = w.getTileEntity(x, y, z) match
    {
        case t:MultiTileTile => t.strongPower(side)
        case _ => 0
    }

    override def isProvidingWeakPower(w:IBlockAccess, x:Int, y:Int, z:Int, side:Int) =
        w.getTileEntity(x, y, z) match
        {
            case t:MultiTileTile => t.weakPower(side)
            case _ => 0

        }

    override def onBlockActivated(w:World, x:Int, y:Int, z:Int, player:EntityPlayer, side:Int, hx:Float, hy:Float, hz:Float) = w.getTileEntity(x, y, z) match
    {
        case t:MultiTileTile => t.onBlockActivated(player, side)
        case _ => false
    }


    override def onEntityCollidedWithBlock(w:World, x:Int, y:Int, z:Int, ent:Entity) = w.getTileEntity(x, y, z) match
    {
        case t:MultiTileTile => t.onEntityCollidedWithBlock(ent)
        case _ =>
    }

    override def getCollisionBoundingBoxFromPool(w:World, x:Int, y:Int, z:Int) =
    {
        def getSuper = super.getCollisionBoundingBoxFromPool(w, x, y, z)

        w.getTileEntity(x, y, z) match
        {
            case t:MultiTileTile => t.getCollisionBoundingBox match
            {
                case null => getSuper
                case bb => bb
            }
            case _ => getSuper
        }
    }

    override def getLightValue(w:IBlockAccess, x:Int, y:Int, z:Int) = w.getTileEntity(x, y, z) match
    {
        case t:MultiTileTile => t.getLightValue
        case _ => super.getLightValue(w, x, y, z)
    }

    override def isFireSource(w:World, x:Int, y:Int, z:Int, side:ForgeDirection) = w.getTileEntity(x, y, z) match
    {
        case t:MultiTileTile => t.isFireSource(side)
        case _ => super.isFireSource(w, x, y, z, side)
    }

    override def isSideSolid(w:IBlockAccess, x:Int, y:Int, z:Int, side:ForgeDirection) = w.getTileEntity(x, y, z) match
    {
        case t:MultiTileTile => t.isBlockSolidOnSide(side)
        case _ => super.isSideSolid(w, x, y, z, side)
    }
}

abstract class MultiTileTile extends TileEntity with ICustomPacketTile
{
    protected var schedTick = -1L

    def onNeighborChange(b:Block){}

    def onBlockPlaced(side:Int, meta:Int){}

    def onBlockPlacedBy(stack:ItemStack, player:EntityLivingBase){}

    def onBlockRemoval(){}

    def strongPower(side:Int) = 0
    def weakPower(side:Int) = strongPower(side)

    def getLightValue = 0

    def isFireSource(side:ForgeDirection) = false

    def isBlockSolidOnSide(side:ForgeDirection) = true

    def onBlockActivated(player:EntityPlayer, side:Int) = false

    def onEntityCollidedWithBlock(ent:Entity) {}

    def getCollisionBoundingBox:AxisAlignedBB = null

    def onScheduledTick(){}

    def updateClient(){}

    def update(){}

    def getBlock:Block

    def getMetaData = getBlockMetadata

    def getPickBlock = new ItemStack(getBlock, 1, getMetaData)

    def addHarvestContents(ist:ListBuffer[ItemStack])
    {
        ist += getPickBlock
    }

    def world = worldObj

    def scheduleTick(time:Int)
    {
        val tn = worldObj.getTotalWorldTime+time
        if (schedTick > 0L && schedTick < tn) return
        schedTick = tn
        markDirty()
    }

    def isTickScheduled = schedTick >= 0L

    def breakBlock()
    {
        val il = new ListBuffer[ItemStack]
        addHarvestContents(il)
        for (stack <- il) PRLib.dropItem(worldObj, xCoord, yCoord, zCoord, stack)
        worldObj.setBlock(xCoord, yCoord, zCoord, Blocks.air)
    }

    override def markDirty()
    {
        worldObj.markTileEntityChunkModified(xCoord, yCoord, zCoord, this)
    }

    final def markRender()
    {
        worldObj.func_147479_m(xCoord, yCoord, zCoord)
    }

    final def markLight()
    {
        worldObj.updateLightByType(EnumSkyBlock.Block, xCoord, yCoord, zCoord)
    }

    final override def updateEntity()
    {
        if (worldObj.isRemote)
        {
            updateClient()
            return
        }
        else update()
        if (schedTick < 0L) return
        val time = worldObj.getTotalWorldTime
        if (schedTick <= time)
        {
            schedTick = -1L
            onScheduledTick()
            markDirty()
        }
    }

    final override def readFromNBT(tag:NBTTagCompound)
    {
        super.readFromNBT(tag)
        schedTick = tag.getLong("sched")
        load(tag)
    }

    final override def writeToNBT(tag:NBTTagCompound)
    {
        super.writeToNBT(tag)
        tag.setLong("sched", schedTick)
        save(tag)
    }

    def save(tag:NBTTagCompound){}
    def load(tag:NBTTagCompound){}

    final override def getDescriptionPacket =
    {
        val packet = writeStream(0)
        writeDesc(packet)
        packet.toPacket
    }

    final def handleDescriptionPacket(packet:PacketCustom) = packet.readUByte() match
    {
        case 0 => readDesc(packet)
        case key => read(packet, key)
    }

    def read(in:MCDataInput, key:Int){}

    def readDesc(in:MCDataInput){}
    def writeDesc(out:MCDataOutput){}

    final def writeStream(switchkey:Int):PacketCustom =
    {
        val stream = new PacketCustom(CoreSPH.channel, CoreSPH.tilePacket)
        stream.writeCoord(new BlockCoord(this)).writeByte(switchkey)
        stream
    }

    final def writeStreamSend(out:PacketCustom)
    {
        out.sendToChunk(worldObj, xCoord/16, zCoord/16)
    }
}

abstract class MultiTileRender(val block:Block)
{
    def renderWorldBlock(r:RenderBlocks, w:IBlockAccess, x:Int, y:Int, z:Int, meta:Int)

    def renderInvBlock(r:RenderBlocks, meta:Int)

    def randomDisplayTick(w:World, x:Int, y:Int, z:Int, r:Random)
}