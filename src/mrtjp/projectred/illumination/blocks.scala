package mrtjp.projectred.illumination

import java.util.{Random, List => JList}

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.{BlockCoord, Vector3}
import codechicken.multipart.IRedstoneConnectorBlock
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.core.block.{BlockCore, InstancedBlock, InstancedBlockTile}
import mrtjp.core.color.Colors_old
import mrtjp.core.world.WorldLib
import mrtjp.projectred.ProjectRedIllumination
import mrtjp.projectred.core.libmc.PRLib
import mrtjp.projectred.core.libmc.fx._
import net.minecraft.block.Block
import net.minecraft.block.material.Material
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.EnumCreatureType
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.IIcon
import net.minecraft.world.{IBlockAccess, World}

class BlockLamp extends InstancedBlock("projectred.illumination.lamp", new Material(Material.circuits.getMaterialMapColor)) with IRedstoneConnectorBlock
{
    setBlockBounds(0.0F, 0.0F, 0.0F, 1.0F, 1.0F, 1.0F)
    setHardness(0.5F)
    setCreativeTab(ProjectRedIllumination.tabLighting)

    override def renderAsNormalBlock = true

    override def getRenderType = 0

    override def isOpaqueCube = true

    override def isBlockNormalCube = true

    override def getSubBlocks(b:Item, tab:CreativeTabs, list:JList[_]) =
    {
        for (i <- 0 until 32)
            list.asInstanceOf[JList[ItemStack]].add(
                new ItemStack(ProjectRedIllumination.blockLamp, 1, i))
    }

    override def canCreatureSpawn(t:EnumCreatureType, w:IBlockAccess, x:Int, y:Int, z:Int) = false

    override def canConnectRedstone(w:IBlockAccess, x:Int, y:Int, z:Int, s:Int) = true

    override def canProvidePower = true

    override def registerBlockIcons(reg:IIconRegister)
    {
        val onIcons = Vector.newBuilder[IIcon]
        val offIcons = Vector.newBuilder[IIcon]
        for (i <- 0 until 16)
        {
            onIcons += reg.registerIcon("projectred:lights/lampon/"+i)
            offIcons += reg.registerIcon("projectred:lights/lampoff/"+i)
        }
        BlockLamp.on = onIcons.result()
        BlockLamp.off = offIcons.result()
    }

    override def getIcon(w:IBlockAccess, x:Int, y:Int, z:Int, side:Int) =
    {
        val t = WorldLib.getTileEntity(w, x, y, z, classOf[TileLamp])
        if (t != null) if (t.isOn) BlockLamp.on(t.getColor) else BlockLamp.off(t.getColor)
        else super.getIcon(w, x, y, z, side)
    }

    override def getIcon(side:Int, meta:Int) = if (meta>15) BlockLamp.on(meta%16) else BlockLamp.off(meta)

    def getConnectionMask(world:IBlockAccess, x:Int, y:Int, z:Int, side:Int) = 0x1F
    def weakPowerLevel(world:IBlockAccess, x:Int, y:Int, z:Int, side:Int, mask:Int) = 0
}

object BlockLamp
{
    var on:Vector[IIcon] = null
    var off:Vector[IIcon] = null
}

class TileLamp extends InstancedBlockTile with ILight
{
    var inverted = false
    var powered = false

    override def getBlock = ProjectRedIllumination.blockLamp
    override def getMetaData = getColor+(if (inverted) 16 else 0)

    override def onBlockPlaced(side:Int, meta:Int, player:EntityPlayer, stack:ItemStack, hit:Vector3)
    {
        inverted = meta > 15
    }

    override def getLightValue = if (inverted != powered)
        IlluminationProxy.getLightValue(getColor, 15) else 0

    override def onNeighborChange(b:Block)
    {
        if (!world.isRemote) scheduleTick(2)
    }

    def checkPower =
    {
        worldObj.isBlockIndirectlyGettingPowered(xCoord, yCoord, zCoord) ||
            worldObj.getBlockPowerInput(xCoord, yCoord, zCoord) != 0
    }

    override def onScheduledTick()
    {
        val old = powered
        powered = checkPower
        if (old != powered) markDescUpdate()
    }

    override def readDesc(in:MCDataInput)
    {
        inverted = in.readBoolean()
        powered = in.readBoolean()
        markRender()
        markLight()
    }

    override def writeDesc(out:MCDataOutput)
    {
        out.writeBoolean(inverted).writeBoolean(powered)
    }

    override def load(tag:NBTTagCompound)
    {
        inverted = tag.getBoolean("inv")
        powered = tag.getBoolean("pow")
    }

    override def save(tag:NBTTagCompound)
    {
        tag.setBoolean("inv", inverted)
        tag.setBoolean("pow", powered)
    }

    override def getColor = getBlockMetadata%16
    override def isOn = inverted != powered
}

class BlockAirousLight extends BlockCore("projectred.illumination.airousLight", Material.air)
{
    override def isAir(world:IBlockAccess, x:Int, y:Int, z:Int) = true
    override def getRenderType = -1
    override def getCollisionBoundingBoxFromPool(w:World, x:Int, y:Int, z:Int) = null
    override def isOpaqueCube = false
    override def canCollideCheck(meta:Int, click:Boolean) = false
    override def dropBlockAsItemWithChance(w:World, x:Int, y:Int, z:Int, a:Int, b:Float, c:Int){}

    override def createTileEntity(w:World, meta:Int) = new TileAirousLight
    override def hasTileEntity(meta:Int) = true

    @SideOnly(Side.CLIENT)
    override def randomDisplayTick(world:World, x:Int, y:Int, z:Int, rand:Random)
    {
        if (rand.nextInt(10) > 0) return
        val color = world.getBlockMetadata(x, y, z)%16

        val dist = 3
        val dx = x+rand.nextInt(dist)-rand.nextInt(dist)
        val dy = y+rand.nextInt(dist)-rand.nextInt(dist)
        val dz = z+rand.nextInt(dist)-rand.nextInt(dist)
        val ex = dx+rand.nextInt(dist)-rand.nextInt(dist)
        val ey = dy+rand.nextInt(dist)-rand.nextInt(dist)
        val ez = dz+rand.nextInt(dist)-rand.nextInt(dist)

        val c = ParticleManagement.instance.spawn(world, "ember", dx, dy, dz)
        if (c != null)
        {
            val orbit = new ParticleLogicOrbitPoint(new Vector3(ex, ey, ez))
            orbit.setOrbitSpeed(0.5f*rand.nextDouble).setTargetDistance(0.3D)
            orbit.setShrinkingOrbit(0.01, 0.01).setPriority(2)
            val scale = new ParticleLogicScale
            scale.setRate(-0.001F, -0.0001F*rand.nextFloat)
            scale.setTerminate(true)

            val iconshift = ParticleLogicIconShift.fluttering
            val approach = new ParticleLogicApproachPoint(new Vector3(ex, ey, ez), 0.03f, 0.5f)
            approach.setFinal(true)

            c.setIgnoreMaxAge(true)
            c.setScale(0.05f+0.02f*rand.nextFloat)
            c.setPRColor(Colors_old.get(color))
            c += orbit
            c += scale
            c += iconshift
            c += approach
        }
    }

    override def getLightValue(world:IBlockAccess, x:Int, y:Int, z:Int) =
    {
        val te = WorldLib.getTileEntity(world, x, y, z, classOf[TileAirousLight])
        if (te != null) te.lightVal else 0
    }
}

class TileAirousLight extends TileEntity
{
    private val source = new BlockCoord(-1, -1, -1)
    private var sourcePartID = -1
    private var color = -1
    private var delay = 100

    override def updateEntity()
    {
        if (!worldObj.isRemote)
        {
            if ({delay -= 1; delay} > 0) return
            delay = worldObj.rand.nextInt(100)

            val light = getLight
            if (light == null || !light.isOn || light.getColor != color) worldObj.setBlockToAir(xCoord, yCoord, zCoord)
        }
    }

    private def getLight:ILight =
    {
        if (sourcePartID > -1)
        {
            PRLib.getMultiPart(worldObj, source, sourcePartID) match
            {
                case light:ILight => return light
                case _ =>
            }
        }
        WorldLib.getTileEntity(worldObj, source, classOf[ILight])
    }

    def setSource(x:Int, y:Int, z:Int, color:Int, partID:Int) =
    {
        source.set(x, y, z)
        this.color = color
        sourcePartID = partID
    }

    override def readFromNBT(tag:NBTTagCompound)
    {
        super.readFromNBT(tag)
        val x = tag.getInteger("sX")
        val y = tag.getInteger("sY")
        val z = tag.getInteger("sZ")
        source.set(x, y, z)
        sourcePartID = tag.getByte("spID")
        color = tag.getByte("col")
    }

    override def writeToNBT(tag:NBTTagCompound)
    {
        super.writeToNBT(tag)
        tag.setInteger("sX", source.x)
        tag.setInteger("sY", source.y)
        tag.setInteger("sX", source.z)
        tag.setByte("spID", sourcePartID.asInstanceOf[Byte])
        tag.setByte("col", color.asInstanceOf[Byte])
    }

    def lightVal = IlluminationProxy.getLightValue(color, 15)
}