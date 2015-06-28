package mrtjp.projectred.exploration

import java.util
import java.util.Random

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.vec.{Cuboid6, Vector3}
import cpw.mods.fml.common.registry.GameRegistry
import mrtjp.core.block._
import mrtjp.core.color.Colors
import mrtjp.core.math.MathLib
import mrtjp.core.world.WorldLib
import mrtjp.projectred.ProjectRedExploration
import mrtjp.projectred.core.PartDefs
import net.minecraft.block.material.Material
import net.minecraft.block.{Block, BlockWall}
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.Entity
import net.minecraft.init.Blocks
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.nbt.NBTTagCompound
import net.minecraft.util.IIcon
import net.minecraft.world.{IBlockAccess, World}
import net.minecraftforge.common.EnumPlantType

import scala.collection.mutable.ListBuffer

class BlockOre extends BlockCore("projectred.exploration.ore", Material.rock)
{
    setHardness(3.0F)
    setResistance(5.0F)
    setCreativeTab(ProjectRedExploration.tabExploration)

    override def getDrops(world:World, x:Int, y:Int, z:Int, meta:Int, fortune:Int) =
    {
        if (OreDefs.isDefinedAt(meta))
        {
            val odef = OreDefs(meta)
            import odef._
            var count = world.rand.nextInt(fortune+max)
            if (count > max) count = max
            if (count < min) count = min

            val array = new util.ArrayList[ItemStack]()
            if (hasDrop) array.add(makeDropStack(count))
            else array.add(makeStack(count))
            array
        }
        else new util.ArrayList[ItemStack]()
    }

    override def getExpDrop(world:IBlockAccess, meta:Int, fortune:Int) =
    {
        if (OreDefs.isDefinedAt(meta))
        {
            val odef = OreDefs(meta)
            MathLib.randomFromIntRange(odef.minXP to odef.maxXP)
        }
        else 0
    }

    override def getIcon(side:Int, meta:Int) =
    {
        if (OreDefs.isDefinedAt(meta)) OreDefs(meta).icon
        else null
    }

    override def damageDropped(meta:Int) = meta

    override def registerBlockIcons(reg:IIconRegister)
    {
        for (o <- OreDefs.values) o.registerIcon(reg)
    }

    override def getSubBlocks(item:Item, tab:CreativeTabs, list:util.List[_])
    {
        for (o <- OreDefs.values)
            list.asInstanceOf[util.List[ItemStack]].add(o.makeStack)
    }
}

object OreDefs extends BlockDefinition
{
    override type EnumVal = OreVal
    override def getBlock = ProjectRedExploration.blockOres

    val ORERUBY = new OreVal("ruby_ore", 2, PartDefs.RUBY.makeStack, 1, 4, 1, 8)
    val ORESAPPHIRE = new OreVal("sapphire_ore", 2, PartDefs.SAPPHIRE.makeStack, 1, 4, 1, 8)
    val OREPERIDOT = new OreVal("peridot_ore", 2, PartDefs.PERIDOT.makeStack, 1, 4, 1, 8)

    val ORECOPPER = new OreVal("copper_ore", 1, null, 1, 1, 0, 0)
    val ORETIN = new OreVal("tin_ore", 1, null, 1, 1, 0, 0)
    val ORESILVER = new OreVal("silver_ore", 2, null, 1, 1, 0, 0)
    val OREELECTROTINE = new OreVal("electrotine_ore", 2, PartDefs.ELECTROTINE.makeStack, 1, 8, 1, 8)

    class OreVal(iconName:String, val harvest:Int, val drop:ItemStack, val min:Int, val max:Int, val minXP:Int, val maxXP:Int) extends BlockDef
    {
        var icon:IIcon = null
        def registerIcon(reg:IIconRegister)
        {
            icon = reg.registerIcon("projectred:world/"+iconName)
        }

        def hasDrop = drop != null

        def makeDropStack:ItemStack = makeDropStack(1)
        def makeDropStack(i:Int) = new ItemStack(drop.getItem, i, drop.getItemDamage)
    }
}

class BlockDecoratives extends BlockCore("projectred.exploration.stone", Material.rock)
{
    setHardness(3.0F)
    setResistance(10.0F)
    setCreativeTab(ProjectRedExploration.tabExploration)

    override def getBlockHardness(w:World, x:Int, y:Int, z:Int) =
    {
        val meta = w.getBlockMetadata(x, y, z)
        if (DecorativeStoneDefs.isDefinedAt(meta)) DecorativeStoneDefs(meta).hardness
        else super.getBlockHardness(w, x, y, z)
    }

    override def getExplosionResistance(e:Entity, w:World, x:Int, y:Int, z:Int, ex:Double, ey:Double, ez:Double) =
    {
        val meta = w.getBlockMetadata(x, y, z)
        if (DecorativeStoneDefs.isDefinedAt(meta)) DecorativeStoneDefs(meta).explosion
        else super.getExplosionResistance(e, w, x, y, z, ex, ey, ez)
    }

    override def getIcon(side:Int, meta:Int) =
    {
        if (DecorativeStoneDefs.isDefinedAt(meta)) DecorativeStoneDefs(meta).icon
        else super.getIcon(side, meta)
    }

    override def getDrops(world:World, x:Int, y:Int, z:Int, meta:Int, fortune:Int) =
    {
        if (DecorativeStoneDefs.isDefinedAt(meta))
        {
            val ddef = DecorativeStoneDefs(meta)
            val array = new util.ArrayList[ItemStack]()
            if (ddef.hasDrop) array.add(ddef.makeDropStack)
            else array.add(ddef.makeStack)
            array
        }
        else new util.ArrayList[ItemStack]()
    }

    override def registerBlockIcons(reg:IIconRegister) =
    {
        for (s <- DecorativeStoneDefs.values)
            s.registerIcon(reg)
    }

    override def damageDropped(meta:Int) = meta

    override def getSubBlocks(item:Item, tab:CreativeTabs, list:util.List[_])
    {
        for (s <- DecorativeStoneDefs.values)
            list.asInstanceOf[util.List[ItemStack]].add(s.makeStack)
    }
}

object DecorativeStoneDefs extends BlockDefinition
{
    override type EnumVal = StoneVal
    override def getBlock = ProjectRedExploration.blockDecoratives

    val MARBLE = new StoneVal("marble", 2, 1.0F, 14.0F, null)
    val MARBLEBRICK = new StoneVal("marble_brick", 2, 1.0F, 14.0F, null)
    val BASALTCOBBLE = new StoneVal("basalt_cobble", 2, 2.5F, 14.0F, null)
    val BASALT = new StoneVal("basalt", 2, 2.5F, 16, BASALTCOBBLE.makeStack)
    val BASALTBRICK = new StoneVal("basalt_brick", 2, 2.5F, 20, null)
    val RUBYBLOCK = new StoneVal("ruby_block", 2, 5.0F, 10.0F, null)
    val SAPPHIREBLOCK = new StoneVal("sapphire_block", 2, 5.0F, 10.0F, null)
    val PERIDOTBLOCK = new StoneVal("peridot_block", 2, 5.0F, 10.0F, null)
    val COPPERBLOCK = new StoneVal("copper_block", 2, 5.0F, 10.0F, null)
    val TINBLOCK = new StoneVal("tin_block", 2, 5.0F, 10.0F, null)
    val SILVERBLOCK = new StoneVal("silver_block", 2, 5.0F, 10.0F, null)
    val ELECTROTINEBLOCK = new StoneVal("electrotine_block", 2, 5.0F, 10.0F, null)

    class StoneVal(iconName:String, val harvest:Int, val hardness:Float, val explosion:Float, val drop:ItemStack) extends BlockDef
    {
        var icon:IIcon = null
        def registerIcon(reg:IIconRegister)
        {
            icon = reg.registerIcon("projectred:world/"+iconName)
        }

        def hasDrop = drop != null

        def makeDropStack:ItemStack = makeDropStack(1)
        def makeDropStack(i:Int) = new ItemStack(drop.getItem, i, drop.getItemDamage)
    }
}

class BlockDecorativeWalls extends BlockWall(Blocks.stone)
{
    setBlockName("projectred.exploration.stonewalls")
    setCreativeTab(ProjectRedExploration.tabExploration)

    //block registration
    GameRegistry.registerBlock(this, classOf[ItemBlockCore], "projectred.exploration.stonewalls")

    override def getIcon(side:Int, meta:Int) =
    {
        if (DecorativeStoneDefs.isDefinedAt(meta)) DecorativeStoneDefs(meta).icon
        else super.getIcon(side, meta)
    }

    override def getSubBlocks(item:Item, tab:CreativeTabs, list:util.List[_])
    {
        for (s <- DecorativeStoneDefs.values)
            list.asInstanceOf[util.List[ItemStack]].add(new ItemStack(ProjectRedExploration.blockDecorativeWalls, 1, s.meta))
    }

    override def canConnectWallTo(w:IBlockAccess, x:Int, y:Int, z:Int) =
    {
        val b = w.getBlock(x, y, z)
        if (b != this && b != Blocks.fence_gate) b != null && b.getMaterial.isOpaque && b.renderAsNormalBlock && b.getMaterial != Material.gourd
        else true
    }

    override def canPlaceTorchOnTop(w:World, x:Int, y:Int, z:Int) =
        super.canPlaceTorchOnTop(w, x, y, z) || w.getBlock(x, y, z) == this
}

class BlockLily extends InstancedBlock("projectred.exploration.lily", Material.plants) with TPlantBlock
{
    val soil = Seq(Blocks.grass, Blocks.dirt)

    override def initialCanStay(w:World, x:Int, y:Int, z:Int) = soil.contains(w.getBlock(x, y-1, z))

    override def canBlockStay(w:World, x:Int, y:Int, z:Int) = initialCanStay(w, x, y, z)

    override def getPlantType(w:IBlockAccess, x:Int, y:Int, z:Int) = EnumPlantType.Plains
}

class TileLily extends InstancedBlockTile with TPlantTile
{
    // mmmm gggg
    var phase:Byte = 0
    var pollinated = false

    def meta = phase>>4&0xF
    def setMeta(m:Int){ phase = (phase&0xF|m<<4).toByte }
    def growth = phase&0xF
    def setGrowth(g:Int){ phase = (phase&0xF0|g).toByte }

    override def getBlock = ProjectRedExploration.blockLily

    override def save(tag:NBTTagCompound)
    {
        super.save(tag)
        tag.setByte("phase", phase)
    }

    private var isNewWorldgen = true
    override def load(tag:NBTTagCompound)
    {
        super.load(tag)
        phase = tag.getByte("phase")
        isNewWorldgen = false
    }

    override def writeDesc(out:MCDataOutput)
    {
        super.writeDesc(out)
        out.writeByte(phase)
    }

    override def readDesc(in:MCDataInput)
    {
        super.readDesc(in)
        phase = in.readByte()
    }

    override def read(in:MCDataInput, key:Int) = key match
    {
        case 1 =>
            phase = in.readByte()
            markRender()
        case _ => super.read(in, key)
    }

    def sendPhaseUpdate()
    {
        writeStream(1).writeByte(phase).sendToChunk()
    }

    def setupPlanted(m:Int)
    {
        setMeta(m)
        setGrowth(0)
    }

    override def validate()
    {
        super.validate()
        if (!world.isRemote && isNewWorldgen)
        {
            setGrowth(7)
            setMeta(MathLib.weightedRandom(TileLily.rarity))
        }
    }

    override def onNeighborChange(b:Block)
    {
        super.onNeighborChange(b)
        dropIfCantStay
    }

    override def getBlockBounds = TileLily.bounds(growth)

    override def addHarvestContents(ist:ListBuffer[ItemStack])
    {
        if (growth == 7)
        {
            val count = MathLib.weightedRandom(Seq(
                (0, 10),
                (1, 55),
                (2, 30),
                (3, 5)
            ))
            if (count > 0) ist += new ItemStack(ProjectRedExploration.itemLilySeed, count, meta)
        }
        else
        {
            if (world.rand.nextDouble() < growth/7.0D)
                ist += new ItemStack(ProjectRedExploration.itemLilySeed, 1, meta)
        }
    }

    override def randomTick(rand:Random)
    {
        if (!world.isRemote)
        {
            var updated = tickGrowth(rand)
            updated |= tickPollination(rand)
            if (updated)
            {
                markDirty()
                sendPhaseUpdate()
            }
        }
    }

    def tickPollination(rand:Random):Boolean =
    {
        if (growth == 7 || pollinated) return false

        val delta = (WorldLib.getWindSpeed(world, x, y, z)*10).toInt
        val dx = MathLib.randomFromIntRange(-delta to delta, rand)
        val dy = MathLib.randomFromIntRange(-delta to delta, rand)
        val dz = MathLib.randomFromIntRange(-delta to delta, rand)

        world.getTileEntity(x+dx, y+dy, z+dz) match
        {
            case p:TileLily if p.pollinated =>
                val mix = Colors(meta).mcMix(Colors(p.meta))
                if (mix != null)
                {
                    setMeta(mix.ordinal)
                    pollinated = true
                    return true
                }
            case _ =>
        }
        false
    }

    override def applyBonemeal() =
    {
        if (growth < 7)
        {
            if (!world.isRemote)
            {
                setGrowth(growth+1)
                sendPhaseUpdate()
            }
            true
        }
        else false
    }

    def tickGrowth(rand:Random):Boolean =
    {
        if (growth == 7) return false

        //TODO refine temporary growth algorithm
        if (rand.nextInt(2) != 0) return false

        var growthChance = 1.0D
        growthChance *= getPlantLight
        growthChance *= getSoilSaturation

        if (rand.nextDouble() < growthChance)
        {
            setGrowth(growth+1)
            true
        }
        else false
    }

    def getPlantLight =
        math.max(WorldLib.getSkyLightValue(world, x, y, z)/15.0D*0.99D,
            WorldLib.getBlockLightValue(world, x, y, z)/15.0D*0.66D)

    def getSoilSaturation:Double =
    {
        var sat = 0.0D
        for (dx <- -4 to 4) for (dy <- -1 to 1) for (dz <- -4 to 4)
            if (world.getBlock(x+dx, y+dy, z+dz) == Blocks.water)
                sat += (1.0D-new Vector3(dx, 0, dz).mag/5.66D)*0.25D

        import net.minecraftforge.common.BiomeDictionary.Type._
        import net.minecraftforge.common.BiomeDictionary._
        val tags = getTypesForBiome(world.getBiomeGenForCoords(x, z)).toSeq
        val satMap = Map(
            SPARSE ->   -0.02D,
            HOT ->      -0.05D,
            DRY ->      -0.15D,
            DEAD ->     -0.04D,
            SANDY ->    -0.06D,
            WASTELAND -> -0.09D,
            NETHER ->   -0.11D,
            END ->      -0.03D,

            DENSE ->    0.02D,
            WET ->      0.15D,
            LUSH ->     0.04D,
            OCEAN ->    0.13D,
            RIVER ->    0.09D,
            WATER ->    0.16D,
            SWAMP ->    0.01D,
            BEACH ->    0.08D
        ).withDefaultValue(0.0D)

        tags.foreach(t => sat += satMap(t))

        if (world.isRaining) sat += 0.50D*world.rainingStrength
        else if (world.prevRainingStrength > 0) sat += 0.20D*world.prevRainingStrength

        math.min(1.0D, math.max(sat, 0.0D))
    }
}

object TileLily
{
    val bounds =
    {
        val b = Seq.newBuilder[Cuboid6]
        val gains = Seq(2/16D, 2/16D, 2/16D, 2/16D, 2/16D, 0.0D, 2/16D, 2/16D)
        for (i <- 0 until 8) b += new Cuboid6(0.5f-0.2f, 0.0f, 0.5f-0.2f, 0.5f+0.2f, gains.take(i+1).sum, 0.5f+0.2f)
        b.result()
    }

    import mrtjp.core.color.Colors._
    val rarity = Seq(
        WHITE.ordinal   -> 17,
        YELLOW.ordinal  -> 25,
        BLUE.ordinal    -> 13,
        RED.ordinal     -> 35,
        BLACK.ordinal   -> 10
    )
}

