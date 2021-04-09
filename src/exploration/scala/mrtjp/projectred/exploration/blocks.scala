package mrtjp.projectred.exploration

import mrtjp.core.math.MathLib
import net.minecraft.block.{Block, BlockState}
import net.minecraft.util.math.BlockPos
import net.minecraft.world.IWorldReader

class BlockOre(properties: Block.Properties, minXP: Int = 0, maxXP: Int = 0) extends Block(properties) {
    override def getExpDrop(state: BlockState, world: IWorldReader, pos: BlockPos, fortune: Int, silktouch: Int) =
        if (silktouch == 0) MathLib.randomFromIntRange(minXP to maxXP) else 0
}

/*class BlockLily extends MultiTileBlock("projectred.exploration.lily", "lily", Material.PLANTS) with TPlantBlock
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
            case p:TileLily if p.growth == 7 =>
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
}*/

