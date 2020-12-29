package mrtjp.projectred.exploration

import java.lang.{Character => JChar}
import mrtjp.projectred.core.IProxy
import net.minecraft.client.gui.ScreenManager
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}
import net.minecraftforge.fml.event.lifecycle.FMLClientSetupEvent


class ExplorationProxy_server extends IProxy
{
/*    override def preinit()
    {
        blockBarrel = new BlockBarrel
        blockBarrel.setUnlocalizedName("projectred.exploration.barrel")
        ForgeRegistries.BLOCKS.register(blockBarrel.setRegistryName("barrel"))
        ForgeRegistries.ITEMS.register(new ItemBlockCore(blockBarrel).setRegistryName(blockBarrel.getRegistryName))
        blockBarrel.addTile(classOf[TileBarrel], 0)
    }*/

/*
    override def init()
    {
        //World Gen

        //Ruby
        if (Configurator.gen_Ruby)
        {
            val logic = new GenLogicUniform
            logic.name = "pr_ruby"
            logic.resistance = 8+Configurator.gen_Ruby_resistance
            logic.allowRetroGen = Configurator.gen_Ruby_retro
            logic.minY = 12
            logic.maxY = 20
            logic.attempts = 1
            val gen = new WorldGenClusterizer
            gen.cluster = Set(((blockOres, OreDefs.ORERUBY.meta), 1))
            gen.clusterSize = 5
            gen.material = Set((Blocks.STONE, 0))
            logic.gen = gen
            SimpleGenHandler.registerStructure(logic)
        }

        //Sapphire
        if (Configurator.gen_Sapphire)
        {
            val logic = new GenLogicUniform
            logic.name = "pr_sapphire"
            logic.resistance = 8+Configurator.gen_Sapphire_resistance
            logic.allowRetroGen = Configurator.gen_Sapphire_retro
            logic.minY = 12
            logic.maxY = 20
            logic.attempts = 1
            val gen = new WorldGenClusterizer
            gen.cluster = Set(((blockOres, OreDefs.ORESAPPHIRE.meta), 1))
            gen.clusterSize = 5
            gen.material = Set((Blocks.STONE, 0))
            logic.gen = gen
            SimpleGenHandler.registerStructure(logic)
        }

        //Peridot
        if (Configurator.gen_Peridot)
        {
            val logic = new GenLogicUniform
            logic.name = "pr_peridot"
            logic.resistance = 8+Configurator.gen_Peridot_resistance
            logic.allowRetroGen = Configurator.gen_Peridot_retro
            logic.minY = 16
            logic.maxY = 28
            logic.attempts = 2
            val gen = new WorldGenClusterizer
            gen.cluster = Set(((blockOres, OreDefs.OREPERIDOT.meta), 1))
            gen.clusterSize = 5
            gen.material = Set((Blocks.STONE, 0))
            logic.gen = gen
            SimpleGenHandler.registerStructure(logic)
        }

        //Marble
        if (Configurator.gen_MarbleCave)
        {
            val logic = new GenLogicUniform
            logic.name = "pr_marblecave"
            logic.resistance = 4+Configurator.gen_MarbleCave_resistance
            logic.allowRetroGen = Configurator.gen_MarbleCave_retro
            logic.dimensionBlacklist = false
            logic.dimensions = Set(0)
            logic.minY = 32
            logic.maxY = 64
            val gen = new WorldGenCaveReformer
            gen.cluster = Set(((blockDecorativeStone, DecorativeStoneDefs.MARBLE.meta), 1))
            gen.clusterSize = 4096
            gen.material = Set((Blocks.STONE, 0))
            logic.gen = gen
            SimpleGenHandler.registerStructure(logic)
        }

        //Volcano
        if (Configurator.gen_Volcano)
        {
            val logic = new GenLogicUniform
            logic.name = "pr_volcano"
            logic.resistance = 16+Configurator.gen_Volcano_resistance
            logic.allowRetroGen = Configurator.gen_Volcano_retro
            logic.dimensionBlacklist = false
            logic.dimensions = Set(0)
            logic.minY = 0
            logic.maxY = 64
            val gen = new WorldGenVolcanic
            gen.ashCluster = Set(((blockDecorativeStone, DecorativeStoneDefs.BASALT.meta), 1))
            gen.conduitCluster = gen.ashCluster
            gen.liq = (Blocks.LAVA, 0)
            gen.materialStart = Set(gen.liq)
            logic.gen = gen
            SimpleGenHandler.registerStructure(logic)
        }

        //Lily
//        if (Configurator.gen_Lily)
//        {
            //val logic = new GenLogicSurface
            //logic.name = "pr_lily"
            //logic.resistance = 8+Configurator.gen_Lily_resistance
            //logic.allowRetroGen = Configurator.gen_Lily_retro
            //val gen = new WorldGenDecorator
            //gen.cluster = Set(((blockLily, 0), 1))
            //gen.material = Set((Blocks.air, 0))
            //gen.soil = Set((Blocks.grass, 0), (Blocks.dirt, 0))
            //logic.gen = gen
            //SimpleGenHandler.registerStructure(logic)
//        }

        //Copper
        if (Configurator.gen_Copper)
        {
            val logic = new GenLogicUniform
            logic.name = "pr_copper"
            logic.resistance = Configurator.gen_Copper_resistance
            logic.allowRetroGen = Configurator.gen_Copper_retro
            logic.minY = 0
            logic.maxY = 64
            logic.attempts = 16
            val gen = new WorldGenClusterizer
            gen.cluster = Set(((blockOres, OreDefs.ORECOPPER.meta), 1))
            gen.clusterSize = 8
            gen.material = Set((Blocks.STONE, 0))
            logic.gen = gen
            SimpleGenHandler.registerStructure(logic)

        }

        //Tin
        if (Configurator.gen_Tin)
        {
            val logic = new GenLogicUniform
            logic.name = "pr_tin"
            logic.resistance = Configurator.gen_Tin_resistance
            logic.allowRetroGen = Configurator.gen_Tin_retro
            logic.minY = 0
            logic.maxY = 48
            logic.attempts = 8
            val gen = new WorldGenClusterizer
            gen.cluster = Set(((blockOres, OreDefs.ORETIN.meta), 1))
            gen.clusterSize = 8
            gen.material = Set((Blocks.STONE, 0))
            logic.gen = gen
            SimpleGenHandler.registerStructure(logic)

        }

        //Silver
        if (Configurator.gen_Silver)
        {
            val logic = new GenLogicUniform
            logic.name = "pr_silver"
            logic.resistance = Configurator.gen_Silver_resistance
            logic.allowRetroGen = Configurator.gen_Silver_retro
            logic.minY = 0
            logic.maxY = 32
            logic.attempts = 4
            val gen = new WorldGenClusterizer
            gen.cluster = Set(((blockOres, OreDefs.ORESILVER.meta), 1))
            gen.clusterSize = 4
            gen.material = Set((Blocks.STONE, 0))
            logic.gen = gen
            SimpleGenHandler.registerStructure(logic)
        }

        //Electrotine
        if (Configurator.gen_Electrotine)
        {
            val logic = new GenLogicUniform
            logic.name = "pr_electrotine"
            logic.resistance = Configurator.gen_Electrotine_resistance
            logic.allowRetroGen = Configurator.gen_Electrotine_retro
            logic.minY = 0
            logic.maxY = 16
            logic.attempts = 4
            val gen = new WorldGenClusterizer
            gen.cluster = Set(((blockOres, OreDefs.OREELECTROTINE.meta), 1))
            gen.clusterSize = 8
            gen.material = Set((Blocks.STONE, 0))
            logic.gen = gen
            SimpleGenHandler.registerStructure(logic)
        }

        /** Smelting Recipes **/
        GameRegistry.addSmelting(DecorativeStoneDefs.BASALTCOBBLE.makeStack, DecorativeStoneDefs.BASALT.makeStack, 0.1f)
        GameRegistry.addSmelting(OreDefs.ORERUBY.makeStack, PartDefs.RUBY.makeStack, 1)
        GameRegistry.addSmelting(OreDefs.ORESAPPHIRE.makeStack, PartDefs.SAPPHIRE.makeStack, 1)
        GameRegistry.addSmelting(OreDefs.OREPERIDOT.makeStack, PartDefs.PERIDOT.makeStack, 1)
        GameRegistry.addSmelting(OreDefs.ORECOPPER.makeStack, PartDefs.COPPERINGOT.makeStack, 0.7f)
        GameRegistry.addSmelting(OreDefs.ORETIN.makeStack, PartDefs.TININGOT.makeStack, 0.7f)
        GameRegistry.addSmelting(OreDefs.ORESILVER.makeStack, PartDefs.SILVERINGOT.makeStack, 0.8f)
        GameRegistry.addSmelting(OreDefs.OREELECTROTINE.makeStack, PartDefs.ELECTROTINE.makeStack, 0.7f)
    }
*/

/*    override def postinit()
    {
        InvWrapper.register(BarrelInvWrapper)
    }

    private def initOreDict()
    {
        for (i <- 0 until 16)
            OreDictionary.registerOre(ItemBackpack.oreDictionaryVal, new ItemStack(ProjectRedExploration.itemBackpack, 1, i))
    }*/
}

class ExplorationProxy_client extends ExplorationProxy_server
{

    @OnlyIn(Dist.CLIENT)
    override def clientSetup(event: FMLClientSetupEvent) {
        ScreenManager.registerFactory(ExplorationContent.containerBackpack.get(), (c: ContainerBackpack, i, t) => new GuiBackpack(c, i, t))
    }

/*    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        super.preinit()
        ModelLoader.setCustomStateMapper(blockBarrel, new StateMapperBase {
            override protected def getModelResourceLocation(state: IBlockState): ModelResourceLocation = {
                new ModelResourceLocation("projectred:world/barrel", "type=barrel")
            }
        })
        ModelLoader.setCustomModelResourceLocation(Item.getItemFromBlock(blockBarrel), 0, new ModelResourceLocation("projectred:world/barrel", "type=barrel"))
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()

        ClientRegistry.bindTileEntitySpecialRenderer(classOf[TileBarrel], RenderBarrel)
    }*/
}

object ExplorationProxy extends ExplorationProxy_client
