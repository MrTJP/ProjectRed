package mrtjp.projectred.exploration

import java.lang.{Character => JChar}

import codechicken.lib.colour.EnumColour
import codechicken.lib.model.ModelRegistryHelper
import codechicken.microblock.BlockMicroMaterial
import mrtjp.core.block.ItemBlockCore
import mrtjp.core.gui.GuiHandler
import mrtjp.core.inventory.InvWrapper
import mrtjp.core.item.ItemDefinition
import mrtjp.core.world._
import mrtjp.projectred.ProjectRedExploration
import mrtjp.projectred.ProjectRedExploration._
import mrtjp.projectred.core.libmc.recipe._
import mrtjp.projectred.core.{Configurator, IProxy, PartDefs, ShapelessOreNBTRecipe}
import net.minecraft.block.state.IBlockState
import net.minecraft.client.renderer.ItemMeshDefinition
import net.minecraft.client.renderer.block.model.ModelResourceLocation
import net.minecraft.client.renderer.block.statemap.StateMapperBase
import net.minecraft.init.{Blocks, Items, SoundEvents}
import net.minecraft.inventory.EntityEquipmentSlot
import net.minecraft.item.{Item, ItemStack}
import net.minecraftforge.client.model.ModelLoader
import net.minecraftforge.common.util.EnumHelper
import net.minecraftforge.fml.client.registry.ClientRegistry
import net.minecraftforge.fml.common.registry.GameRegistry
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import net.minecraftforge.oredict.{OreDictionary, ShapedOreRecipe}

class ExplorationProxy_server extends IProxy
{
    val guiIDBackpack = 1

    override def preinit()
    {
        itemWoolGin = new ItemWoolGin
        itemWoolGin.setUnlocalizedName("projectred.exploration.woolGin")
        GameRegistry.register(itemWoolGin.setRegistryName("wool_gin"))

        itemBackpack = new ItemBackpack
        itemBackpack.setUnlocalizedName("projectred.exploration.backpack")
        GameRegistry.register(itemBackpack.setRegistryName("backpack"))

        itemAthame = new ItemAthame
        itemAthame.setUnlocalizedName("projectred.exploration.athame")
        GameRegistry.register(itemAthame.setRegistryName("athame"))

        blockOres = new BlockOre
        blockOres.setUnlocalizedName("projectred.exploration.ore")
        GameRegistry.register(blockOres.setRegistryName("ore"))
        GameRegistry.register(new ItemBlockCore(blockOres).setRegistryName(blockOres.getRegistryName))
        for (o <- OreDefs.values) blockOres.setHarvestLevel("pickaxe", o.harvest, blockOres.getStateFromMeta(o.meta))

        blockDecorativeStone = new BlockDecorativeStone
        blockDecorativeStone.setUnlocalizedName("projectred.exploration.stone")
        GameRegistry.register(blockDecorativeStone.setRegistryName("stone"))
        GameRegistry.register(new ItemBlockCore(blockDecorativeStone).setRegistryName(blockDecorativeStone.getRegistryName))
        for (b <- DecorativeStoneDefs.values) {
            blockDecorativeStone.setHarvestLevel("pickaxe", b.harvest, blockDecorativeStone.getStateFromMeta(b.meta))
            BlockMicroMaterial.createAndRegister(blockDecorativeStone.getStateFromMeta(b.meta)) //Register as microblocks
        }

        blockDecorativeWall = new BlockDecorativeWall
        blockDecorativeWall.setUnlocalizedName("projectred.exploration.stoneWall")
        GameRegistry.register(blockDecorativeWall.setRegistryName("stone_wall"))
        GameRegistry.register(new ItemBlockCore(blockDecorativeWall).setRegistryName(blockDecorativeWall.getRegistryName))

        blockBarrel = new BlockBarrel
        blockBarrel.setUnlocalizedName("projectred.exploration.barrel")
        GameRegistry.register(blockBarrel.setRegistryName("barrel"))
        GameRegistry.register(new ItemBlockCore(blockBarrel).setRegistryName(blockBarrel.getRegistryName))
        blockBarrel.addTile(classOf[TileBarrel], 0)

        toolMaterialRuby        = EnumHelper.addToolMaterial("RUBY",      2, 512, 8.00F, 3.00F, 10)
        toolMaterialSapphire    = EnumHelper.addToolMaterial("SAPPHIRE",  2, 512, 8.00F, 3.00F, 10)
        toolMaterialPeridot     = EnumHelper.addToolMaterial("PERIDOT",   2, 512, 7.75F, 2.75F, 14)

        armorMatrialRuby        = EnumHelper.addArmorMaterial("RUBY",     "ruby",     16, Array(3, 8, 6, 3), 10, SoundEvents.ITEM_ARMOR_EQUIP_DIAMOND,  8)
        armorMatrialSapphire    = EnumHelper.addArmorMaterial("SAPPHIRE", "sapphire", 16, Array(3, 8, 6, 3), 10, SoundEvents.ITEM_ARMOR_EQUIP_DIAMOND,  8)
        armorMatrialPeridot     = EnumHelper.addArmorMaterial("PERIDOT",  "peridot",  14, Array(3, 8, 6, 3), 14, SoundEvents.ITEM_ARMOR_EQUIP_DIAMOND, 10)

        /** Following items are self-registering **/

        itemRubyAxe = new ItemGemAxe(ToolDefs.RUBYAXE, 8f, -3.0F)
        itemSapphireAxe = new ItemGemAxe(ToolDefs.SAPPHIREAXE, 8f, -3.0F)
        itemPeridotAxe = new ItemGemAxe(ToolDefs.PERIDOTAXE, 8f, -3.0F)

        itemRubyHoe = new ItemGemHoe(ToolDefs.RUBYHOE)
        itemSapphireHoe = new ItemGemHoe(ToolDefs.SAPPHIREHOE)
        itemPeridotHoe = new ItemGemHoe(ToolDefs.PERIDOTHOE)

        itemRubyPickaxe = new ItemGemPickaxe(ToolDefs.RUBYPICKAXE)
        itemSapphirePickaxe = new ItemGemPickaxe(ToolDefs.SAPPHIREPICKAXE)
        itemPeridotPickaxe = new ItemGemPickaxe(ToolDefs.PERIDOTPICKAXE)

        itemRubyShovel = new ItemGemShovel(ToolDefs.RUBYSHOVEL)
        itemSapphireShovel = new ItemGemShovel(ToolDefs.SAPPHIRESHOVEL)
        itemPeridotShovel = new ItemGemShovel(ToolDefs.PERIDOTSHOVEL)

        itemRubySword = new ItemGemSword(ToolDefs.RUBYSWORD)
        itemSapphireSword = new ItemGemSword(ToolDefs.SAPPHIRESWORD)
        itemPeridotSword = new ItemGemSword(ToolDefs.PERIDOTSWORD)

        itemGoldSaw = new ItemGemSaw(ToolDefs.GOLDSAW)
        itemRubySaw = new ItemGemSaw(ToolDefs.RUBYSAW)
        itemSapphireSaw = new ItemGemSaw(ToolDefs.SAPPHIRESAW)
        itemPeridotSaw = new ItemGemSaw(ToolDefs.PERIDOTSAW)

        itemWoodSickle = new ItemGemSickle(ToolDefs.WOODSICKLE)
        itemStoneSickle = new ItemGemSickle(ToolDefs.STONESICKLE)
        itemIronSickle = new ItemGemSickle(ToolDefs.IRONSICKLE)
        itemGoldSickle = new ItemGemSickle(ToolDefs.GOLDSICKLE)
        itemRubySickle = new ItemGemSickle(ToolDefs.RUBYSICKLE)
        itemSapphireSickle = new ItemGemSickle(ToolDefs.SAPPHIRESICKLE)
        itemPeridotSickle = new ItemGemSickle(ToolDefs.PERIDOTSICKLE)
        itemDiamondSickle = new ItemGemSickle(ToolDefs.DIAMONDSICKLE)

        itemRubyHelmet = new ItemGemArmor(ArmorDefs.RUBYHELMET, EntityEquipmentSlot.HEAD)
        itemRubyChestplate = new ItemGemArmor(ArmorDefs.RUBYCHESTPLATE, EntityEquipmentSlot.CHEST)
        itemRubyLeggings = new ItemGemArmor(ArmorDefs.RUBYLEGGINGS, EntityEquipmentSlot.LEGS)
        itemRubyBoots = new ItemGemArmor(ArmorDefs.RUBYBOOTS, EntityEquipmentSlot.FEET)

        itemSapphireHelmet = new ItemGemArmor(ArmorDefs.SAPPHIREHELMET, EntityEquipmentSlot.HEAD)
        itemSapphireChestplate = new ItemGemArmor(ArmorDefs.SAPPHIRECHESTPLATE, EntityEquipmentSlot.CHEST)
        itemSapphireLeggings = new ItemGemArmor(ArmorDefs.SAPPHIRELEGGINGS, EntityEquipmentSlot.LEGS)
        itemSapphireBoots = new ItemGemArmor(ArmorDefs.SAPPHIREBOOTS, EntityEquipmentSlot.FEET)

        itemPeridotHelmet = new ItemGemArmor(ArmorDefs.PERIDOTHELMET, EntityEquipmentSlot.HEAD)
        itemPeridotChestplate = new ItemGemArmor(ArmorDefs.PERIDOTCHESTPLATE, EntityEquipmentSlot.CHEST)
        itemPeridotLeggings = new ItemGemArmor(ArmorDefs.PERIDOTLEGGINGS, EntityEquipmentSlot.LEGS)
        itemPeridotBoots = new ItemGemArmor(ArmorDefs.PERIDOTBOOTS, EntityEquipmentSlot.FEET)
    }

    override def init()
    {
        ExplorationRecipes.initRecipes()

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
        if (Configurator.gen_Lily)
        {
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
        }

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
    }

    override def postinit()
    {
//        if (Configurator.gen_SpreadingMoss)
//            BlockUpdateHandler.register(MossSpreadHandler)

        InvWrapper.register(BarrelInvWrapper)
    }

    override def version = "@VERSION@"
    override def build = "@BUILD_NUMBER@"
}

class ExplorationProxy_client extends ExplorationProxy_server
{
    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        super.preinit()
        ModelLoader.setCustomStateMapper(blockOres, new StateMapperBase {
            override protected def getModelResourceLocation(state: IBlockState): ModelResourceLocation = {
                new ModelResourceLocation("projectred:exploration/ore", "type=" + state.getValue(BlockProperties.ORE_TYPES))
            }
        })
        ModelLoader.setCustomStateMapper(blockDecorativeStone, new StateMapperBase {
            override protected def getModelResourceLocation(state: IBlockState): ModelResourceLocation = {
                new ModelResourceLocation("projectred:exploration/deceratives", "type=" + state.getValue(BlockProperties.ORE_TYPES))
            }
        })
        registerItemModelTypes(Item.getItemFromBlock(blockOres), "projectred:exploration/ore", OreDefs)
        registerItemModelTypes(Item.getItemFromBlock(blockDecorativeStone), "projectred:exploration/deceratives", DecorativeStoneDefs)
        for (v <- DecorativeStoneDefs.values) {
            val modelloc = new ModelResourceLocation("projectred:exploration/wall", "type=" + v.getVariantName + ",up=true,east=true,west=true")
            ModelLoader.setCustomModelResourceLocation(Item.getItemFromBlock(blockDecorativeWall), v.meta, modelloc)
        }
        ModelLoader.setCustomStateMapper(blockDecorativeWall, new StateMapperBase {
            override protected def getModelResourceLocation(state: IBlockState): ModelResourceLocation = {
                import java.lang.{Boolean => JBool}

                import mrtjp.projectred.exploration.BlockProperties._
                import net.minecraft.block.BlockWall._
                def parseLocation(state :IBlockState):String = {
                    val t = "type=" + state.getValue(STONE_TYPES)
                    val u = "up=" + JBool.toString(state.getValue(UP))
                    val n = "north=" + JBool.toString(state.getValue(NORTH))
                    val s = "south=" + JBool.toString(state.getValue(SOUTH))
                    val e = "east=" + JBool.toString(state.getValue(EAST))
                    val w = "west=" + JBool.toString(state.getValue(WEST))
                    t + "," + u + "," + n + "," + s + "," + e + "," + w
                }
                new ModelResourceLocation("projectred:exploration/wall", parseLocation(state))
            }
        })
        ModelLoader.setCustomStateMapper(blockBarrel, new StateMapperBase {
            override protected def getModelResourceLocation(state: IBlockState): ModelResourceLocation = {
                new ModelResourceLocation("projectred:exploration/barrel", "type=barrel")
            }
        })
        ModelLoader.setCustomModelResourceLocation(Item.getItemFromBlock(blockBarrel), 0, new ModelResourceLocation("projectred:exploration/barrel", "type=barrel"))

        registerModelType(itemWoolGin, "projectred:exploration/items", "wool_gin")
        registerModelType(itemAthame, "projectred:exploration/items", "athame")
        for (i <- 0 until 16) {
            registerModelType(itemBackpack, i, "projectred:exploration/items", "backpack_" + i)
        }

        registerToolModel(itemRubyAxe, "ruby_axe")
        registerToolModel(itemSapphireAxe, "sapphire_axe")
        registerToolModel(itemPeridotAxe, "peridot_axe")

        registerToolModel(itemRubyHoe, "ruby_hoe")
        registerToolModel(itemSapphireHoe, "sapphire_hoe")
        registerToolModel(itemPeridotHoe, "peridot_hoe")

        registerToolModel(itemRubyPickaxe, "ruby_pickaxe")
        registerToolModel(itemSapphirePickaxe, "sapphire_pickaxe")
        registerToolModel(itemPeridotPickaxe, "peridot_pickaxe")

        registerToolModel(itemRubyShovel, "ruby_shovel")
        registerToolModel(itemSapphireShovel, "sapphire_shovel")
        registerToolModel(itemPeridotShovel, "peridot_shovel")

        registerToolModel(itemRubySword, "ruby_sword")
        registerToolModel(itemSapphireSword, "sapphire_sword")
        registerToolModel(itemPeridotSword, "peridot_sword")

        ModelRegistryHelper.registerItemRenderer(itemGoldSaw, GemSawRenderer)
        ModelRegistryHelper.registerItemRenderer(itemRubySaw, GemSawRenderer)
        ModelRegistryHelper.registerItemRenderer(itemSapphireSaw, GemSawRenderer)
        ModelRegistryHelper.registerItemRenderer(itemPeridotSaw, GemSawRenderer)

        registerToolModel(itemWoodSickle, "wood_sickle")
        registerToolModel(itemStoneSickle, "stone_sickle")
        registerToolModel(itemIronSickle, "iron_sickle")
        registerToolModel(itemGoldSickle, "gold_sickle")
        registerToolModel(itemRubySickle, "ruby_sickle")
        registerToolModel(itemSapphireSickle, "sapphire_sickle")
        registerToolModel(itemPeridotSickle, "peridot_sickle")
        registerToolModel(itemDiamondSickle, "diamond_sickle")

        registerArmorModel(itemRubyHelmet, "ruby_helmet")
        registerArmorModel(itemRubyChestplate, "ruby_chestplate")
        registerArmorModel(itemRubyLeggings, "ruby_leggings")
        registerArmorModel(itemRubyBoots, "ruby_boots")

        registerArmorModel(itemSapphireHelmet, "sapphire_helmet")
        registerArmorModel(itemSapphireChestplate, "sapphire_chestplate")
        registerArmorModel(itemSapphireLeggings, "sapphire_leggings")
        registerArmorModel(itemSapphireBoots, "sapphire_boots")

        registerArmorModel(itemPeridotHelmet, "peridot_helmet")
        registerArmorModel(itemPeridotChestplate, "peridot_chestplate")
        registerArmorModel(itemPeridotLeggings, "peridot_leggings")
        registerArmorModel(itemPeridotBoots, "peridot_boots")
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()

        GuiHandler.register(GuiBackpack, guiIDBackpack)

        //TileRenderRegistry.setRenderer(blockLily, 0, RenderLily)
        //MultiTileRenderRegistry.setRenderer(blockBarrel, 0, RenderBarrel)

        ClientRegistry.bindTileEntitySpecialRenderer(classOf[TileBarrel], RenderBarrel)
    }

    @SideOnly(Side.CLIENT)
    def registerItemModelTypes(item:Item, regName:String, itemDef:ItemDefinition) {
        for (v <- itemDef.values) {
            val modelloc = new ModelResourceLocation(regName, "type=" + v.getVariantName)
            ModelLoader.setCustomModelResourceLocation(item, v.meta, modelloc)
        }
    }

    @SideOnly(Side.CLIENT)
    def registerModelType(item:Item, jsonLocation:String, typeValue:String){
        registerModelType(item, 0, jsonLocation, typeValue)
    }

    @SideOnly(Side.CLIENT)
    def registerModelType(item:Item, meta:Int, jsonLocation:String, typeValue:String) {
        val modelLoc = new ModelResourceLocation(jsonLocation, "type=" + typeValue)
        ModelLoader.setCustomModelResourceLocation(item, meta, modelLoc)
    }

    @SideOnly(Side.CLIENT)
    def registerToolModel(item: Item, variant:String) {
        val modelLoc = new ModelResourceLocation("projectred:exploration/tools", "type=" + variant)
        ModelLoader.setCustomModelResourceLocation(item, 0, modelLoc)
        ModelLoader.setCustomMeshDefinition(item, new ItemMeshDefinition {
            override def getModelLocation(stack: ItemStack): ModelResourceLocation = modelLoc
        })
    }

    @SideOnly(Side.CLIENT)
    def registerArmorModel(item: Item, variant:String) = {
        val modelLoc = new ModelResourceLocation("projectred:exploration/armor", s"type=$variant")
        ModelLoader.setCustomModelResourceLocation(item, 0, modelLoc)
    }
}

object ExplorationProxy extends ExplorationProxy_client

object ExplorationRecipes
{
    def initRecipes()
    {
        initOreDict()
        initEtcRecipes()
        initGemToolRecipes()
        initToolRecipes()
        initWorldRecipes()
    }

    private def initOreDict()
    {
        for (i <- 0 until 16)
            OreDictionary.registerOre(ItemBackpack.oreDictionaryVal, new ItemStack(ProjectRedExploration.itemBackpack, 1, i))

        OreDictionary.registerOre("oreRuby", OreDefs.ORERUBY.makeStack)
        OreDictionary.registerOre("oreSapphire", OreDefs.ORESAPPHIRE.makeStack)
        OreDictionary.registerOre("orePeridot", OreDefs.OREPERIDOT.makeStack)
        OreDictionary.registerOre("oreCopper", OreDefs.ORECOPPER.makeStack)
        OreDictionary.registerOre("oreTin", OreDefs.ORETIN.makeStack)
        OreDictionary.registerOre("oreSilver", OreDefs.ORESILVER.makeStack)
        OreDictionary.registerOre("oreElectrotine", OreDefs.OREELECTROTINE.makeStack)

        OreDictionary.registerOre("blockMarble", DecorativeStoneDefs.MARBLE.makeStack)
        OreDictionary.registerOre("blockRuby", DecorativeStoneDefs.RUBYBLOCK.makeStack)
        OreDictionary.registerOre("blockSapphire", DecorativeStoneDefs.SAPPHIREBLOCK.makeStack)
        OreDictionary.registerOre("blockPeridot", DecorativeStoneDefs.PERIDOTBLOCK.makeStack)
        OreDictionary.registerOre("blockCopper", DecorativeStoneDefs.COPPERBLOCK.makeStack)
        OreDictionary.registerOre("blockTin", DecorativeStoneDefs.TINBLOCK.makeStack)
        OreDictionary.registerOre("blockSilver", DecorativeStoneDefs.SILVERBLOCK.makeStack)
        OreDictionary.registerOre("blockElectrotine", DecorativeStoneDefs.ELECTROTINEBLOCK.makeStack)
    }

    private def initGemToolRecipes()
    {
        /** Axes **/
        addAxeRecipe(new ItemStack(ProjectRedExploration.itemRubyAxe), "gemRuby")
        addAxeRecipe(new ItemStack(ProjectRedExploration.itemSapphireAxe), "gemSapphire")
        addAxeRecipe(new ItemStack(ProjectRedExploration.itemPeridotAxe), "gemPeridot")

        /** Hoes **/
        addHoeRecipe(new ItemStack(ProjectRedExploration.itemRubyHoe), "gemRuby")
        addHoeRecipe(new ItemStack(ProjectRedExploration.itemSapphireHoe), "gemSapphire")
        addHoeRecipe(new ItemStack(ProjectRedExploration.itemPeridotHoe), "gemPeridot")

        /** Pickaxe **/
        addPickaxeRecipe(new ItemStack(ProjectRedExploration.itemRubyPickaxe), "gemRuby")
        addPickaxeRecipe(new ItemStack(ProjectRedExploration.itemSapphirePickaxe), "gemSapphire")
        addPickaxeRecipe(new ItemStack(ProjectRedExploration.itemPeridotPickaxe), "gemPeridot")

        /** Shovel **/
        addShovelRecipe(new ItemStack(ProjectRedExploration.itemRubyShovel), "gemRuby")
        addShovelRecipe(new ItemStack(ProjectRedExploration.itemSapphireShovel), "gemSapphire")
        addShovelRecipe(new ItemStack(ProjectRedExploration.itemPeridotShovel), "gemPeridot")

        /** Sword **/
        addSwordRecipe(new ItemStack(ProjectRedExploration.itemRubySword), "gemRuby")
        addSwordRecipe(new ItemStack(ProjectRedExploration.itemSapphireSword), "gemSapphire")
        addSwordRecipe(new ItemStack(ProjectRedExploration.itemPeridotSword), "gemPeridot")

        /** Saw **/
        addSawRecipe(new ItemStack(ProjectRedExploration.itemGoldSaw), "ingotGold")
        addSawRecipe(new ItemStack(ProjectRedExploration.itemRubySaw), "gemRuby")
        addSawRecipe(new ItemStack(ProjectRedExploration.itemSapphireSaw), "gemSapphire")
        addSawRecipe(new ItemStack(ProjectRedExploration.itemPeridotSaw), "gemPeridot")

        /** Sickle **/
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemWoodSickle), "plankWood")
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemStoneSickle), new ItemStack(Items.FLINT))
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemIronSickle), "ingotIron")
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemGoldSickle), "ingotGold")
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemRubySickle), "gemRuby")
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemSapphireSickle), "gemSapphire")
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemPeridotSickle), "gemPeridot")
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemDiamondSickle), "gemDiamond")

        /** Armor **/
        addHelmetRecipe(new ItemStack(ProjectRedExploration.itemRubyHelmet), "gemRuby")
        addChestplateRecipe(new ItemStack(ProjectRedExploration.itemRubyChestplate), "gemRuby")
        addLeggingsRecipe(new ItemStack(ProjectRedExploration.itemRubyLeggings), "gemRuby")
        addBootsRecipe(new ItemStack(ProjectRedExploration.itemRubyBoots), "gemRuby")
        addHelmetRecipe(new ItemStack(ProjectRedExploration.itemSapphireHelmet), "gemSapphire")
        addChestplateRecipe(new ItemStack(ProjectRedExploration.itemSapphireChestplate), "gemSapphire")
        addLeggingsRecipe(new ItemStack(ProjectRedExploration.itemSapphireLeggings), "gemSapphire")
        addBootsRecipe(new ItemStack(ProjectRedExploration.itemSapphireBoots), "gemSapphire")
        addHelmetRecipe(new ItemStack(ProjectRedExploration.itemPeridotHelmet), "gemPeridot")
        addChestplateRecipe(new ItemStack(ProjectRedExploration.itemPeridotChestplate), "gemPeridot")
        addLeggingsRecipe(new ItemStack(ProjectRedExploration.itemPeridotLeggings), "gemPeridot")
        addBootsRecipe(new ItemStack(ProjectRedExploration.itemPeridotBoots), "gemPeridot")
    }

    private def addHelmetRecipe(o:ItemStack, m:String)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o, "mmm", "m m", 'm':JChar, m))
    }

    private def addChestplateRecipe(o:ItemStack, m:String)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o, "m m", "mmm", "mmm", 'm':JChar, m))
    }

    private def addLeggingsRecipe(o:ItemStack, m:String)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o, "mmm", "m m", "m m", 'm':JChar, m))
    }

    private def addBootsRecipe(o:ItemStack, m:String)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o, "m m", "m m", 'm':JChar, m))
    }

    private def addAxeRecipe(o:ItemStack, m:String)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
            "mm", "ms", " s",
            'm':JChar, m,
            's':JChar, "stickWood"))
    }

    private def addHoeRecipe(o:ItemStack, m:String)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
            "mm", " s", " s",
            'm':JChar, m,
            's':JChar, "stickWood"))
    }

    private def addPickaxeRecipe(o:ItemStack, m:String)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
            "mmm", " s ", " s ",
            'm':JChar, m,
            's':JChar, "stickWood"))
    }

    private def addShovelRecipe(o:ItemStack, m:String)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
            "m", "s", "s",
            'm':JChar, m,
            's':JChar, "stickWood"))
    }

    private def addSwordRecipe(o:ItemStack, m:String)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
            "m", "m", "s",
            'm':JChar, m,
            's':JChar, "stickWood"))
    }

    private def addSawRecipe(o:ItemStack, m:String)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
            "srr", "sbb",
            's':JChar, "stickWood",
            'r':JChar, "rodStone",
            'b':JChar, m))
    }

    private def addSickleRecipe(o:ItemStack, m:AnyRef)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
            " m ", "  m", "sm ",
            's':JChar, "stickWood",
            'm':JChar, m))
    }

    private def initEtcRecipes()
    {
        /** Wool Gin to string recipe **/
        GameRegistry.addRecipe(new ItemStack(Items.STRING, 4),
            "gw",
            'g':JChar, new ItemStack(ProjectRedExploration.itemWoolGin, 1, OreDictionary.WILDCARD_VALUE),
            'w':JChar, Blocks.WOOL)

        /** Item Barrel  **/
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedExploration.blockBarrel),
            "lwl", "i i", "lll",
            'l':JChar, "logWood",
            'w':JChar, "slabWood",
            'i':JChar, "ingotIron"))
    }

    private def initToolRecipes()
    {
        /** Wool Gin **/
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedExploration.itemWoolGin),
            "sis", "sss", " s ",
            's':JChar, "stickWood",
            'i':JChar, PartDefs.IRONCOIL.makeStack))

        /** Backpacks **/
        for (i <- 0 until 16)
        {
            GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedExploration.itemBackpack, 1, i),
                "ccc",
                if(i == 0) "c c" else "cdc",
                "ccc",
                'c':JChar, PartDefs.WOVENCLOTH.makeStack,
                'd':JChar, EnumColour.fromWoolID(i).getOreDictionaryName))

            GameRegistry.addRecipe(new ShapelessOreNBTRecipe(new ItemStack(ProjectRedExploration.itemBackpack, 1, i),
                ItemBackpack.oreDictionaryVal, EnumColour.fromWoolID(i).getOreDictionaryName).setKeepNBT())
        }

       GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedExploration.itemAthame),
           "s", "w",
           's':JChar, "ingotSilver",
           'w':JChar, "stickWood"))
    }

    private def initWorldRecipes()
    {
        /** Marble brick **/
        GameRegistry.addRecipe(new ShapedOreRecipe(DecorativeStoneDefs.MARBLEBRICK.makeStack(4),
            "bb", "bb",
            'b':JChar, "blockMarble"))

        /** Basalt brick **/
        GameRegistry.addRecipe(DecorativeStoneDefs.BASALTBRICK.makeStack(4),
            "bb", "bb",
            'b':JChar, DecorativeStoneDefs.BASALT.makeStack)

        /** Basalt **/
        addSmeltingRecipe(DecorativeStoneDefs.BASALTCOBBLE.makeStack, DecorativeStoneDefs.BASALT.makeStack)

        /** Ore Smelting**/
        addSmeltingRecipe(OreDefs.ORERUBY.makeStack, PartDefs.RUBY.makeStack)
        addSmeltingRecipe(OreDefs.ORESAPPHIRE.makeStack, PartDefs.SAPPHIRE.makeStack)
        addSmeltingRecipe(OreDefs.OREPERIDOT.makeStack, PartDefs.PERIDOT.makeStack)
        addSmeltingRecipe(OreDefs.ORECOPPER.makeStack, PartDefs.COPPERINGOT.makeStack)
        addSmeltingRecipe(OreDefs.ORETIN.makeStack, PartDefs.TININGOT.makeStack)
        addSmeltingRecipe(OreDefs.ORESILVER.makeStack, PartDefs.SILVERINGOT.makeStack)
        addSmeltingRecipe(OreDefs.OREELECTROTINE.makeStack, PartDefs.ELECTROTINE.makeStack)

        /** Storage blocks **/
        addStorageBlockRecipe("gemRuby", PartDefs.RUBY.makeStack(9), "blockRuby", DecorativeStoneDefs.RUBYBLOCK.makeStack)
        addStorageBlockRecipe("gemSapphire", PartDefs.SAPPHIRE.makeStack(9), "blockSapphire", DecorativeStoneDefs.SAPPHIREBLOCK.makeStack)
        addStorageBlockRecipe("gemPeridot", PartDefs.PERIDOT.makeStack(9), "blockPeridot", DecorativeStoneDefs.PERIDOTBLOCK.makeStack)
        addStorageBlockRecipe("ingotCopper", PartDefs.COPPERINGOT.makeStack(9), "blockCopper", DecorativeStoneDefs.COPPERBLOCK.makeStack)
        addStorageBlockRecipe("ingotTin", PartDefs.TININGOT.makeStack(9), "blockTin", DecorativeStoneDefs.TINBLOCK.makeStack)
        addStorageBlockRecipe("ingotSilver", PartDefs.SILVERINGOT.makeStack(9), "blockSilver", DecorativeStoneDefs.SILVERBLOCK.makeStack)
        addStorageBlockRecipe("dustElectrotine", PartDefs.ELECTROTINE.makeStack(9), "blockElectrotine", DecorativeStoneDefs.ELECTROTINEBLOCK.makeStack)

        for (i <- 0 until DecorativeStoneDefs.values.size)
        {
            val s:DecorativeStoneDefs.StoneVal = DecorativeStoneDefs.values.apply(i)
            addWallRecipe(new ItemStack(ProjectRedExploration.blockDecorativeWall, 6, s.meta), s.makeStack)
        }
    }

    private def addSmeltingRecipe(in:ItemStack, out:ItemStack)
    {
        (RecipeLib.newSmeltingBuilder
                += new ItemIn(in)
                += new ItemOut(out)).registerResults()
    }

    private def addStorageBlockRecipe(itemOre:String, item:ItemStack, blockOre:String, block:ItemStack)
    {
        (RecipeLib.newShapedBuilder <-> "xxxxxxxxx"
                += new OreIn(itemOre).to("x")
                += new ItemOut(block)).registerResult()
        (RecipeLib.newShapelessBuilder
                += new OreIn(blockOre)
                += new ItemOut(item)).registerResult()
    }

    private def addWallRecipe(o:ItemStack, m:ItemStack)
    {
        GameRegistry.addRecipe(o, "mmm", "mmm", 'm':JChar, m)
    }
}
