package mrtjp.projectred.exploration

import java.lang.{Character => JChar}

import codechicken.microblock.BlockMicroMaterial
import mrtjp.core.block.ItemBlockCore
import mrtjp.core.gui.GuiHandler
import mrtjp.core.inventory.InvWrapper
import mrtjp.core.item.ItemDefinition
import mrtjp.core.world._
import mrtjp.projectred.ProjectRedExploration
import mrtjp.projectred.ProjectRedExploration._
import mrtjp.projectred.core.{Configurator, IProxy, PartDefs}
import net.minecraft.block.state.IBlockState
import net.minecraft.client.renderer.ItemMeshDefinition
import net.minecraft.client.renderer.block.model.ModelResourceLocation
import net.minecraft.client.renderer.block.statemap.StateMapperBase
import net.minecraft.init.{Blocks, SoundEvents}
import net.minecraft.inventory.EntityEquipmentSlot
import net.minecraft.item.{Item, ItemStack}
import net.minecraftforge.client.model.ModelLoader
import net.minecraftforge.common.util.EnumHelper
import net.minecraftforge.fml.client.registry.ClientRegistry
import net.minecraftforge.fml.common.registry.{ForgeRegistries, GameRegistry}
import net.minecraftforge.fml.relauncher.{Side, SideOnly}
import net.minecraftforge.oredict.OreDictionary

class ExplorationProxy_server extends IProxy
{
    val guiIDBackpack = 1

    override def preinit()
    {
        itemWoolGin = new ItemWoolGin
        itemWoolGin.setUnlocalizedName("projectred.exploration.woolGin")
        ForgeRegistries.ITEMS.register(itemWoolGin.setRegistryName("wool_gin"))

        itemBackpack = new ItemBackpack
        itemBackpack.setUnlocalizedName("projectred.exploration.backpack")
        ForgeRegistries.ITEMS.register(itemBackpack.setRegistryName("backpack"))

        itemAthame = new ItemAthame
        itemAthame.setUnlocalizedName("projectred.exploration.athame")
        ForgeRegistries.ITEMS.register(itemAthame.setRegistryName("athame"))

        blockOres = new BlockOre
        blockOres.setUnlocalizedName("projectred.exploration.ore")
        ForgeRegistries.BLOCKS.register(blockOres.setRegistryName("ore"))
        ForgeRegistries.ITEMS.register(new ItemBlockCore(blockOres).setRegistryName(blockOres.getRegistryName))
        for (o <- OreDefs.values) blockOres.setHarvestLevel("pickaxe", o.harvest, blockOres.getStateFromMeta(o.meta))

        blockDecorativeStone = new BlockDecorativeStone
        blockDecorativeStone.setUnlocalizedName("projectred.exploration.stone")
        ForgeRegistries.BLOCKS.register(blockDecorativeStone.setRegistryName("stone"))
        ForgeRegistries.ITEMS.register(new ItemBlockCore(blockDecorativeStone).setRegistryName(blockDecorativeStone.getRegistryName))
        for (b <- DecorativeStoneDefs.values) {
            blockDecorativeStone.setHarvestLevel("pickaxe", b.harvest, blockDecorativeStone.getStateFromMeta(b.meta))
            BlockMicroMaterial.createAndRegister(blockDecorativeStone.getStateFromMeta(b.meta)) //Register as microblocks
        }

        blockDecorativeWall = new BlockDecorativeWall
        blockDecorativeWall.setUnlocalizedName("projectred.exploration.stoneWall")
        ForgeRegistries.BLOCKS.register(blockDecorativeWall.setRegistryName("stone_wall"))
        ForgeRegistries.ITEMS.register(new ItemBlockCore(blockDecorativeWall).setRegistryName(blockDecorativeWall.getRegistryName))

        blockBarrel = new BlockBarrel
        blockBarrel.setUnlocalizedName("projectred.exploration.barrel")
        ForgeRegistries.BLOCKS.register(blockBarrel.setRegistryName("barrel"))
        ForgeRegistries.ITEMS.register(new ItemBlockCore(blockBarrel).setRegistryName(blockBarrel.getRegistryName))
        blockBarrel.addTile(classOf[TileBarrel], 0)

        toolMaterialRuby        = EnumHelper.addToolMaterial("RUBY",      2, 512, 8.00F, 3.00F, 10)
        toolMaterialSapphire    = EnumHelper.addToolMaterial("SAPPHIRE",  2, 512, 8.00F, 3.00F, 10)
        toolMaterialPeridot     = EnumHelper.addToolMaterial("PERIDOT",   2, 512, 7.75F, 2.75F, 14)

        armorMatrialRuby        = EnumHelper.addArmorMaterial("RUBY",     "ruby",     16, Array(3, 6, 8, 3), 10, SoundEvents.ITEM_ARMOR_EQUIP_DIAMOND, 1.25f)
        armorMatrialSapphire    = EnumHelper.addArmorMaterial("SAPPHIRE", "sapphire", 16, Array(3, 6, 8, 3), 10, SoundEvents.ITEM_ARMOR_EQUIP_DIAMOND, 1.25f)
        armorMatrialPeridot     = EnumHelper.addArmorMaterial("PERIDOT",  "peridot",  14, Array(3, 6, 8, 3), 14, SoundEvents.ITEM_ARMOR_EQUIP_DIAMOND, 1.25f)

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
    }

    override def postinit()
    {
        InvWrapper.register(BarrelInvWrapper)
        initOreDict()
        /** Basalt **/
        GameRegistry.addSmelting(DecorativeStoneDefs.BASALTCOBBLE.makeStack, DecorativeStoneDefs.BASALT.makeStack, 0.1f)

        /** Ore Smelting**/
        GameRegistry.addSmelting(OreDefs.ORERUBY.makeStack, PartDefs.RUBY.makeStack, 1)
        GameRegistry.addSmelting(OreDefs.ORESAPPHIRE.makeStack, PartDefs.SAPPHIRE.makeStack, 1)
        GameRegistry.addSmelting(OreDefs.OREPERIDOT.makeStack, PartDefs.PERIDOT.makeStack, 1)
        GameRegistry.addSmelting(OreDefs.ORECOPPER.makeStack, PartDefs.COPPERINGOT.makeStack, 0.7f)
        GameRegistry.addSmelting(OreDefs.ORETIN.makeStack, PartDefs.TININGOT.makeStack, 0.7f)
        GameRegistry.addSmelting(OreDefs.ORESILVER.makeStack, PartDefs.SILVERINGOT.makeStack, 0.8f)
        GameRegistry.addSmelting(OreDefs.OREELECTROTINE.makeStack, PartDefs.ELECTROTINE.makeStack, 0.7f)
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
                new ModelResourceLocation("projectred:world/ore", "type=" + state.getValue(BlockProperties.ORE_TYPES))
            }
        })
        ModelLoader.setCustomStateMapper(blockDecorativeStone, new StateMapperBase {
            override protected def getModelResourceLocation(state: IBlockState): ModelResourceLocation = {
                new ModelResourceLocation("projectred:world/deceratives", "type=" + state.getValue(BlockProperties.ORE_TYPES))
            }
        })
        registerItemModelTypes(Item.getItemFromBlock(blockOres), "projectred:world/ore", OreDefs)
        registerItemModelTypes(Item.getItemFromBlock(blockDecorativeStone), "projectred:world/deceratives", DecorativeStoneDefs)
        for (v <- DecorativeStoneDefs.values) {
            val modelloc = new ModelResourceLocation("projectred:world/wall", "type=" + v.getVariantName + ",up=true,east=true,west=true")
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
                new ModelResourceLocation("projectred:world/wall", parseLocation(state))
            }
        })
        ModelLoader.setCustomStateMapper(blockBarrel, new StateMapperBase {
            override protected def getModelResourceLocation(state: IBlockState): ModelResourceLocation = {
                new ModelResourceLocation("projectred:world/barrel", "type=barrel")
            }
        })
        ModelLoader.setCustomModelResourceLocation(Item.getItemFromBlock(blockBarrel), 0, new ModelResourceLocation("projectred:world/barrel", "type=barrel"))

        registerModelType(itemWoolGin, "projectred:world/items", "wool_gin")
        registerModelType(itemAthame, "projectred:world/items", "athame")
        for (i <- 0 until 16) {
            registerModelType(itemBackpack, i, "projectred:world/items", "backpack_" + i)
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

        registerToolModel(itemGoldSaw, "gold_saw")
        registerToolModel(itemRubySaw, "ruby_saw")
        registerToolModel(itemSapphireSaw, "sapphire_saw")
        registerToolModel(itemPeridotSaw, "peridot_saw")
        //ModelRegistryHelper.registerItemRenderer(itemGoldSaw, GemSawRenderer)
        //ModelRegistryHelper.registerItemRenderer(itemRubySaw, GemSawRenderer)
        //ModelRegistryHelper.registerItemRenderer(itemSapphireSaw, GemSawRenderer)
        //ModelRegistryHelper.registerItemRenderer(itemPeridotSaw, GemSawRenderer)

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
        val modelLoc = new ModelResourceLocation("projectred:world/tools", "type=" + variant)
        ModelLoader.setCustomModelResourceLocation(item, 0, modelLoc)
        ModelLoader.setCustomMeshDefinition(item, new ItemMeshDefinition {
            override def getModelLocation(stack: ItemStack): ModelResourceLocation = modelLoc
        })
    }

    @SideOnly(Side.CLIENT)
    def registerArmorModel(item: Item, variant:String) = {
        val modelLoc = new ModelResourceLocation("projectred:world/armor", s"type=$variant")
        ModelLoader.setCustomModelResourceLocation(item, 0, modelLoc)
    }
}

object ExplorationProxy extends ExplorationProxy_client
