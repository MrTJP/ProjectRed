package mrtjp.projectred.exploration

import codechicken.lib.colour.EnumColour
import codechicken.lib.datagen.recipe.RecipeProvider
import codechicken.lib.datagen.{ItemModelProvider, LootTableProvider}
import codechicken.lib.gui.SimpleItemGroup
import codechicken.lib.inventory.container.ICCLContainerType
import codechicken.lib.item.{SimpleArmorMaterial, SimpleItemTier}
import codechicken.lib.util.CrashLock
import codechicken.microblock.handler.MicroblockModContent
import mrtjp.projectred.ProjectRedExploration.MOD_ID
import mrtjp.projectred.core.CoreContent._
import mrtjp.projectred.core.ShapelessNBTCopyRecipeBuilder
import mrtjp.projectred.exploration.ExplorationContent._
import net.minecraft.block.material.Material
import net.minecraft.block.{AbstractBlock, Block, Blocks, WallBlock}
import net.minecraft.data.{BlockTagsProvider, DataGenerator, ItemTagsProvider}
import net.minecraft.inventory.EquipmentSlotType
import net.minecraft.item._
import net.minecraft.item.crafting.Ingredient
import net.minecraft.tags.{BlockTags, ITag, ItemTags}
import net.minecraft.util.{ResourceLocation, SoundEvents}
import net.minecraftforge.client.model.generators.BlockStateProvider
import net.minecraftforge.common.Tags.{Blocks => ForgeBlockTags, Items => ForgeItemTags}
import net.minecraftforge.common.ToolType
import net.minecraftforge.common.data.ExistingFileHelper
import net.minecraftforge.eventbus.api.{IEventBus, SubscribeEvent}
import net.minecraftforge.fml.event.lifecycle.GatherDataEvent
import net.minecraftforge.registries.{DeferredRegister, ForgeRegistries}

import java.util.function.Supplier

object ExplorationContent {

    private val LOCK = new CrashLock("Already Initialized.")
    private val ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, MOD_ID)
    private val BLOCKS = DeferredRegister.create(ForgeRegistries.BLOCKS, MOD_ID)
    private val CONTAINERS = DeferredRegister.create(ForgeRegistries.CONTAINERS, MOD_ID)

    val explorationItemGroup = new SimpleItemGroup(MOD_ID, () => new ItemStack(Blocks.GRASS_BLOCK))

    /** Materials */
    val athameItemTier = SimpleItemTier.builder(ItemTier.DIAMOND)
        .maxUses(100)
        .enchantability(30)
        .repairMaterial(() => Ingredient.of(itemSilverIngot))
        .build()

    val rubyItemTier = SimpleItemTier.builder()
        .maxUses(512)
        .efficiency(8.00F)
        .attackDamage(3.00F)
        .harvestLevel(2)
        .enchantability(10)
        .repairMaterial(() => Ingredient.of(itemRuby))
        .build()
    val sapphireItemTier = SimpleItemTier.builder()
        .maxUses(512)
        .efficiency(8.00F)
        .attackDamage(3.00F)
        .harvestLevel(2)
        .enchantability(10)
        .repairMaterial(() => Ingredient.of(itemSapphire))
        .build()
    val peridotItemTier = SimpleItemTier.builder()
        .maxUses(512)
        .efficiency(7.75F)
        .attackDamage(2.75F)
        .harvestLevel(2)
        .enchantability(14)
        .repairMaterial(() => Ingredient.of(itemPeridot))
        .build()

    val rubyArmorMaterial = SimpleArmorMaterial.builder()
        .durabilityFactor(16)
        .damageReduction(Array(3, 6, 8, 3))
        .enchantability(10)
        .soundEvent(SoundEvents.ARMOR_EQUIP_DIAMOND)
        .repairMaterial(() => Ingredient.of(itemRuby))
        .textureName(MOD_ID + ":ruby")
        .toughness(1.25F)
        .build()

    val sapphireArmorMaterial = SimpleArmorMaterial.builder()
        .durabilityFactor(16)
        .damageReduction(Array(3, 6, 8, 3))
        .enchantability(10)
        .soundEvent(SoundEvents.ARMOR_EQUIP_DIAMOND)
        .repairMaterial(() => Ingredient.of(itemSapphire))
        .textureName(MOD_ID + ":sapphire")
        .toughness(1.25F)
        .build()

    val peridotArmorMaterial = SimpleArmorMaterial.builder()
        .durabilityFactor(14)
        .damageReduction(Array(3, 6, 8, 3))
        .enchantability(14)
        .soundEvent(SoundEvents.ARMOR_EQUIP_DIAMOND)
        .repairMaterial(() => Ingredient.of(itemPeridot))
        .textureName(MOD_ID + ":peridot")
        .toughness(1.25F)
        .build()

    /** Blocks */
    val blockRubyOre = BLOCKS.register("ruby_ore", makeOreBlock(2, 2, 7))
    val blockSapphireOre = BLOCKS.register("sapphire_ore", makeOreBlock(2, 2, 7))
    val blockPeridotOre = BLOCKS.register("peridot_ore", makeOreBlock(2, 2, 7))

    val blockCopperOre = BLOCKS.register("copper_ore", makeOreBlock())
    val blockTinOre = BLOCKS.register("tin_ore", makeOreBlock())
    val blockSilverOre = BLOCKS.register("silver_ore", makeOreBlock(2))
    val blockElectrotineOre = BLOCKS.register("electrotine_ore", makeOreBlock(2, 1, 5))

    val blockMarble = BLOCKS.register("marble", makeDecorativeBlock(2, 1.0F, 14.0F))
    val blockMarbleBrick = BLOCKS.register("marble_brick", makeDecorativeBlock(2, 1.0F, 14.0F))
    val blockBasalt = BLOCKS.register("basalt", makeDecorativeBlock(2, 2.5F, 16.0F))
    val blockBasaltCobble = BLOCKS.register("basalt_cobble", makeDecorativeBlock(2, 2.5F, 14.0F))
    val blockBasaltBrick = BLOCKS.register("basalt_brick", makeDecorativeBlock(2, 2.5F, 20.0F))
    val blockRubyBlock = BLOCKS.register("ruby_block", makeDecorativeBlock())
    val blockSapphireBlock = BLOCKS.register("sapphire_block", makeDecorativeBlock())
    val blockPeridotBlock = BLOCKS.register("peridot_block", makeDecorativeBlock())
    val blockCopperBlock = BLOCKS.register("copper_block", makeDecorativeBlock())
    val blockTinBlock = BLOCKS.register("tin_block", makeDecorativeBlock())
    val blockSilverBlock = BLOCKS.register("silver_block", makeDecorativeBlock())
    val blockElectrotineBlock = BLOCKS.register("electrotine_block", makeDecorativeBlock())

    val blockMarbleWall = BLOCKS.register("marble_wall", makeDecorativeWallBlock(blockMarble))
    val blockMarbleBrickWall = BLOCKS.register("marble_brick_wall", makeDecorativeWallBlock(blockMarbleBrick))
    val blockBasaltWall = BLOCKS.register("basalt_wall", makeDecorativeWallBlock(blockBasalt))
    val blockBasaltCobbleWall = BLOCKS.register("basalt_cobble_wall", makeDecorativeWallBlock(blockBasaltCobble))
    val blockBasaltBrickWall = BLOCKS.register("basalt_brick_wall", makeDecorativeWallBlock(blockBasaltBrick))
    val blockRubyBlockWall = BLOCKS.register("ruby_block_wall", makeDecorativeWallBlock(blockRubyBlock))
    val blockSapphireBlockWall = BLOCKS.register("sapphire_block_wall", makeDecorativeWallBlock(blockSapphireBlock))
    val blockPeridotBlockWall = BLOCKS.register("peridot_block_wall", makeDecorativeWallBlock(blockPeridotBlock))
    val blockCopperBlockWall = BLOCKS.register("copper_block_wall", makeDecorativeWallBlock(blockCopperBlock))
    val blockTinBlockWall = BLOCKS.register("tin_block_wall", makeDecorativeWallBlock(blockTinBlock))
    val blockSilverBlockWall = BLOCKS.register("silver_block_wall", makeDecorativeWallBlock(blockSilverBlock))
    val blockElectrotineBlockWall = BLOCKS.register("electrotine_block_wall", makeDecorativeWallBlock(blockElectrotineBlock))

    /** Items */
    val itemRubyOre = ITEMS.register("ruby_ore", makeItemBlock(blockRubyOre))
    val itemSapphireOre = ITEMS.register("sapphire_ore", makeItemBlock(blockSapphireOre))
    val itemPeridotOre = ITEMS.register("peridot_ore", makeItemBlock(blockPeridotOre))

    val itemCopperOre = ITEMS.register("copper_ore", makeItemBlock(blockCopperOre))
    val itemTinOre = ITEMS.register("tin_ore", makeItemBlock(blockTinOre))
    val itemSilverOre = ITEMS.register("silver_ore", makeItemBlock(blockSilverOre))
    val itemElectrotineOre = ITEMS.register("electrotine_ore", makeItemBlock(blockElectrotineOre))

    val itemMarble = ITEMS.register("marble", makeItemBlock(blockMarble))
    val itemMarbleBrick = ITEMS.register("marble_brick", makeItemBlock(blockMarbleBrick))
    val itemBasalt = ITEMS.register("basalt", makeItemBlock(blockBasalt))
    val itemBasaltCobble = ITEMS.register("basalt_cobble", makeItemBlock(blockBasaltCobble))
    val itemBasaltBrick = ITEMS.register("basalt_brick", makeItemBlock(blockBasaltBrick))
    val itemRubyBlock = ITEMS.register("ruby_block", makeItemBlock(blockRubyBlock))
    val itemSapphireBlock = ITEMS.register("sapphire_block", makeItemBlock(blockSapphireBlock))
    val itemPeridotBlock = ITEMS.register("peridot_block", makeItemBlock(blockPeridotBlock))
    val itemCopperBlock = ITEMS.register("copper_block", makeItemBlock(blockCopperBlock))
    val itemTinBlock = ITEMS.register("tin_block", makeItemBlock(blockTinBlock))
    val itemSilverBlock = ITEMS.register("silver_block", makeItemBlock(blockSilverBlock))
    val itemElectrotineBlock = ITEMS.register("electrotine_block", makeItemBlock(blockElectrotineBlock))

    val itemMarbleWall = ITEMS.register("marble_wall", makeItemBlock(blockMarbleWall))
    val itemMarbleBrickWall = ITEMS.register("marble_brick_wall", makeItemBlock(blockMarbleBrickWall))
    val itemBasaltWall = ITEMS.register("basalt_wall", makeItemBlock(blockBasaltWall))
    val itemBasaltCobbleWall = ITEMS.register("basalt_cobble_wall", makeItemBlock(blockBasaltCobbleWall))
    val itemBasaltBrickWall = ITEMS.register("basalt_brick_wall", makeItemBlock(blockBasaltBrickWall))
    val itemRubyBlockWall = ITEMS.register("ruby_block_wall", makeItemBlock(blockRubyBlockWall))
    val itemSapphireBlockWall = ITEMS.register("sapphire_block_wall", makeItemBlock(blockSapphireBlockWall))
    val itemPeridotBlockWall = ITEMS.register("peridot_block_wall", makeItemBlock(blockPeridotBlockWall))
    val itemCopperBlockWall = ITEMS.register("copper_block_wall", makeItemBlock(blockCopperBlockWall))
    val itemTinBlockWall = ITEMS.register("tin_block_wall", makeItemBlock(blockTinBlockWall))
    val itemSilverBlockWall = ITEMS.register("silver_block_wall", makeItemBlock(blockSilverBlockWall))
    val itemElectrotineBlockWall = ITEMS.register("electrotine_block_wall", makeItemBlock(blockElectrotineBlockWall))

    val itemWoolGin = ITEMS.register("wool_gin", () => new ItemWoolGin)
    val itemAthame = ITEMS.register("athame", () => new ItemAthame)
    var itemWhiteBackpack = ITEMS.register("white_backpack", () => new ItemBackpack())
    var itemOrangeBackpack = ITEMS.register("orange_backpack", () => new ItemBackpack())
    var itemMagentaBackpack = ITEMS.register("magenta_backpack", () => new ItemBackpack())
    var itemLightBlueBackpack = ITEMS.register("light_blue_backpack", () => new ItemBackpack())
    var itemYellowBackpack = ITEMS.register("yellow_backpack", () => new ItemBackpack())
    var itemLimeBackpack = ITEMS.register("lime_backpack", () => new ItemBackpack())
    var itemPinkBackpack = ITEMS.register("pink_backpack", () => new ItemBackpack())
    var itemGrayBackpack = ITEMS.register("gray_backpack", () => new ItemBackpack())
    var itemLightGrayBackpack = ITEMS.register("light_gray_backpack", () => new ItemBackpack())
    var itemCyanBackpack = ITEMS.register("cyan_backpack", () => new ItemBackpack())
    var itemPurpleBackpack = ITEMS.register("purple_backpack", () => new ItemBackpack())
    var itemBlueBackpack = ITEMS.register("blue_backpack", () => new ItemBackpack())
    var itemBrownBackpack = ITEMS.register("brown_backpack", () => new ItemBackpack())
    var itemGreenBackpack = ITEMS.register("green_backpack", () => new ItemBackpack())
    var itemRedBackpack = ITEMS.register("red_backpack", () => new ItemBackpack())
    var itemBlackBackpack = ITEMS.register("black_backpack", () => new ItemBackpack())

    val itemRubyAxe = ITEMS.register("ruby_axe", makeItem(new AxeItem(rubyItemTier, 5.0F, -3.0F, _)))
    val itemSapphireAxe = ITEMS.register("sapphire_axe", makeItem(new AxeItem(sapphireItemTier, 5.0F, -3.0F, _)))
    val itemPeridotAxe = ITEMS.register("peridot_axe", makeItem(new AxeItem(peridotItemTier, 5.0F, -3.0F, _)))

    val itemRubyHoe = ITEMS.register("ruby_hoe", makeItem(new HoeItem(rubyItemTier, -3, 0.0F, _)))
    val itemSapphireHoe = ITEMS.register("sapphire_hoe", makeItem(new HoeItem(sapphireItemTier, -3, 0.0F, _)))
    val itemPeridotHoe = ITEMS.register("peridot_hoe", makeItem(new HoeItem(peridotItemTier, -3, 0.0F, _)))

    val itemRubyPickaxe = ITEMS.register("ruby_pickaxe", makeItem(new PickaxeItem(rubyItemTier, 1, -2.8F, _)))
    val itemSapphirePickaxe = ITEMS.register("sapphire_pickaxe", makeItem(new PickaxeItem(sapphireItemTier, 1, -2.8F, _)))
    val itemPeridotPickaxe = ITEMS.register("peridot_pickaxe", makeItem(new PickaxeItem(peridotItemTier, 1, -2.8F, _)))

    val itemRubyShovel = ITEMS.register("ruby_shovel", makeItem(new ShovelItem(rubyItemTier, 1.5F, -3.0F, _)))
    val itemSapphireShovel = ITEMS.register("sapphire_shovel", makeItem(new ShovelItem(sapphireItemTier, 1.5F, -3.0F, _)))
    val itemPeridotShovel = ITEMS.register("peridot_shovel", makeItem(new ShovelItem(peridotItemTier, 1.5F, -3.0F, _)))

    val itemRubySword = ITEMS.register("ruby_sword", makeItem(new SwordItem(rubyItemTier, 3, -2.4F, _)))
    val itemSapphireSword = ITEMS.register("sapphire_sword", makeItem(new SwordItem(sapphireItemTier, 3, -2.4F, _)))
    val itemPeridotSword = ITEMS.register("peridot_sword", makeItem(new SwordItem(peridotItemTier, 3, -2.4F, _)))

    val itemGoldSaw = ITEMS.register("gold_saw", makeItem(new ItemSaw(ItemTier.GOLD, _)))
    val itemRubySaw = ITEMS.register("ruby_saw", makeItem(new ItemSaw(rubyItemTier, _)))
    val itemSapphireSaw = ITEMS.register("sapphire_saw", makeItem(new ItemSaw(sapphireItemTier, _)))
    val itemPeridotSaw = ITEMS.register("peridot_saw", makeItem(new ItemSaw(peridotItemTier, _)))

    val itemWoodSickle = ITEMS.register("wood_sickle", makeItem(new ItemSickle(ItemTier.WOOD, 1, -2.8F, _)))
    val itemStoneSickle = ITEMS.register("stone_sickle", makeItem(new ItemSickle(ItemTier.STONE, 1, -2.8F, _)))
    val itemIronSickle = ITEMS.register("iron_sickle", makeItem(new ItemSickle(ItemTier.IRON, 1, -2.8F, _)))
    val itemGoldSickle = ITEMS.register("gold_sickle", makeItem(new ItemSickle(ItemTier.GOLD, 1, -2.8F, _)))
    val itemRubySickle = ITEMS.register("ruby_sickle", makeItem(new ItemSickle(rubyItemTier, 1, -2.8F, _)))
    val itemSapphireSickle = ITEMS.register("sapphire_sickle", makeItem(new ItemSickle(sapphireItemTier, 1, -2.8F, _)))
    val itemPeridotSickle = ITEMS.register("peridot_sickle", makeItem(new ItemSickle(peridotItemTier, 1, -2.8F, _)))
    val itemDiamondSickle = ITEMS.register("diamond_sickle", makeItem(new ItemSickle(ItemTier.DIAMOND, 1, -2.8F, _)))

    val itemRubyHelmet = ITEMS.register("ruby_helmet", makeItem(new ArmorItem(rubyArmorMaterial, EquipmentSlotType.HEAD, _)))
    val itemRubyChestplate = ITEMS.register("ruby_chestplate", makeItem(new ArmorItem(rubyArmorMaterial, EquipmentSlotType.CHEST, _)))
    val itemRubyLeggings = ITEMS.register("ruby_leggings", makeItem(new ArmorItem(rubyArmorMaterial, EquipmentSlotType.LEGS, _)))
    val itemRubyBoots = ITEMS.register("ruby_boots", makeItem(new ArmorItem(rubyArmorMaterial, EquipmentSlotType.FEET, _)))

    val itemSapphireHelmet = ITEMS.register("sapphire_helmet", makeItem(new ArmorItem(sapphireArmorMaterial, EquipmentSlotType.HEAD, _)))
    val itemSapphireChestplate = ITEMS.register("sapphire_chestplate", makeItem(new ArmorItem(sapphireArmorMaterial, EquipmentSlotType.CHEST, _)))
    val itemSapphireLeggings = ITEMS.register("sapphire_leggings", makeItem(new ArmorItem(sapphireArmorMaterial, EquipmentSlotType.LEGS, _)))
    val itemSapphireBoots = ITEMS.register("sapphire_boots", makeItem(new ArmorItem(sapphireArmorMaterial, EquipmentSlotType.FEET, _)))

    val itemPeridotHelmet = ITEMS.register("peridot_helmet", makeItem(new ArmorItem(peridotArmorMaterial, EquipmentSlotType.HEAD, _)))
    val itemPeridotChestplate = ITEMS.register("peridot_chestplate", makeItem(new ArmorItem(peridotArmorMaterial, EquipmentSlotType.CHEST, _)))
    val itemPeridotLeggings = ITEMS.register("peridot_leggings", makeItem(new ArmorItem(peridotArmorMaterial, EquipmentSlotType.LEGS, _)))
    val itemPeridotBoots = ITEMS.register("peridot_boots", makeItem(new ArmorItem(peridotArmorMaterial, EquipmentSlotType.FEET, _)))

    /** Groups */
    lazy val backpacks = List(
        itemWhiteBackpack.get,
        itemOrangeBackpack.get,
        itemMagentaBackpack.get,
        itemLightBlueBackpack.get,
        itemYellowBackpack.get,
        itemLimeBackpack.get,
        itemPinkBackpack.get,
        itemGrayBackpack.get,
        itemLightGrayBackpack.get,
        itemCyanBackpack.get,
        itemPurpleBackpack.get,
        itemBlueBackpack.get,
        itemBrownBackpack.get,
        itemGreenBackpack.get,
        itemRedBackpack.get,
        itemBlackBackpack.get
    )

    /** Containers */
    val containerBackpack = CONTAINERS.register("container_type", () => ICCLContainerType.create((id, inv, _) => new ContainerBackpack(id, inv)))

    /** Block Tags */
    val tagBlockRubyOre = BlockTags.bind("forge:ores/ruby")
    val tagBlockSapphireOre = BlockTags.bind("forge:ores/sapphire")
    val tagBlockPeridotOre = BlockTags.bind("forge:ores/peridot")
    val tagBlockCopperOre = BlockTags.bind("forge:ores/copper")
    val tagBlockTinOre = BlockTags.bind("forge:ores/tin")
    val tagBlockSilverOre = BlockTags.bind("forge:ores/silver")
    val tagBlockElectrotineOre = BlockTags.bind("forge:ores/electrotine")

    val tagBlockMarble = BlockTags.bind("forge:stone/marble")
    val tagBlockBasalt = BlockTags.bind("forge:stone/basalt")
    val tagBlockRubyBlock = BlockTags.bind("forge:storage_blocks/ruby")
    val tagBlockSapphireBlock = BlockTags.bind("forge:storage_blocks/sapphire")
    val tagBlockPeridotBlock = BlockTags.bind("forge:storage_blocks/peridot")
    val tagBlockCopperBlock = BlockTags.bind("forge:storage_blocks/copper")
    val tagBlockTinBlock = BlockTags.bind("forge:storage_blocks/tin")
    val tagBlockSilverBlock = BlockTags.bind("forge:storage_blocks/silver")
    val tagBlockElectrotineBlock = BlockTags.bind("forge:storage_blocks/electrotine")

    /** Item Tags */
    val tagItemRubyOre = ItemTags.bind("forge:ores/ruby")
    val tagItemSapphireOre = ItemTags.bind("forge:ores/sapphire")
    val tagItemPeridotOre = ItemTags.bind("forge:ores/peridot")
    val tagItemCopperOre = ItemTags.bind("forge:ores/copper")
    val tagItemTinOre = ItemTags.bind("forge:ores/tin")
    val tagItemSilverOre = ItemTags.bind("forge:ores/silver")
    val tagItemElectrotineOre = ItemTags.bind("forge:ores/electrotine")

    val tagItemMarble = ItemTags.bind("forge:stone/marble")
    val tagItemBasalt = ItemTags.bind("forge:stone/basalt")
    val tagItemRubyBlock = ItemTags.bind("forge:storage_blocks/ruby")
    val tagItemSapphireBlock = ItemTags.bind("forge:storage_blocks/sapphire")
    val tagItemPeridotBlock = ItemTags.bind("forge:storage_blocks/peridot")
    val tagItemCopperBlock = ItemTags.bind("forge:storage_blocks/copper")
    val tagItemTinBlock = ItemTags.bind("forge:storage_blocks/tin")
    val tagItemSilverBlock = ItemTags.bind("forge:storage_blocks/silver")
    val tagItemElectrotineBlock = ItemTags.bind("forge:storage_blocks/electrotine")

    val tagBackpacks = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks"))
    val tagBackpacksWhite = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/white"))
    val tagBackpacksOrange = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/orange"))
    val tagBackpacksMagenta = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/magenta"))
    val tagBackpacksLightBlue = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/light_blue"))
    val tagBackpacksYellow = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/yellow"))
    val tagBackpacksLime = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/lime"))
    val tagBackpacksPink = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/pink"))
    val tagBackpacksGray = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/gray"))
    val tagBackpacksLightGray = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/light_gray"))
    val tagBackpacksCyan = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/cyan"))
    val tagBackpacksPurple = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/purple"))
    val tagBackpacksBlue = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/blue"))
    val tagBackpacksBrown = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/brown"))
    val tagBackpacksGreen = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/green"))
    val tagBackpacksRed = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/red"))
    val tagBackpacksBlack = ItemTags.bind(new ResourceLocation(MOD_ID, "backpacks/black"))
    val tagBackpackDisallowed = ItemTags.bind(new ResourceLocation(MOD_ID, "backpack/disallowed"))

    private def makeOreBlock(harvestLevel: Int = 1, minXP: Int = 0, maxXP: Int = 0): Supplier[Block] = () =>
        new BlockOre(
            AbstractBlock.Properties.of(Material.STONE)
                .strength(3.0F, 5.0F)
                .harvestLevel(harvestLevel)
                .harvestTool(ToolType.PICKAXE),
            minXP,
            maxXP
        )

    private def makeDecorativeBlock(harvestLevel: Int = 2, hardness: Float = 5.0F, resistance: Float = 10.0F): Supplier[Block] = () =>
        new Block(
            AbstractBlock.Properties.of(Material.STONE)
                .strength(hardness, resistance)
                .harvestLevel(harvestLevel)
                .harvestTool(ToolType.PICKAXE)
        )

    private def makeDecorativeWallBlock(block: Supplier[Block]): Supplier[WallBlock] = () =>
        new WallBlock(AbstractBlock.Properties.copy(block))

    private def makeItemBlock[T <: Block](block: Supplier[T]): Supplier[Item] = () =>
        new BlockItem(block.get(), new Item.Properties().tab(explorationItemGroup))


    private def makeItem(make: Item.Properties => Item): Supplier[Item] = () =>
        make(new Item.Properties().tab(explorationItemGroup))


    def register(bus: IEventBus) {
        LOCK.lock()
        ITEMS.register(bus)
        BLOCKS.register(bus)
        CONTAINERS.register(bus)
        bus.register(DataGen)
    }
}

private object DataGen {

    @SubscribeEvent
    def gatherDataGenerators(event: GatherDataEvent) {
        val gen = event.getGenerator
        val helper = event.getExistingFileHelper
        if (event.includeClient) {
            gen.addProvider(new BlockStates(gen, helper))
            gen.addProvider(new ItemModels(gen, helper))
        }
        if (event.includeServer) {
            gen.addProvider(new BlockTags(gen, helper))
            gen.addProvider(new ItemTags(gen, helper))
            gen.addProvider(new BlockLootTables(gen))
            gen.addProvider(new Recipes(gen))
        }
    }
}

private class BlockStates(gen: DataGenerator, fileHelper: ExistingFileHelper) extends BlockStateProvider(gen, MOD_ID, fileHelper) {
    override def getName = "ProjectRed-Exploration BlockStates."

    override protected def registerStatesAndModels() {
        simpleBlock(blockRubyOre)
        simpleBlock(blockSapphireOre)
        simpleBlock(blockPeridotOre)
        simpleBlock(blockCopperOre)
        simpleBlock(blockTinOre)
        simpleBlock(blockSilverOre)
        simpleBlock(blockElectrotineOre)

        simpleBlock(blockMarble)
        simpleBlock(blockMarbleBrick)
        simpleBlock(blockBasalt)
        simpleBlock(blockBasaltCobble)
        simpleBlock(blockBasaltBrick)
        simpleBlock(blockRubyBlock)
        simpleBlock(blockSapphireBlock)
        simpleBlock(blockPeridotBlock)
        simpleBlock(blockCopperBlock)
        simpleBlock(blockTinBlock)
        simpleBlock(blockSilverBlock)
        simpleBlock(blockElectrotineBlock)

        wallBlock(blockMarbleWall, blockTexture(blockMarble))
        wallBlock(blockMarbleBrickWall, blockTexture(blockMarbleBrick))
        wallBlock(blockBasaltWall, blockTexture(blockBasalt))
        wallBlock(blockBasaltCobbleWall, blockTexture(blockBasaltCobble))
        wallBlock(blockBasaltBrickWall, blockTexture(blockBasaltBrick))
        wallBlock(blockRubyBlockWall, blockTexture(blockRubyBlock))
        wallBlock(blockSapphireBlockWall, blockTexture(blockSapphireBlock))
        wallBlock(blockPeridotBlockWall, blockTexture(blockPeridotBlock))
        wallBlock(blockCopperBlockWall, blockTexture(blockCopperBlock))
        wallBlock(blockTinBlockWall, blockTexture(blockTinBlock))
        wallBlock(blockSilverBlockWall, blockTexture(blockSilverBlock))
        wallBlock(blockElectrotineBlockWall, blockTexture(blockElectrotineBlock))
    }
}

private class ItemModels(gen: DataGenerator, fileHelper: ExistingFileHelper) extends ItemModelProvider(gen, MOD_ID, fileHelper) {
    override def getName = "ProjectRed-Exploration Item Models."

    override protected def registerModels() {
        simpleItemBlock(blockRubyOre)
        simpleItemBlock(blockSapphireOre)
        simpleItemBlock(blockPeridotOre)
        simpleItemBlock(blockCopperOre)
        simpleItemBlock(blockTinOre)
        simpleItemBlock(blockSilverOre)
        simpleItemBlock(blockElectrotineOre)

        simpleItemBlock(blockMarble)
        simpleItemBlock(blockMarbleBrick)
        simpleItemBlock(blockBasalt)
        simpleItemBlock(blockBasaltCobble)
        simpleItemBlock(blockBasaltBrick)
        simpleItemBlock(blockRubyBlock)
        simpleItemBlock(blockSapphireBlock)
        simpleItemBlock(blockPeridotBlock)
        simpleItemBlock(blockCopperBlock)
        simpleItemBlock(blockTinBlock)
        simpleItemBlock(blockSilverBlock)
        simpleItemBlock(blockElectrotineBlock)


        getSimple(blockMarbleWall)
            .texture(null)
            .parent(wallInventory(blockMarbleWall.getRegistryName + "_inventory", blockTexture(blockMarble)))
        getSimple(blockMarbleBrickWall)
            .texture(null)
            .parent(wallInventory(blockMarbleBrickWall.getRegistryName + "_inventory", blockTexture(blockMarbleBrick)))
        getSimple(blockBasaltWall)
            .texture(null)
            .parent(wallInventory(blockBasaltWall.getRegistryName + "_inventory", blockTexture(blockBasalt)))
        getSimple(blockBasaltCobbleWall)
            .texture(null)
            .parent(wallInventory(blockBasaltCobbleWall.getRegistryName + "_inventory", blockTexture(blockBasaltCobble)))
        getSimple(blockBasaltBrickWall)
            .texture(null)
            .parent(wallInventory(blockBasaltBrickWall.getRegistryName + "_inventory", blockTexture(blockBasaltBrick)))
        getSimple(blockRubyBlockWall)
            .texture(null)
            .parent(wallInventory(blockRubyBlockWall.getRegistryName + "_inventory", blockTexture(blockRubyBlock)))
        getSimple(blockSapphireBlockWall)
            .texture(null)
            .parent(wallInventory(blockSapphireBlockWall.getRegistryName + "_inventory", blockTexture(blockSapphireBlock)))
        getSimple(blockPeridotBlockWall)
            .texture(null)
            .parent(wallInventory(blockPeridotBlockWall.getRegistryName + "_inventory", blockTexture(blockPeridotBlock)))
        getSimple(blockCopperBlockWall)
            .texture(null)
            .parent(wallInventory(blockCopperBlockWall.getRegistryName + "_inventory", blockTexture(blockCopperBlock)))
        getSimple(blockTinBlockWall)
            .texture(null)
            .parent(wallInventory(blockTinBlockWall.getRegistryName + "_inventory", blockTexture(blockTinBlock)))
        getSimple(blockSilverBlockWall)
            .texture(null)
            .parent(wallInventory(blockSilverBlockWall.getRegistryName + "_inventory", blockTexture(blockSilverBlock)))
        getSimple(blockElectrotineBlockWall)
            .texture(null)
            .parent(wallInventory(blockElectrotineBlockWall.getRegistryName + "_inventory", blockTexture(blockElectrotineBlock)))

        generated(itemWoolGin)
        handheld(itemAthame)

        generated(itemWhiteBackpack).folder("backpack")
        generated(itemOrangeBackpack).folder("backpack")
        generated(itemMagentaBackpack).folder("backpack")
        generated(itemLightBlueBackpack).folder("backpack")
        generated(itemYellowBackpack).folder("backpack")
        generated(itemLimeBackpack).folder("backpack")
        generated(itemPinkBackpack).folder("backpack")
        generated(itemGrayBackpack).folder("backpack")
        generated(itemLightGrayBackpack).folder("backpack")
        generated(itemCyanBackpack).folder("backpack")
        generated(itemPurpleBackpack).folder("backpack")
        generated(itemBlueBackpack).folder("backpack")
        generated(itemBrownBackpack).folder("backpack")
        generated(itemGreenBackpack).folder("backpack")
        generated(itemRedBackpack).folder("backpack")
        generated(itemBlackBackpack).folder("backpack")

        handheld(itemRubyAxe)
        handheld(itemSapphireAxe)
        handheld(itemPeridotAxe)

        handheld(itemRubyHoe)
        handheld(itemSapphireHoe)
        handheld(itemPeridotHoe)

        handheld(itemRubyPickaxe)
        handheld(itemSapphirePickaxe)
        handheld(itemPeridotPickaxe)

        handheld(itemRubyShovel)
        handheld(itemSapphireShovel)
        handheld(itemPeridotShovel)

        handheld(itemRubySword)
        handheld(itemSapphireSword)
        handheld(itemPeridotSword)

        handheld(itemGoldSaw)
        handheld(itemRubySaw)
        handheld(itemSapphireSaw)
        handheld(itemPeridotSaw)

        handheld(itemWoodSickle)
        handheld(itemStoneSickle)
        handheld(itemIronSickle)
        handheld(itemGoldSickle)
        handheld(itemRubySickle)
        handheld(itemSapphireSickle)
        handheld(itemPeridotSickle)
        handheld(itemDiamondSickle)

        generated(itemRubyHelmet)
        generated(itemRubyChestplate)
        generated(itemRubyLeggings)
        generated(itemRubyBoots)

        generated(itemSapphireHelmet)
        generated(itemSapphireChestplate)
        generated(itemSapphireLeggings)
        generated(itemSapphireBoots)

        generated(itemPeridotHelmet)
        generated(itemPeridotChestplate)
        generated(itemPeridotLeggings)
        generated(itemPeridotBoots)
    }
}

private class BlockTags(gen:DataGenerator, fileHelper:ExistingFileHelper) extends BlockTagsProvider(gen, MOD_ID, fileHelper) {
    override def getName = "ProjectRed-Exploration Block Tags."

    override protected def addTags() {
        tag(BlockTags.WALLS).add(blockMarbleWall)
        tag(BlockTags.WALLS).add(blockMarbleBrickWall)
        tag(BlockTags.WALLS).add(blockBasaltWall)
        tag(BlockTags.WALLS).add(blockBasaltCobbleWall)
        tag(BlockTags.WALLS).add(blockBasaltBrickWall)
        tag(BlockTags.WALLS).add(blockRubyBlockWall)
        tag(BlockTags.WALLS).add(blockSapphireBlockWall)
        tag(BlockTags.WALLS).add(blockPeridotBlockWall)
        tag(BlockTags.WALLS).add(blockCopperBlockWall)
        tag(BlockTags.WALLS).add(blockTinBlockWall)
        tag(BlockTags.WALLS).add(blockSilverBlockWall)
        tag(BlockTags.WALLS).add(blockElectrotineBlockWall)

        tag(ForgeBlockTags.ORES)
            .addTag(tagBlockRubyOre)
            .addTag(tagBlockSapphireOre)
            .addTag(tagBlockPeridotOre)
            .addTag(tagBlockCopperOre)
            .addTag(tagBlockTinOre)
            .addTag(tagBlockSilverOre)
            .addTag(tagBlockElectrotineOre)
        tag(tagBlockRubyOre).add(blockRubyOre)
        tag(tagBlockSapphireOre).add(blockSapphireOre)
        tag(tagBlockPeridotOre).add(blockPeridotOre)
        tag(tagBlockCopperOre).add(blockCopperOre)
        tag(tagBlockTinOre).add(blockTinOre)
        tag(tagBlockSilverOre).add(blockSilverOre)
        tag(tagBlockElectrotineOre).add(blockElectrotineOre)

        tag(ForgeBlockTags.STORAGE_BLOCKS)
            .addTag(tagBlockMarble)
            .addTag(tagBlockBasalt)
            .addTag(tagBlockRubyBlock)
            .addTag(tagBlockSapphireBlock)
            .addTag(tagBlockPeridotBlock)
            .addTag(tagBlockCopperBlock)
            .addTag(tagBlockTinBlock)
            .addTag(tagBlockSilverBlock)
            .addTag(tagBlockElectrotineBlock)
        tag(tagBlockMarble).add(blockMarble)
        tag(tagBlockBasalt).add(blockBasalt)
        tag(tagBlockRubyBlock).add(blockRubyBlock)
        tag(tagBlockSapphireBlock).add(blockSapphireBlock)
        tag(tagBlockPeridotBlock).add(blockPeridotBlock)
        tag(tagBlockCopperBlock).add(blockCopperBlock)
        tag(tagBlockTinBlock).add(blockTinBlock)
        tag(tagBlockSilverBlock).add(blockSilverBlock)
        tag(tagBlockElectrotineBlock).add(blockElectrotineBlock)
    }
}

private class ItemTags(gen:DataGenerator, fileHelper:ExistingFileHelper) extends ItemTagsProvider(gen, new BlockTagsProvider(gen, MOD_ID, fileHelper), MOD_ID, fileHelper) {
    override def getName = "ProjectRed-Exploration Item Tags."

    override protected def addTags() {
        tag(ForgeItemTags.ORES)
            .addTag(tagItemRubyOre)
            .addTag(tagItemSapphireOre)
            .addTag(tagItemPeridotOre)
            .addTag(tagItemCopperOre)
            .addTag(tagItemTinOre)
            .addTag(tagItemSilverOre)
            .addTag(tagItemElectrotineOre)
        tag(tagItemRubyOre).add(itemRubyOre)
        tag(tagItemSapphireOre).add(itemSapphireOre)
        tag(tagItemPeridotOre).add(itemPeridotOre)
        tag(tagItemCopperOre).add(itemCopperOre)
        tag(tagItemTinOre).add(itemTinOre)
        tag(tagItemSilverOre).add(itemSilverOre)
        tag(tagItemElectrotineOre).add(itemElectrotineOre)

        tag(ForgeItemTags.STORAGE_BLOCKS)
            .addTag(tagItemMarble)
            .addTag(tagItemBasalt)
            .addTag(tagItemRubyBlock)
            .addTag(tagItemSapphireBlock)
            .addTag(tagItemPeridotBlock)
            .addTag(tagItemCopperBlock)
            .addTag(tagItemTinBlock)
            .addTag(tagItemSilverBlock)
            .addTag(tagItemElectrotineBlock)
        tag(tagItemMarble).add(itemMarble)
        tag(tagItemBasalt).add(itemBasalt)
        tag(tagItemRubyBlock).add(itemRubyBlock)
        tag(tagItemSapphireBlock).add(itemSapphireBlock)
        tag(tagItemPeridotBlock).add(itemPeridotBlock)
        tag(tagItemCopperBlock).add(itemCopperBlock)
        tag(tagItemTinBlock).add(itemTinBlock)
        tag(tagItemSilverBlock).add(itemSilverBlock)
        tag(tagItemElectrotineBlock).add(itemElectrotineBlock)

        tag(tagBackpacks)
            .addTag(tagBackpacksWhite)
            .addTag(tagBackpacksOrange)
            .addTag(tagBackpacksMagenta)
            .addTag(tagBackpacksLightBlue)
            .addTag(tagBackpacksYellow)
            .addTag(tagBackpacksLime)
            .addTag(tagBackpacksPink)
            .addTag(tagBackpacksGray)
            .addTag(tagBackpacksLightGray)
            .addTag(tagBackpacksCyan)
            .addTag(tagBackpacksPurple)
            .addTag(tagBackpacksBlue)
            .addTag(tagBackpacksBrown)
            .addTag(tagBackpacksGreen)
            .addTag(tagBackpacksRed)
            .addTag(tagBackpacksBlack)
        tag(tagBackpacksWhite).add(itemWhiteBackpack)
        tag(tagBackpacksOrange).add(itemOrangeBackpack)
        tag(tagBackpacksMagenta).add(itemMagentaBackpack)
        tag(tagBackpacksLightBlue).add(itemLightBlueBackpack)
        tag(tagBackpacksYellow).add(itemYellowBackpack)
        tag(tagBackpacksLime).add(itemLimeBackpack)
        tag(tagBackpacksPink).add(itemPinkBackpack)
        tag(tagBackpacksGray).add(itemGrayBackpack)
        tag(tagBackpacksLightGray).add(itemLightGrayBackpack)
        tag(tagBackpacksCyan).add(itemCyanBackpack)
        tag(tagBackpacksPurple).add(itemPurpleBackpack)
        tag(tagBackpacksBlue).add(itemBlueBackpack)
        tag(tagBackpacksBrown).add(itemBrownBackpack)
        tag(tagBackpacksGreen).add(itemGreenBackpack)
        tag(tagBackpacksRed).add(itemRedBackpack)
        tag(tagBackpacksBlack).add(itemBlackBackpack)

        tag(tagBackpackDisallowed).addTag(tagBackpacks)
    }
}

private class BlockLootTables(gen: DataGenerator) extends LootTableProvider.BlockLootProvider(gen) {
    override def getName = "ProjectRed-Exploration Block LootTables."

    override protected def registerTables() {
        register(blockRubyOre, valueRangeOrSilkWithFortune(blockRubyOre, itemRuby, 1, 4))
        register(blockSapphireOre, valueRangeOrSilkWithFortune(blockSapphireOre, itemSapphire, 1, 4))
        register(blockPeridotOre, valueRangeOrSilkWithFortune(blockPeridotOre, itemPeridot, 1, 4))

        register(blockCopperOre, singleItem(blockCopperOre))
        register(blockTinOre, singleItem(blockTinOre))
        register(blockSilverOre, singleItem(blockSilverOre))
        register(blockElectrotineOre, valueRangeOrSilkWithFortune(blockElectrotineOre, itemElectrotineDust, 1, 8))

        register(blockMarble, singleItem(blockMarble))
        register(blockMarbleBrick, singleItem(blockMarbleBrick))
        register(blockBasalt, singleItemOrSilk(blockBasalt, blockBasaltCobble))
        register(blockBasaltCobble, singleItem(blockBasaltCobble))
        register(blockBasaltBrick, singleItem(blockBasaltBrick))
        register(blockRubyBlock, singleItem(blockRubyBlock))
        register(blockSapphireBlock, singleItem(blockSapphireBlock))
        register(blockPeridotBlock, singleItem(blockPeridotBlock))
        register(blockCopperBlock, singleItem(blockCopperBlock))
        register(blockTinBlock, singleItem(blockTinBlock))
        register(blockSilverBlock, singleItem(blockSilverBlock))
        register(blockElectrotineBlock, singleItem(blockElectrotineBlock))

        register(blockMarbleWall, singleItem(blockMarbleWall))
        register(blockMarbleBrickWall, singleItem(blockMarbleBrickWall))
        register(blockBasaltWall, singleItem(blockBasaltWall))
        register(blockBasaltCobbleWall, singleItem(blockBasaltCobbleWall))
        register(blockBasaltBrickWall, singleItem(blockBasaltBrickWall))
        register(blockRubyBlockWall, singleItem(blockRubyBlockWall))
        register(blockSapphireBlockWall, singleItem(blockSapphireBlockWall))
        register(blockPeridotBlockWall, singleItem(blockPeridotBlockWall))
        register(blockCopperBlockWall, singleItem(blockCopperBlockWall))
        register(blockTinBlockWall, singleItem(blockTinBlockWall))
        register(blockSilverBlockWall, singleItem(blockSilverBlockWall))
        register(blockElectrotineBlockWall, singleItem(blockElectrotineBlockWall))
    }
}

private class Recipes(gen: DataGenerator) extends RecipeProvider(gen) {
    override def getName = "ProjectRed-Exploration Recipes."

    override protected def registerRecipes() {
        smelting(itemBasalt, 1)
            .ingredient(itemBasaltCobble)
            .experience(0.1F)

        smelting(itemRuby, 1, new ResourceLocation(MOD_ID, "ruby_from_ore"))
            .ingredient(tagItemRubyOre)
            .experience(1F)

        smelting(itemSapphire, 1, new ResourceLocation(MOD_ID, "sapphire_from_ore"))
            .ingredient(tagItemSapphireOre)
            .experience(1F)

        smelting(itemPeridot, 1, new ResourceLocation(MOD_ID, "peridot_from_ore"))
            .ingredient(tagItemPeridotOre)
            .experience(1F)

        smelting(itemCopperIngot, 1, new ResourceLocation(MOD_ID, "copper_from_ore"))
            .ingredient(tagItemCopperOre)
            .experience(0.7F)

        smelting(itemTinIngot, 1, new ResourceLocation(MOD_ID, "tin_from_ore"))
            .ingredient(tagItemTinOre)
            .experience(0.7F)

        smelting(itemSilverIngot, 1, new ResourceLocation(MOD_ID, "silver_from_ore"))
            .ingredient(tagItemSilverOre)
            .experience(0.8F)

        smelting(itemElectrotineDust, 1, new ResourceLocation(MOD_ID, "electrotine_from_ore"))
            .ingredient(tagItemElectrotineOre)
            .experience(0.7F)

        shapedRecipe(blockMarbleBrick, 4)
            .key('B', tagItemMarble)
            .patternLine("BB")
            .patternLine("BB")

        shapedRecipe(blockBasaltBrick, 4)
            .key('B', tagItemBasalt)
            .patternLine("BB")
            .patternLine("BB")

        shapedRecipe(blockRubyBlock)
            .key('S', tagGemsRuby)
            .patternLine("SSS")
            .patternLine("SSS")
            .patternLine("SSS")

        shapelessRecipe(itemRuby, 9, new ResourceLocation(MOD_ID, "ruby_from_block"))
            .addIngredient(tagItemRubyBlock)

        shapedRecipe(blockSapphireBlock)
            .key('S', tagGemsSapphire)
            .patternLine("SSS")
            .patternLine("SSS")
            .patternLine("SSS")

        shapelessRecipe(itemSapphire, 9, new ResourceLocation(MOD_ID, "sapphire_from_block"))
            .addIngredient(tagItemSapphireBlock)

        shapedRecipe(blockPeridotBlock)
            .key('S', tagGemsPeridot)
            .patternLine("SSS")
            .patternLine("SSS")
            .patternLine("SSS")

        shapelessRecipe(itemPeridot, 9, new ResourceLocation(MOD_ID, "peridot_from_block"))
            .addIngredient(tagItemPeridotBlock)

        shapedRecipe(blockCopperBlock)
            .key('S', tagIngotsCopper)
            .patternLine("SSS")
            .patternLine("SSS")
            .patternLine("SSS")

        shapelessRecipe(itemCopperIngot, 9, new ResourceLocation(MOD_ID, "copper_from_block"))
            .addIngredient(tagItemCopperBlock)

        shapedRecipe(blockTinBlock)
            .key('S', tagIngotsTin)
            .patternLine("SSS")
            .patternLine("SSS")
            .patternLine("SSS")

        shapelessRecipe(itemTinIngot, 9, new ResourceLocation(MOD_ID, "tin_from_block"))
            .addIngredient(tagItemTinBlock)

        shapedRecipe(blockSilverBlock)
            .key('S', tagIngotsSilver)
            .patternLine("SSS")
            .patternLine("SSS")
            .patternLine("SSS")

        shapelessRecipe(itemSilverIngot, 9, new ResourceLocation(MOD_ID, "silver_from_block"))
            .addIngredient(tagItemSilverBlock)

        shapedRecipe(blockElectrotineBlock)
            .key('S', tagDustsElectrotine)
            .patternLine("SSS")
            .patternLine("SSS")
            .patternLine("SSS")

        shapelessRecipe(itemElectrotineDust, 9, new ResourceLocation(MOD_ID, "electrotine_from_block"))
            .addIngredient(tagItemElectrotineBlock)

        shapedRecipe(blockMarbleWall, 6)
            .key('S', tagItemMarble)
            .patternLine("SSS")
            .patternLine("SSS")

        shapedRecipe(blockMarbleBrickWall, 6)
            .key('S', blockMarbleBrick)
            .patternLine("SSS")
            .patternLine("SSS")

        shapedRecipe(blockBasaltWall, 6)
            .key('S', blockBasalt)
            .patternLine("SSS")
            .patternLine("SSS")

        shapedRecipe(blockBasaltCobbleWall, 6)
            .key('S', blockBasaltCobble)
            .patternLine("SSS")
            .patternLine("SSS")

        shapedRecipe(blockBasaltBrickWall, 6)
            .key('S', blockBasaltBrick)
            .patternLine("SSS")
            .patternLine("SSS")

        shapedRecipe(blockRubyBlockWall, 6)
            .key('S', blockRubyBlock)
            .patternLine("SSS")
            .patternLine("SSS")

        shapedRecipe(blockSapphireBlockWall, 6)
            .key('S', blockSapphireBlock)
            .patternLine("SSS")
            .patternLine("SSS")

        shapedRecipe(blockPeridotBlockWall, 6)
            .key('S', blockPeridotBlock)
            .patternLine("SSS")
            .patternLine("SSS")

        shapedRecipe(blockCopperBlockWall, 6)
            .key('S', blockCopperBlock)
            .patternLine("SSS")
            .patternLine("SSS")

        shapedRecipe(blockTinBlockWall, 6)
            .key('S', blockTinBlock)
            .patternLine("SSS")
            .patternLine("SSS")

        shapedRecipe(blockSilverBlockWall, 6)
            .key('S', blockSilverBlock)
            .patternLine("SSS")
            .patternLine("SSS")

        shapedRecipe(blockElectrotineBlockWall, 6)
            .key('S', blockElectrotineBlock)
            .patternLine("SSS")
            .patternLine("SSS")

        shapedRecipe(itemWoolGin)
            .key('S', ForgeItemTags.RODS_WOODEN)
            .key('I', itemIronCoil)
            .patternLine("SIS")
            .patternLine("SSS")
            .patternLine(" S ")

        shapedRecipe(Items.STRING, 4, new ResourceLocation(MOD_ID, "string_from_wool"))
            .key('W', ItemTags.WOOL)
            .key('G', itemWoolGin)
            .patternLine("GW")

        shapedRecipe(itemAthame)
            .key('W', ForgeItemTags.RODS_WOODEN)
            .key('S', tagIngotsSilver)
            .patternLine("S")
            .patternLine("W")

        EnumColour.values().foreach(addBackpackRecipe)

        addAxeRecipe(itemRubyAxe, tagGemsRuby)
        addAxeRecipe(itemSapphireAxe, tagGemsSapphire)
        addAxeRecipe(itemPeridotAxe, tagGemsPeridot)

        addHoeRecipe(itemRubyHoe, tagGemsRuby)
        addHoeRecipe(itemSapphireHoe, tagGemsSapphire)
        addHoeRecipe(itemPeridotHoe, tagGemsPeridot)

        addPickaxeRecipe(itemRubyPickaxe, tagGemsRuby)
        addPickaxeRecipe(itemSapphirePickaxe, tagGemsSapphire)
        addPickaxeRecipe(itemPeridotPickaxe, tagGemsPeridot)

        addShovelRecipe(itemRubyShovel, tagGemsRuby)
        addShovelRecipe(itemSapphireShovel, tagGemsSapphire)
        addShovelRecipe(itemPeridotShovel, tagGemsPeridot)

        addSwordRecipe(itemRubySword, tagGemsRuby)
        addSwordRecipe(itemSapphireSword, tagGemsSapphire)
        addSwordRecipe(itemPeridotSword, tagGemsPeridot)

        addSawRecipe(itemGoldSaw, ForgeItemTags.INGOTS_GOLD)
        addSawRecipe(itemRubySaw, tagGemsRuby)
        addSawRecipe(itemSapphireSaw, tagGemsSapphire)
        addSawRecipe(itemPeridotSaw, tagGemsPeridot)

        addSickleRecipe(itemWoodSickle, ItemTags.PLANKS)
        addSickleRecipe(itemStoneSickle, Items.FLINT)
        addSickleRecipe(itemIronSickle, ForgeItemTags.INGOTS_IRON)
        addSickleRecipe(itemGoldSickle, ForgeItemTags.INGOTS_GOLD)
        addSickleRecipe(itemRubySickle, tagGemsRuby)
        addSickleRecipe(itemSapphireSickle, tagGemsSapphire)
        addSickleRecipe(itemPeridotSickle, tagGemsPeridot)
        addSickleRecipe(itemDiamondSickle, ForgeItemTags.GEMS_DIAMOND)

        addHelmetRecipe(itemRubyHelmet, tagGemsRuby)
        addHelmetRecipe(itemSapphireHelmet, tagGemsSapphire)
        addHelmetRecipe(itemPeridotHelmet, tagGemsPeridot)

        addChestplateRecipe(itemRubyChestplate, tagGemsRuby)
        addChestplateRecipe(itemSapphireChestplate, tagGemsSapphire)
        addChestplateRecipe(itemPeridotChestplate, tagGemsPeridot)

        addLeggingsRecipe(itemRubyLeggings, tagGemsRuby)
        addLeggingsRecipe(itemSapphireLeggings, tagGemsSapphire)
        addLeggingsRecipe(itemPeridotLeggings, tagGemsPeridot)

        addBootsRecipe(itemRubyBoots, tagGemsRuby)
        addBootsRecipe(itemSapphireBoots, tagGemsSapphire)
        addBootsRecipe(itemPeridotBoots, tagGemsPeridot)
    }

    def addBackpackRecipe(colour: EnumColour) {
        val b = shapedRecipe(backpacks(colour.ordinal))
            .key('C', itemWovenCloth)
            .patternLine("CCC")
        if (colour != EnumColour.WHITE) {
            b.key('D', ItemTags.bind(colour.getDyeTagName))
            b.patternLine("CDC")
        } else {
            b.patternLine("C C")
        }
        b.patternLine("CCC")

        builder(ShapelessNBTCopyRecipeBuilder(backpacks(colour.ordinal), 1, new ResourceLocation(MOD_ID, colour.getSerializedName + "_backpack_recolor")))
            .addIngredient(tagBackpacks)
            .addIngredient(ItemTags.bind(colour.getDyeTagName))
    }

    def addAxeRecipe(result: Item, material: ITag[Item]) {
        shapedRecipe(result)
            .key('M', material)
            .key('S', ForgeItemTags.RODS_WOODEN)
            .patternLine("MM ")
            .patternLine("MS ")
            .patternLine(" S ")
    }

    def addHoeRecipe(result: Item, material: ITag[Item]) {
        shapedRecipe(result)
            .key('M', material)
            .key('S', ForgeItemTags.RODS_WOODEN)
            .patternLine("MM ")
            .patternLine(" S ")
            .patternLine(" S ")
    }

    def addPickaxeRecipe(result: Item, material: ITag[Item]) {
        shapedRecipe(result)
            .key('M', material)
            .key('S', ForgeItemTags.RODS_WOODEN)
            .patternLine("MMM")
            .patternLine(" S ")
            .patternLine(" S ")
    }

    def addShovelRecipe(result: Item, material: ITag[Item]) {
        shapedRecipe(result)
            .key('M', material)
            .key('S', ForgeItemTags.RODS_WOODEN)
            .patternLine(" M ")
            .patternLine(" S ")
            .patternLine(" S ")
    }

    def addSwordRecipe(result: Item, material: ITag[Item]) {
        shapedRecipe(result)
            .key('M', material)
            .key('S', ForgeItemTags.RODS_WOODEN)
            .patternLine(" M ")
            .patternLine(" M ")
            .patternLine(" S ")
    }

    def addSawRecipe(result: Item, material: ITag[Item]) {
        shapedRecipe(result)
            .key('M', material)
            .key('S', ForgeItemTags.RODS_WOODEN)
            .key('R', MicroblockModContent.stoneRodTag)
            .patternLine("SRR")
            .patternLine("SMM")
    }

    def addSickleRecipe(result: Item, material: ITag[Item]) {
        shapedRecipe(result)
            .key('M', material)
            .key('S', ForgeItemTags.RODS_WOODEN)
            .patternLine(" M ")
            .patternLine("  M")
            .patternLine("SM ")
    }

    def addSickleRecipe(result: Item, material: Item) {
        shapedRecipe(result)
            .key('M', material)
            .key('S', ForgeItemTags.RODS_WOODEN)
            .patternLine(" M ")
            .patternLine("  M")
            .patternLine("SM ")
    }

    def addHelmetRecipe(result: Item, material: ITag[Item]) {
        shapedRecipe(result)
            .key('M', material)
            .patternLine("MMM")
            .patternLine("M M")
    }

    def addChestplateRecipe(result: Item, material: ITag[Item]) {
        shapedRecipe(result)
            .key('M', material)
            .patternLine("M M")
            .patternLine("MMM")
            .patternLine("MMM")
    }

    def addLeggingsRecipe(result: Item, material: ITag[Item]) {
        shapedRecipe(result)
            .key('M', material)
            .patternLine("MMM")
            .patternLine("M M")
            .patternLine("M M")
    }

    def addBootsRecipe(result: Item, material: ITag[Item]) {
        shapedRecipe(result)
            .key('M', material)
            .patternLine("M M")
            .patternLine("M M")
    }
}
