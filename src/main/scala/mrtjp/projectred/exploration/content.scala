package mrtjp.projectred.exploration

import codechicken.lib.datagen.recipe.RecipeProvider
import codechicken.lib.datagen.{ItemModelProvider, LootTableProvider}
import codechicken.lib.gui.SimpleItemGroup
import codechicken.lib.inventory.container.ICCLContainerType
import codechicken.lib.item.{SimpleArmorMaterial, SimpleItemTier}
import codechicken.lib.util.CrashLock
import mrtjp.projectred.ProjectRedExploration.MOD_ID
import mrtjp.projectred.core.CoreContent._
import mrtjp.projectred.exploration.ExplorationContent._
import net.minecraft.block.material.Material
import net.minecraft.block.{Block, Blocks, WallBlock}
import net.minecraft.data.{BlockTagsProvider, DataGenerator, ItemTagsProvider}
import net.minecraft.inventory.EquipmentSlotType
import net.minecraft.item._
import net.minecraft.item.crafting.Ingredient
import net.minecraft.tags.BlockTags.{Wrapper => BlockTag}
import net.minecraft.tags.ItemTags.{Wrapper => ItemTag}
import net.minecraft.tags.{BlockTags => MCBlockTags}
import net.minecraft.util.{ResourceLocation, SoundEvents}
import net.minecraftforge.client.model.generators.{BlockStateProvider, ExistingFileHelper}
import net.minecraftforge.common.Tags.{Blocks => ForgeBlockTags, Items => ForgeItemTags}
import net.minecraftforge.common.ToolType
import net.minecraftforge.eventbus.api.{IEventBus, SubscribeEvent}
import net.minecraftforge.fml.event.lifecycle.GatherDataEvent
import net.minecraftforge.registries.{DeferredRegister, ForgeRegistries}

import java.util.function.Supplier

object ExplorationContent {

    private val LOCK = new CrashLock("Already Initialized.")
    private val ITEMS = DeferredRegister.create(ForgeRegistries.ITEMS, MOD_ID)
    private val BLOCKS = DeferredRegister.create(ForgeRegistries.BLOCKS, MOD_ID)
    private val CONTAINERS = DeferredRegister.create(ForgeRegistries.CONTAINERS, MOD_ID)

    val explorationItemGroup = new SimpleItemGroup(MOD_ID, () => new ItemStack(Blocks.GRASS))

    /** Materials */
    val athameItemTier = SimpleItemTier.builder(ItemTier.DIAMOND)
        .maxUses(100)
        .enchantability(30)
        .repairMaterial(() => Ingredient.fromItems(itemSilverIngot))
        .build()

    val rubyItemTier = SimpleItemTier.builder()
        .maxUses(512)
        .efficiency(8.00F)
        .attackDamage(3.00F)
        .harvestLevel(2)
        .enchantability(10)
        .repairMaterial(() => Ingredient.fromItems(itemRuby))
        .build()
    val sapphireItemTier = SimpleItemTier.builder()
        .maxUses(512)
        .efficiency(8.00F)
        .attackDamage(3.00F)
        .harvestLevel(2)
        .enchantability(10)
        .repairMaterial(() => Ingredient.fromItems(itemSapphire))
        .build()
    val peridotItemTier = SimpleItemTier.builder()
        .maxUses(512)
        .efficiency(7.75F)
        .attackDamage(2.75F)
        .harvestLevel(2)
        .enchantability(14)
        .repairMaterial(() => Ingredient.fromItems(itemPeridot))
        .build()

    val rubyArmorMaterial = SimpleArmorMaterial.builder()
        .durabilityFactor(16)
        .damageReduction(Array(3, 6, 8, 3))
        .enchantability(10)
        .soundEvent(SoundEvents.ITEM_ARMOR_EQUIP_DIAMOND)
        .repairMaterial(() => Ingredient.fromItems(itemRuby))
        .textureName(MOD_ID + ":ruby")
        .toughness(1.25F)
        .build()

    val sapphireArmorMaterial = SimpleArmorMaterial.builder()
        .durabilityFactor(16)
        .damageReduction(Array(3, 6, 8, 3))
        .enchantability(10)
        .soundEvent(SoundEvents.ITEM_ARMOR_EQUIP_DIAMOND)
        .repairMaterial(() => Ingredient.fromItems(itemSapphire))
        .textureName(MOD_ID + ":sapphire")
        .toughness(1.25F)
        .build()

    val peridotArmorMaterial = SimpleArmorMaterial.builder()
        .durabilityFactor(14)
        .damageReduction(Array(3, 6, 8, 3))
        .enchantability(14)
        .soundEvent(SoundEvents.ITEM_ARMOR_EQUIP_DIAMOND)
        .repairMaterial(() => Ingredient.fromItems(itemPeridot))
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

    val itemRubyAxe = ITEMS.register("ruby_axe", makeItem(new AxeItem(rubyItemTier, 8.0F, -3.0F, _)))
    val itemSapphireAxe = ITEMS.register("sapphire_axe", makeItem(new AxeItem(sapphireItemTier, 8.0F, -3.0F, _)))
    val itemPeridotAxe = ITEMS.register("peridot_axe", makeItem(new AxeItem(peridotItemTier, 8.0F, -3.0F, _)))

    val itemRubyHoe = ITEMS.register("ruby_hoe", makeItem(new HoeItem(rubyItemTier, 0.0F, _)))
    val itemSapphireHoe = ITEMS.register("sapphire_hoe", makeItem(new HoeItem(sapphireItemTier, 0.0F, _)))
    val itemPeridotHoe = ITEMS.register("peridot_hoe", makeItem(new HoeItem(peridotItemTier, 0.25F, _)))

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
        itemWhiteBackpack,
        itemOrangeBackpack,
        itemMagentaBackpack,
        itemLightBlueBackpack,
        itemYellowBackpack,
        itemLimeBackpack,
        itemPinkBackpack,
        itemGrayBackpack,
        itemLightGrayBackpack,
        itemCyanBackpack,
        itemPurpleBackpack,
        itemBlueBackpack,
        itemBrownBackpack,
        itemGreenBackpack,
        itemRedBackpack,
        itemBlackBackpack
    ).map(_.get())

    /** Containers */
    val containerBackpack = CONTAINERS.register("container_type", () => ICCLContainerType.create((id, inv, _) => new ContainerBackpack(id, inv)))

    /** Block Tags */
    val tagBlockRubyOre = new BlockTag("forge:ores/ruby")
    val tagBlockSapphireOre = new BlockTag("forge:ores/sapphire")
    val tagBlockPeridotOre = new BlockTag("forge:ores/peridot")
    val tagBlockCopperOre = new BlockTag("forge:ores/copper")
    val tagBlockTinOre = new BlockTag("forge:ores/tin")
    val tagBlockSilverOre = new BlockTag("forge:ores/silver")
    val tagBlockElectrotineOre = new BlockTag("forge:ores/electrotine")

    val tagBlockMarble = new BlockTag("forge:stone/marble")
    val tagBlockBasalt = new BlockTag("forge:stone/basalt")
    val tagBlockRuby = new BlockTag("forge:storage_blocks/ruby")
    val tagBlockSapphire = new BlockTag("forge:storage_blocks/sapphire")
    val tagBlockPeridot = new BlockTag("forge:storage_blocks/peridot")
    val tagBlockCopper = new BlockTag("forge:storage_blocks/copper")
    val tagBlockTin = new BlockTag("forge:storage_blocks/tin")
    val tagBlockSilver = new BlockTag("forge:storage_blocks/silver")
    val tagBlockElectrotine = new BlockTag("forge:storage_blocks/electrotine")

    /** Item Tags */
    val tagItemRubyOre = new ItemTag("forge:ores/ruby")
    val tagItemSapphireOre = new ItemTag("forge:ores/sapphire")
    val tagItemPeridotOre = new ItemTag("forge:ores/peridot")
    val tagItemCopperOre = new ItemTag("forge:ores/copper")
    val tagItemTinOre = new ItemTag("forge:ores/tin")
    val tagItemSilverOre = new ItemTag("forge:ores/silver")
    val tagItemElectrotineOre = new ItemTag("forge:ores/electrotine")

    val tagItemMarble = new ItemTag("forge:stone/marble")
    val tagItemBasalt = new ItemTag("forge:stone/basalt")
    val tagItemRuby = new ItemTag("forge:storage_blocks/ruby")
    val tagItemSapphire = new ItemTag("forge:storage_blocks/sapphire")
    val tagItemPeridot = new ItemTag("forge:storage_blocks/peridot")
    val tagItemCopper = new ItemTag("forge:storage_blocks/copper")
    val tagItemTin = new ItemTag("forge:storage_blocks/tin")
    val tagItemSilver = new ItemTag("forge:storage_blocks/silver")
    val tagItemElectrotine = new ItemTag("forge:storage_blocks/electrotine")

    val tagBackpacks = new ItemTag(new ResourceLocation(MOD_ID, "backpacks"))
    val tagBackpacksWhite = new ItemTag(new ResourceLocation(MOD_ID, "backpacks/white"))
    val tagBackpacksOrange = new ItemTag(new ResourceLocation(MOD_ID, "backpacks/orange"))
    val tagBackpacksMagenta = new ItemTag(new ResourceLocation(MOD_ID, "backpacks/magenta"))
    val tagBackpacksLightBlue = new ItemTag(new ResourceLocation(MOD_ID, "backpacks/light_blue"))
    val tagBackpacksYellow = new ItemTag(new ResourceLocation(MOD_ID, "backpacks/yellow"))
    val tagBackpacksLime = new ItemTag(new ResourceLocation(MOD_ID, "backpacks/lime"))
    val tagBackpacksPink = new ItemTag(new ResourceLocation(MOD_ID, "backpacks/pink"))
    val tagBackpacksGray = new ItemTag(new ResourceLocation(MOD_ID, "backpacks/gray"))
    val tagBackpacksLightGray = new ItemTag(new ResourceLocation(MOD_ID, "backpacks/light_gray"))
    val tagBackpacksCyan = new ItemTag(new ResourceLocation(MOD_ID, "backpacks/cyan"))
    val tagBackpacksPurple = new ItemTag(new ResourceLocation(MOD_ID, "backpacks/purple"))
    val tagBackpacksBlue = new ItemTag(new ResourceLocation(MOD_ID, "backpacks/blue"))
    val tagBackpacksBrown = new ItemTag(new ResourceLocation(MOD_ID, "backpacks/brown"))
    val tagBackpacksGreen = new ItemTag(new ResourceLocation(MOD_ID, "backpacks/green"))
    val tagBackpacksRed = new ItemTag(new ResourceLocation(MOD_ID, "backpacks/red"))
    val tagBackpacksBlack = new ItemTag(new ResourceLocation(MOD_ID, "backpacks/black"))
    val tagBackpackDisallowed = new ItemTag(new ResourceLocation(MOD_ID, "backpack/disallowed"))

    private def makeOreBlock(harvestLevel: Int = 1, minXP: Int = 0, maxXP: Int = 0): Supplier[Block] = () =>
        new BlockOre(
            Block.Properties.create(Material.ROCK)
                .hardnessAndResistance(3.0F, 5.0F)
                .harvestLevel(harvestLevel)
                .harvestTool(ToolType.PICKAXE),
            minXP,
            maxXP
        )

    private def makeDecorativeBlock(harvestLevel: Int = 2, hardness: Float = 5.0F, resistance: Float = 10.0F): Supplier[Block] = () =>
        new Block(
            Block.Properties.create(Material.ROCK)
                .hardnessAndResistance(hardness, resistance)
                .harvestLevel(harvestLevel)
                .harvestTool(ToolType.PICKAXE)
        )

    private def makeDecorativeWallBlock(block: Supplier[Block]): Supplier[WallBlock] = () =>
        new WallBlock(Block.Properties.from(block))

    private def makeItemBlock[T <: Block](block: Supplier[T]): Supplier[Item] = () =>
        new BlockItem(block.get(), new Item.Properties().group(explorationItemGroup))


    private def makeItem(make: Item.Properties => Item): Supplier[Item] = () =>
        make(new Item.Properties().group(explorationItemGroup))


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
            gen.addProvider(new BlockTags(gen))
            gen.addProvider(new ItemTags(gen))
            gen.addProvider(new BlockLootTables(gen))
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

private class BlockTags(gen: DataGenerator) extends BlockTagsProvider(gen) {
    override def getName = "ProjectRed-Exploration Block Tags."

    override protected def registerTags() {
        getBuilder(MCBlockTags.WALLS).add(blockMarbleWall)
        getBuilder(MCBlockTags.WALLS).add(blockMarbleBrickWall)
        getBuilder(MCBlockTags.WALLS).add(blockBasaltWall)
        getBuilder(MCBlockTags.WALLS).add(blockBasaltCobbleWall)
        getBuilder(MCBlockTags.WALLS).add(blockBasaltBrickWall)
        getBuilder(MCBlockTags.WALLS).add(blockRubyBlockWall)
        getBuilder(MCBlockTags.WALLS).add(blockSapphireBlockWall)
        getBuilder(MCBlockTags.WALLS).add(blockPeridotBlockWall)
        getBuilder(MCBlockTags.WALLS).add(blockCopperBlockWall)
        getBuilder(MCBlockTags.WALLS).add(blockTinBlockWall)
        getBuilder(MCBlockTags.WALLS).add(blockSilverBlockWall)
        getBuilder(MCBlockTags.WALLS).add(blockElectrotineBlockWall)

        getBuilder(ForgeBlockTags.ORES)
            .add(tagBlockRubyOre)
            .add(tagBlockSapphireOre)
            .add(tagBlockPeridotOre)
            .add(tagBlockCopperOre)
            .add(tagBlockTinOre)
            .add(tagBlockSilverOre)
            .add(tagBlockElectrotineOre)
        getBuilder(tagBlockRubyOre).add(blockRubyOre)
        getBuilder(tagBlockSapphireOre).add(blockSapphireOre)
        getBuilder(tagBlockPeridotOre).add(blockPeridotOre)
        getBuilder(tagBlockCopperOre).add(blockCopperOre)
        getBuilder(tagBlockTinOre).add(blockTinOre)
        getBuilder(tagBlockSilverOre).add(blockSilverOre)
        getBuilder(tagBlockElectrotineOre).add(blockElectrotineOre)

        getBuilder(ForgeBlockTags.STORAGE_BLOCKS)
            .add(tagBlockMarble)
            .add(tagBlockBasalt)
            .add(tagBlockRuby)
            .add(tagBlockSapphire)
            .add(tagBlockPeridot)
            .add(tagBlockCopper)
            .add(tagBlockTin)
            .add(tagBlockSilver)
            .add(tagBlockElectrotine)
        getBuilder(tagBlockMarble).add(blockMarble)
        getBuilder(tagBlockBasalt).add(blockBasalt)
        getBuilder(tagBlockRuby).add(blockRubyBlock)
        getBuilder(tagBlockSapphire).add(blockSapphireBlock)
        getBuilder(tagBlockPeridot).add(blockPeridotBlock)
        getBuilder(tagBlockCopper).add(blockCopperBlock)
        getBuilder(tagBlockTin).add(blockTinBlock)
        getBuilder(tagBlockSilver).add(blockSilverBlock)
        getBuilder(tagBlockElectrotine).add(blockElectrotineBlock)

    }
}

private class ItemTags(gen: DataGenerator) extends ItemTagsProvider(gen) {
    override def getName = "ProjectRed-Exploration Item Tags."

    override protected def registerTags() {
        getBuilder(ForgeItemTags.ORES)
            .add(tagItemRubyOre)
            .add(tagItemSapphireOre)
            .add(tagItemPeridotOre)
            .add(tagItemCopperOre)
            .add(tagItemTinOre)
            .add(tagItemSilverOre)
            .add(tagItemElectrotineOre)

        copy(tagBlockRubyOre, tagItemRubyOre)
        copy(tagBlockSapphireOre, tagItemSapphireOre)
        copy(tagBlockPeridotOre, tagItemPeridotOre)
        copy(tagBlockCopperOre, tagItemCopperOre)
        copy(tagBlockTinOre, tagItemTinOre)
        copy(tagBlockSilverOre, tagItemSilverOre)
        copy(tagBlockElectrotineOre, tagItemElectrotineOre)

        getBuilder(ForgeItemTags.STORAGE_BLOCKS)
            .add(tagItemMarble)
            .add(tagItemBasalt)
            .add(tagItemRuby)
            .add(tagItemSapphire)
            .add(tagItemPeridot)
            .add(tagItemCopper)
            .add(tagItemTin)
            .add(tagItemSilver)
            .add(tagItemElectrotine)
        copy(tagBlockMarble, tagItemMarble)
        copy(tagBlockBasalt, tagItemBasalt)
        copy(tagBlockRuby, tagItemRuby)
        copy(tagBlockSapphire, tagItemSapphire)
        copy(tagBlockPeridot, tagItemPeridot)
        copy(tagBlockCopper, tagItemCopper)
        copy(tagBlockTin, tagItemTin)
        copy(tagBlockSilver, tagItemSilver)
        copy(tagBlockElectrotine, tagItemElectrotine)

        getBuilder(tagBackpacks)
            .add(tagBackpacksWhite)
            .add(tagBackpacksOrange)
            .add(tagBackpacksMagenta)
            .add(tagBackpacksLightBlue)
            .add(tagBackpacksYellow)
            .add(tagBackpacksLime)
            .add(tagBackpacksPink)
            .add(tagBackpacksGray)
            .add(tagBackpacksLightGray)
            .add(tagBackpacksCyan)
            .add(tagBackpacksPurple)
            .add(tagBackpacksBlue)
            .add(tagBackpacksBrown)
            .add(tagBackpacksGreen)
            .add(tagBackpacksRed)
            .add(tagBackpacksBlack)
        getBuilder(tagBackpacksWhite).add(itemWhiteBackpack)
        getBuilder(tagBackpacksOrange).add(itemOrangeBackpack)
        getBuilder(tagBackpacksMagenta).add(itemMagentaBackpack)
        getBuilder(tagBackpacksLightBlue).add(itemLightBlueBackpack)
        getBuilder(tagBackpacksYellow).add(itemYellowBackpack)
        getBuilder(tagBackpacksLime).add(itemLimeBackpack)
        getBuilder(tagBackpacksPink).add(itemPinkBackpack)
        getBuilder(tagBackpacksGray).add(itemGrayBackpack)
        getBuilder(tagBackpacksLightGray).add(itemLightGrayBackpack)
        getBuilder(tagBackpacksCyan).add(itemCyanBackpack)
        getBuilder(tagBackpacksPurple).add(itemPurpleBackpack)
        getBuilder(tagBackpacksBlue).add(itemBlueBackpack)
        getBuilder(tagBackpacksBrown).add(itemBrownBackpack)
        getBuilder(tagBackpacksGreen).add(itemGreenBackpack)
        getBuilder(tagBackpacksRed).add(itemRedBackpack)
        getBuilder(tagBackpacksBlack).add(itemBlackBackpack)

        getBuilder(tagBackpackDisallowed).add(tagBackpacks)
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
    }
}

private class Recipes(gen: DataGenerator) extends RecipeProvider(gen) {
    override def getName = "ProjectRed-Exploration Recipes."

    override protected def registerRecipes() {
        smelting(itemBasalt, 1)
            .autoCriteria()
            .ingredient(Ingredient.fromItems(itemBasaltCobble))
            .experience(0.1F)

        smelting(itemRuby, 1)
            .autoCriteria()
            .ingredient(Ingredient.fromTag(tagItemRubyOre))
            .experience(1F)

        smelting(itemSapphire, 1)
            .autoCriteria()
            .ingredient(Ingredient.fromTag(tagItemSapphireOre))
            .experience(1F)

        smelting(itemPeridot, 1)
            .autoCriteria()
            .ingredient(Ingredient.fromTag(tagItemPeridotOre))
            .experience(1F)

        smelting(itemCopperIngot, 1)
            .autoCriteria()
            .ingredient(Ingredient.fromTag(tagItemCopperOre))
            .experience(0.7F)

        smelting(itemTinIngot, 1)
            .autoCriteria()
            .ingredient(Ingredient.fromTag(tagItemTinOre))
            .experience(0.7F)

        smelting(itemSilverIngot, 1)
            .autoCriteria()
            .ingredient(Ingredient.fromTag(tagItemSilverOre))
            .experience(0.8F)

        smelting(itemElectrotineDust, 1)
            .autoCriteria()
            .ingredient(Ingredient.fromTag(tagItemElectrotineOre))
            .experience(0.7F)


    }

}
