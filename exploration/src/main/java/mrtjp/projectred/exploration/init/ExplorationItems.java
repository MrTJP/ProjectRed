package mrtjp.projectred.exploration.init;

import codechicken.lib.item.SimpleArmorMaterial;
import codechicken.microblock.item.SawItem;
import mrtjp.projectred.exploration.item.AthameItem;
import mrtjp.projectred.exploration.item.BackpackItem;
import mrtjp.projectred.exploration.item.SickleItem;
import mrtjp.projectred.exploration.item.WoolGinItem;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.sounds.SoundEvents;
import net.minecraft.tags.BlockTags;
import net.minecraft.world.item.*;
import net.minecraft.world.item.crafting.Ingredient;
import net.neoforged.neoforge.common.SimpleTier;
import net.neoforged.neoforge.common.TierSortingRegistry;

import java.util.List;
import java.util.function.Supplier;

import static mrtjp.projectred.core.init.CoreTags.*;
import static mrtjp.projectred.exploration.ProjectRedExploration.ITEMS;
import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ExplorationItems {

    public static final String ID_RAW_TIN = "raw_tin";
    public static final String ID_TIN_INGOT = "tin_ingot";
    public static final String ID_RAW_SILVER = "raw_silver";
    public static final String ID_SILVER_INGOT = "silver_ingot";

    public static final String ID_WOOL_GIN = "wool_gin";
    public static final String ID_ATHAME = "athame";

    public static final String ID_RUBY_AXE = "ruby_axe";
    public static final String ID_SAPPHIRE_AXE = "sapphire_axe";
    public static final String ID_PERIDOT_AXE = "peridot_axe";

    public static final String ID_RUBY_PICKAXE = "ruby_pickaxe";
    public static final String ID_SAPPHIRE_PICKAXE = "sapphire_pickaxe";
    public static final String ID_PERIDOT_PICKAXE = "peridot_pickaxe";

    public static final String ID_RUBY_SHOVEL = "ruby_shovel";
    public static final String ID_SAPPHIRE_SHOVEL = "sapphire_shovel";
    public static final String ID_PERIDOT_SHOVEL = "peridot_shovel";

    public static final String ID_RUBY_HOE = "ruby_hoe";
    public static final String ID_SAPPHIRE_HOE = "sapphire_hoe";
    public static final String ID_PERIDOT_HOE = "peridot_hoe";

    public static final String ID_RUBY_SWORD = "ruby_sword";
    public static final String ID_SAPPHIRE_SWORD = "sapphire_sword";
    public static final String ID_PERIDOT_SWORD = "peridot_sword";

    public static final String ID_RUBY_HELMET = "ruby_helmet";
    public static final String ID_SAPPHIRE_HELMET = "sapphire_helmet";
    public static final String ID_PERIDOT_HELMET = "peridot_helmet";

    public static final String ID_RUBY_CHESTPLATE = "ruby_chestplate";
    public static final String ID_SAPPHIRE_CHESTPLATE = "sapphire_chestplate";
    public static final String ID_PERIDOT_CHESTPLATE = "peridot_chestplate";

    public static final String ID_RUBY_LEGGINGS = "ruby_leggings";
    public static final String ID_SAPPHIRE_LEGGINGS = "sapphire_leggings";
    public static final String ID_PERIDOT_LEGGINGS = "peridot_leggings";

    public static final String ID_RUBY_BOOTS = "ruby_boots";
    public static final String ID_SAPPHIRE_BOOTS = "sapphire_boots";
    public static final String ID_PERIDOT_BOOTS = "peridot_boots";

    public static final String ID_GOLD_SAW = "gold_saw";
    public static final String ID_RUBY_SAW = "ruby_saw";
    public static final String ID_SAPPHIRE_SAW = "sapphire_saw";
    public static final String ID_PERIDOT_SAW = "peridot_saw";

    public static final String ID_WOOD_SICKLE = "wood_sickle";
    public static final String ID_STONE_SICKLE = "stone_sickle";
    public static final String ID_IRON_SICKLE = "iron_sickle";
    public static final String ID_GOLD_SICKLE = "gold_sickle";
    public static final String ID_DIAMOND_SICKLE = "diamond_sickle";
    public static final String ID_RUBY_SICKLE = "ruby_sickle";
    public static final String ID_SAPPHIRE_SICKLE = "sapphire_sickle";
    public static final String ID_PERIDOT_SICKLE = "peridot_sickle";

    public static final String ID_WHITE_BACKPACK = "white_backpack";
    public static final String ID_ORANGE_BACKPACK = "orange_backpack";
    public static final String ID_MAGENTA_BACKPACK = "magenta_backpack";
    public static final String ID_LIGHT_BLUE_BACKPACK = "light_blue_backpack";
    public static final String ID_YELLOW_BACKPACK = "yellow_backpack";
    public static final String ID_LIME_BACKPACK = "lime_backpack";
    public static final String ID_PINK_BACKPACK = "pink_backpack";
    public static final String ID_GRAY_BACKPACK = "gray_backpack";
    public static final String ID_LIGHT_GRAY_BACKPACK = "light_gray_backpack";
    public static final String ID_CYAN_BACKPACK = "cyan_backpack";
    public static final String ID_PURPLE_BACKPACK = "purple_backpack";
    public static final String ID_BLUE_BACKPACK = "blue_backpack";
    public static final String ID_BROWN_BACKPACK = "brown_backpack";
    public static final String ID_GREEN_BACKPACK = "green_backpack";
    public static final String ID_RED_BACKPACK = "red_backpack";
    public static final String ID_BLACK_BACKPACK = "black_backpack";

    // Ingots / dusts / gems
    public static Supplier<Item> RAW_TIN_ITEM;
    public static Supplier<Item> TIN_INGOT_ITEM;
    public static Supplier<Item> RAW_SILVER_ITEM;
    public static Supplier<Item> SILVER_INGOT_ITEM;

    public static Supplier<Item> WOOL_GIN;
    public static Supplier<Item> ATHAME;

    // Tools
    public static Supplier<Item> RUBY_AXE;
    public static Supplier<Item> SAPPHIRE_AXE;
    public static Supplier<Item> PERIDOT_AXE;

    public static Supplier<Item> RUBY_PICKAXE;
    public static Supplier<Item> SAPPHIRE_PICKAXE;
    public static Supplier<Item> PERIDOT_PICKAXE;

    public static Supplier<Item> RUBY_SHOVEL;
    public static Supplier<Item> SAPPHIRE_SHOVEL;
    public static Supplier<Item> PERIDOT_SHOVEL;

    public static Supplier<Item> RUBY_HOE;
    public static Supplier<Item> SAPPHIRE_HOE;
    public static Supplier<Item> PERIDOT_HOE;

    public static Supplier<Item> RUBY_SWORD;
    public static Supplier<Item> SAPPHIRE_SWORD;
    public static Supplier<Item> PERIDOT_SWORD;

    public static Supplier<Item> GOLD_SAW;
    public static Supplier<Item> RUBY_SAW;
    public static Supplier<Item> SAPPHIRE_SAW;
    public static Supplier<Item> PERIDOT_SAW;

    public static Supplier<Item> WOOD_SICKLE;
    public static Supplier<Item> STONE_SICKLE;
    public static Supplier<Item> IRON_SICKLE;
    public static Supplier<Item> GOLD_SICKLE;
    public static Supplier<Item> DIAMOND_SICKLE;
    public static Supplier<Item> RUBY_SICKLE;
    public static Supplier<Item> SAPPHIRE_SICKLE;
    public static Supplier<Item> PERIDOT_SICKLE;

    // Armor
    public static Supplier<Item> RUBY_HELMET;
    public static Supplier<Item> SAPPHIRE_HELMET;
    public static Supplier<Item> PERIDOT_HELMET;

    public static Supplier<Item> RUBY_CHESTPLATE;
    public static Supplier<Item> SAPPHIRE_CHESTPLATE;
    public static Supplier<Item> PERIDOT_CHESTPLATE;

    public static Supplier<Item> RUBY_LEGGINGS;
    public static Supplier<Item> SAPPHIRE_LEGGINGS;
    public static Supplier<Item> PERIDOT_LEGGINGS;

    public static Supplier<Item> RUBY_BOOTS;
    public static Supplier<Item> SAPPHIRE_BOOTS;
    public static Supplier<Item> PERIDOT_BOOTS;

    //Backpacks
    public static Supplier<Item> WHITE_BACKPACK;
    public static Supplier<Item> ORANGE_BACKPACK;
    public static Supplier<Item> MAGENTA_BACKPACK;
    public static Supplier<Item> LIGHT_BLUE_BACKPACK;
    public static Supplier<Item> YELLOW_BACKPACK;
    public static Supplier<Item> LIME_BACKPACK;
    public static Supplier<Item> PINK_BACKPACK;
    public static Supplier<Item> GRAY_BACKPACK;
    public static Supplier<Item> LIGHT_GRAY_BACKPACK;
    public static Supplier<Item> CYAN_BACKPACK;
    public static Supplier<Item> PURPLE_BACKPACK;
    public static Supplier<Item> BLUE_BACKPACK;
    public static Supplier<Item> BROWN_BACKPACK;
    public static Supplier<Item> GREEN_BACKPACK;
    public static Supplier<Item> RED_BACKPACK;
    public static Supplier<Item> BLACK_BACKPACK;

    public static final SimpleTier ATHAME_ITEM_TIER = new SimpleTier(
            Tiers.DIAMOND.getLevel(),
            100,
            Tiers.DIAMOND.getSpeed(),
            Tiers.DIAMOND.getAttackDamageBonus(),
            30,
            BlockTags.NEEDS_IRON_TOOL,
            () -> Ingredient.of(SILVER_INGOT_TAG)
    );

    public static final SimpleTier RUBY_ITEM_TIER = new SimpleTier(
            2,
            512,
            8.00F,
            3.00F,
            10,
            BlockTags.NEEDS_IRON_TOOL,
            () -> Ingredient.of(RUBY_GEM_TAG)
    );

    public static final SimpleTier SAPPHIRE_ITEM_TIER = new SimpleTier(
            2,
            512,
            8.00F,
            3.00F,
            10,
            BlockTags.NEEDS_IRON_TOOL,
            () -> Ingredient.of(SAPPHIRE_GEM_TAG)
    );

    public static final SimpleTier PERIDOT_ITEM_TIER = new SimpleTier(
            2,
            512,
            7.75F,
            2.75F,
            14,
            BlockTags.NEEDS_IRON_TOOL,
            () -> Ingredient.of(PERIDOT_GEM_TAG)
    );

    public static final SimpleArmorMaterial RUBY_ARMOR_MATERIAL = SimpleArmorMaterial.builder()
            .durabilityFactor(16)
            .damageReduction(new int[] { 3, 6, 8, 3 })
            .enchantability(10)
            .soundEvent(SoundEvents.ARMOR_EQUIP_DIAMOND)
            .repairMaterial(() -> Ingredient.of(RUBY_GEM_TAG))
            .textureName(MOD_ID + ":ruby")
            .toughness(1.25F)
            .build();

    public static final SimpleArmorMaterial SAPPHIRE_ARMOR_MATERIAL = SimpleArmorMaterial.builder()
            .durabilityFactor(16)
            .damageReduction(new int[] { 3, 6, 8, 3 })
            .enchantability(10)
            .soundEvent(SoundEvents.ARMOR_EQUIP_DIAMOND)
            .repairMaterial(() -> Ingredient.of(SAPPHIRE_GEM_TAG))
            .textureName(MOD_ID + ":sapphire")
            .toughness(1.25F)
            .build();

    public static final SimpleArmorMaterial PERIDOT_ARMOR_MATERIAL = SimpleArmorMaterial.builder()
            .durabilityFactor(14)
            .damageReduction(new int[] { 3, 6, 8, 3 })
            .enchantability(14)
            .soundEvent(SoundEvents.ARMOR_EQUIP_DIAMOND)
            .repairMaterial(() -> Ingredient.of(PERIDOT_GEM_TAG))
            .textureName(MOD_ID + ":peridot")
            .toughness(1.25F)
            .build();

    public static void register() {

        // Ingots/dusts/gems
        RAW_TIN_ITEM = ITEMS.register(ID_RAW_TIN, createSimpleItemSupplier());
        TIN_INGOT_ITEM = ITEMS.register(ID_TIN_INGOT, createSimpleItemSupplier());
        RAW_SILVER_ITEM = ITEMS.register(ID_RAW_SILVER, createSimpleItemSupplier());
        SILVER_INGOT_ITEM = ITEMS.register(ID_SILVER_INGOT, createSimpleItemSupplier());

        WOOL_GIN = ITEMS.register(ID_WOOL_GIN, WoolGinItem::new);
        ATHAME = ITEMS.register(ID_ATHAME, () -> new AthameItem(ATHAME_ITEM_TIER, 3, -2.4F));

        // Tiers
        registerSubDiamondTeir(ATHAME_ITEM_TIER, "athame");
        registerSubDiamondTeir(RUBY_ITEM_TIER, "ruby");
        registerSubDiamondTeir(SAPPHIRE_ITEM_TIER, "sapphire");
        registerSubDiamondTeir(PERIDOT_ITEM_TIER, "peridot");

        /* Tools */

        RUBY_AXE         = ITEMS.register(ID_RUBY_AXE,         () -> createAxeItem(RUBY_ITEM_TIER, 5.0F, -3.0F));
        SAPPHIRE_AXE     = ITEMS.register(ID_SAPPHIRE_AXE,     () -> createAxeItem(SAPPHIRE_ITEM_TIER, 5.0F, -3.0F));
        PERIDOT_AXE      = ITEMS.register(ID_PERIDOT_AXE,      () -> createAxeItem(PERIDOT_ITEM_TIER, 5.0F, -3.0F));

        RUBY_PICKAXE     = ITEMS.register(ID_RUBY_PICKAXE,     () -> createPickaxeItem(RUBY_ITEM_TIER, 1, -2.8F));
        SAPPHIRE_PICKAXE = ITEMS.register(ID_SAPPHIRE_PICKAXE, () -> createPickaxeItem(SAPPHIRE_ITEM_TIER, 1, -2.8F));
        PERIDOT_PICKAXE  = ITEMS.register(ID_PERIDOT_PICKAXE,  () -> createPickaxeItem(PERIDOT_ITEM_TIER, 1, -2.8F));

        RUBY_SHOVEL      = ITEMS.register(ID_RUBY_SHOVEL,      () -> createShovelItem(RUBY_ITEM_TIER, 1.5F, -3.0F));
        SAPPHIRE_SHOVEL  = ITEMS.register(ID_SAPPHIRE_SHOVEL,  () -> createShovelItem(SAPPHIRE_ITEM_TIER, 1.5F, -3.0F));
        PERIDOT_SHOVEL   = ITEMS.register(ID_PERIDOT_SHOVEL,   () -> createShovelItem(PERIDOT_ITEM_TIER, 1.5F, -3.0F));

        RUBY_HOE         = ITEMS.register(ID_RUBY_HOE,         () -> createHoeItem(RUBY_ITEM_TIER, -3, 0.0F));
        SAPPHIRE_HOE     = ITEMS.register(ID_SAPPHIRE_HOE,     () -> createHoeItem(SAPPHIRE_ITEM_TIER, -3, 0.0F));
        PERIDOT_HOE      = ITEMS.register(ID_PERIDOT_HOE,      () -> createHoeItem(PERIDOT_ITEM_TIER, -3, 0.0F));

        RUBY_SWORD       = ITEMS.register(ID_RUBY_SWORD,       () -> createSwordItem(RUBY_ITEM_TIER, 3, -2.4F));
        SAPPHIRE_SWORD   = ITEMS.register(ID_SAPPHIRE_SWORD,   () -> createSwordItem(SAPPHIRE_ITEM_TIER, 3, -2.4F));
        PERIDOT_SWORD    = ITEMS.register(ID_PERIDOT_SWORD,    () -> createSwordItem(PERIDOT_ITEM_TIER, 3, -2.4F));

        GOLD_SAW         = ITEMS.register(ID_GOLD_SAW,         () -> createSawItem(Tiers.GOLD));
        RUBY_SAW         = ITEMS.register(ID_RUBY_SAW,         () -> createSawItem(RUBY_ITEM_TIER));
        SAPPHIRE_SAW     = ITEMS.register(ID_SAPPHIRE_SAW,     () -> createSawItem(SAPPHIRE_ITEM_TIER));
        PERIDOT_SAW      = ITEMS.register(ID_PERIDOT_SAW,      () -> createSawItem(PERIDOT_ITEM_TIER));

        WOOD_SICKLE      = ITEMS.register(ID_WOOD_SICKLE,      () -> createSickleItem(Tiers.WOOD, 1, -2.8F));
        STONE_SICKLE     = ITEMS.register(ID_STONE_SICKLE,     () -> createSickleItem(Tiers.STONE, 1, -2.8F));
        IRON_SICKLE      = ITEMS.register(ID_IRON_SICKLE,      () -> createSickleItem(Tiers.IRON, 1, -2.8F));
        GOLD_SICKLE      = ITEMS.register(ID_GOLD_SICKLE,      () -> createSickleItem(Tiers.GOLD, 1, -2.8F));
        DIAMOND_SICKLE   = ITEMS.register(ID_DIAMOND_SICKLE,   () -> createSickleItem(Tiers.DIAMOND, 1, -2.8F));
        RUBY_SICKLE      = ITEMS.register(ID_RUBY_SICKLE,      () -> createSickleItem(RUBY_ITEM_TIER, 1, -2.8F));
        SAPPHIRE_SICKLE  = ITEMS.register(ID_SAPPHIRE_SICKLE,  () -> createSickleItem(SAPPHIRE_ITEM_TIER, 1, -2.8F));
        PERIDOT_SICKLE   = ITEMS.register(ID_PERIDOT_SICKLE,   () -> createSickleItem(PERIDOT_ITEM_TIER, 1, -2.8F));

        /* Armor */

        RUBY_HELMET         = ITEMS.register(ID_RUBY_HELMET,           () -> createArmorItem(RUBY_ARMOR_MATERIAL, ArmorItem.Type.HELMET));
        SAPPHIRE_HELMET     = ITEMS.register(ID_SAPPHIRE_HELMET,       () -> createArmorItem(SAPPHIRE_ARMOR_MATERIAL, ArmorItem.Type.HELMET));
        PERIDOT_HELMET      = ITEMS.register(ID_PERIDOT_HELMET,        () -> createArmorItem(PERIDOT_ARMOR_MATERIAL, ArmorItem.Type.HELMET));

        RUBY_CHESTPLATE     = ITEMS.register(ID_RUBY_CHESTPLATE,       () -> createArmorItem(RUBY_ARMOR_MATERIAL, ArmorItem.Type.CHESTPLATE));
        SAPPHIRE_CHESTPLATE = ITEMS.register(ID_SAPPHIRE_CHESTPLATE,   () -> createArmorItem(SAPPHIRE_ARMOR_MATERIAL, ArmorItem.Type.CHESTPLATE));
        PERIDOT_CHESTPLATE  = ITEMS.register(ID_PERIDOT_CHESTPLATE,    () -> createArmorItem(PERIDOT_ARMOR_MATERIAL, ArmorItem.Type.CHESTPLATE));

        RUBY_LEGGINGS       = ITEMS.register(ID_RUBY_LEGGINGS,         () -> createArmorItem(RUBY_ARMOR_MATERIAL, ArmorItem.Type.LEGGINGS));
        SAPPHIRE_LEGGINGS   = ITEMS.register(ID_SAPPHIRE_LEGGINGS,     () -> createArmorItem(SAPPHIRE_ARMOR_MATERIAL, ArmorItem.Type.LEGGINGS));
        PERIDOT_LEGGINGS    = ITEMS.register(ID_PERIDOT_LEGGINGS,      () -> createArmorItem(PERIDOT_ARMOR_MATERIAL, ArmorItem.Type.LEGGINGS));

        RUBY_BOOTS          = ITEMS.register(ID_RUBY_BOOTS,            () -> createArmorItem(RUBY_ARMOR_MATERIAL, ArmorItem.Type.BOOTS));
        SAPPHIRE_BOOTS      = ITEMS.register(ID_SAPPHIRE_BOOTS,        () -> createArmorItem(SAPPHIRE_ARMOR_MATERIAL, ArmorItem.Type.BOOTS));
        PERIDOT_BOOTS       = ITEMS.register(ID_PERIDOT_BOOTS,         () -> createArmorItem(PERIDOT_ARMOR_MATERIAL, ArmorItem.Type.BOOTS));

        /* Backpacks */

        WHITE_BACKPACK        = ITEMS.register(ID_WHITE_BACKPACK,       () -> new BackpackItem(0));
        ORANGE_BACKPACK       = ITEMS.register(ID_ORANGE_BACKPACK,      () -> new BackpackItem(1));
        MAGENTA_BACKPACK      = ITEMS.register(ID_MAGENTA_BACKPACK,     () -> new BackpackItem(2));
        LIGHT_BLUE_BACKPACK   = ITEMS.register(ID_LIGHT_BLUE_BACKPACK,  () -> new BackpackItem(3));
        YELLOW_BACKPACK       = ITEMS.register(ID_YELLOW_BACKPACK,      () -> new BackpackItem(4));
        LIME_BACKPACK         = ITEMS.register(ID_LIME_BACKPACK,        () -> new BackpackItem(5));
        PINK_BACKPACK         = ITEMS.register(ID_PINK_BACKPACK,        () -> new BackpackItem(6));
        GRAY_BACKPACK         = ITEMS.register(ID_GRAY_BACKPACK,        () -> new BackpackItem(7));
        LIGHT_GRAY_BACKPACK   = ITEMS.register(ID_LIGHT_GRAY_BACKPACK,  () -> new BackpackItem(8));
        CYAN_BACKPACK         = ITEMS.register(ID_CYAN_BACKPACK,        () -> new BackpackItem(9));
        PURPLE_BACKPACK       = ITEMS.register(ID_PURPLE_BACKPACK,      () -> new BackpackItem(10));
        BLUE_BACKPACK         = ITEMS.register(ID_BLUE_BACKPACK,        () -> new BackpackItem(11));
        BROWN_BACKPACK        = ITEMS.register(ID_BROWN_BACKPACK,       () -> new BackpackItem(12));
        GREEN_BACKPACK        = ITEMS.register(ID_GREEN_BACKPACK,       () -> new BackpackItem(13));
        RED_BACKPACK          = ITEMS.register(ID_RED_BACKPACK,         () -> new BackpackItem(14));
        BLACK_BACKPACK        = ITEMS.register(ID_BLACK_BACKPACK,       () -> new BackpackItem(15));
    }

    private static Supplier<Item> createSimpleItemSupplier() {
        return () -> new Item(new Item.Properties());
    }

    private static Item createAxeItem(Tier tier, float attackDamage, float attackSpeed) {
        return new AxeItem(tier, attackDamage, attackSpeed, new Item.Properties());
    }

    private static Item createPickaxeItem(Tier tier, int attackDamage, float attackSpeed) {
        return new PickaxeItem(tier, attackDamage, attackSpeed, new Item.Properties());
    }

    private static Item createShovelItem(Tier tier, float attackDamage, float attackSpeed) {
        return new ShovelItem(tier, attackDamage, attackSpeed, new Item.Properties());
    }

    private static Item createHoeItem(Tier tier, int attackDamage, float attackSpeed) {
        return new HoeItem(tier, attackDamage, attackSpeed, new Item.Properties());
    }

    private static Item createSwordItem(Tier tier, int attackDamage, float attackSpeed) {
        return new SwordItem(tier, attackDamage, attackSpeed, new Item.Properties());
    }

    private static Item createSawItem(Tier tier) {
        return new SawItem(tier, new Item.Properties());
    }

    private static Item createSickleItem(Tier tier, int attackDamage, float attackSpeed) {
        return new SickleItem(tier, attackDamage, attackSpeed, new Item.Properties());
    }

    private static Item createArmorItem(SimpleArmorMaterial material, ArmorItem.Type slot) {
        return new ArmorItem(material, slot, new Item.Properties());
    }

    private static void registerSubDiamondTeir(Tier tier, String id) {
        TierSortingRegistry.registerTier(
                tier,
                new ResourceLocation(MOD_ID, id),
                List.of(Tiers.WOOD, Tiers.STONE, Tiers.IRON, Tiers.GOLD),
                List.of(Tiers.DIAMOND, Tiers.NETHERITE));
    }

    //region Utilities
    public static Item getBackpackByColor(int color) {
        return switch (color) {
            case  0 -> WHITE_BACKPACK.get();
            case  1 -> ORANGE_BACKPACK.get();
            case  2 -> MAGENTA_BACKPACK.get();
            case  3 -> LIGHT_BLUE_BACKPACK.get();
            case  4 -> YELLOW_BACKPACK.get();
            case  5 -> LIME_BACKPACK.get();
            case  6 -> PINK_BACKPACK.get();
            case  7 -> GRAY_BACKPACK.get();
            case  8 -> LIGHT_GRAY_BACKPACK.get();
            case  9 -> CYAN_BACKPACK.get();
            case 10 -> PURPLE_BACKPACK.get();
            case 11 -> BLUE_BACKPACK.get();
            case 12 -> BROWN_BACKPACK.get();
            case 13 -> GREEN_BACKPACK.get();
            case 14 -> RED_BACKPACK.get();
            case 15 -> BLACK_BACKPACK.get();
            default -> throw new IllegalArgumentException("Invalid color: " + color);
        };
    }
    //endregion
}
