package mrtjp.projectred.exploration.init;

import codechicken.lib.item.SimpleArmorMaterial;
import codechicken.lib.item.SimpleItemTier;
import mrtjp.projectred.exploration.item.*;
import net.minecraft.inventory.EquipmentSlotType;
import net.minecraft.item.*;
import net.minecraft.item.crafting.Ingredient;
import net.minecraft.util.SoundEvents;

import static mrtjp.projectred.ProjectRedExploration.*;
import static mrtjp.projectred.core.init.CoreTags.*;

public class ExplorationItems {

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

    public static final SimpleItemTier ATHAME_ITEM_TIER = SimpleItemTier.builder(ItemTier.DIAMOND)
            .maxUses(100)
            .enchantability(30)
            .repairMaterial(() ->  Ingredient.of(SILVER_INGOT_TAG))
            .build();

    public static final SimpleItemTier RUBY_ITEM_TIER = SimpleItemTier.builder()
            .maxUses(512)
            .efficiency(8.00F)
            .attackDamage(3.00F)
            .harvestLevel(2)
            .enchantability(10)
            .repairMaterial(() -> Ingredient.of(RUBY_GEM_TAG))
            .build();

    public static final SimpleItemTier SAPPHIRE_ITEM_TIER = SimpleItemTier.builder()
            .maxUses(512)
            .efficiency(8.00F)
            .attackDamage(3.00F)
            .harvestLevel(2)
            .enchantability(10)
            .repairMaterial(() -> Ingredient.of(SAPPHIRE_GEM_TAG))
            .build();

    public static final SimpleItemTier PERIDOT_ITEM_TIER = SimpleItemTier.builder()
            .maxUses(512)
            .efficiency(7.75F)
            .attackDamage(2.75F)
            .harvestLevel(2)
            .enchantability(14)
            .repairMaterial(() -> Ingredient.of(PERIDOT_GEM_TAG))
            .build();

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

        ITEMS.register(ID_WOOL_GIN, WoolGinItem::new);
        ITEMS.register(ID_ATHAME, () -> new AthameItem(ATHAME_ITEM_TIER, 3, -2.4F));

        /* Tools */

        ITEMS.register(ID_RUBY_AXE,     () -> createAxeItem(RUBY_ITEM_TIER, 5.0F, -3.0F));
        ITEMS.register(ID_SAPPHIRE_AXE, () -> createAxeItem(SAPPHIRE_ITEM_TIER, 5.0F, -3.0F));
        ITEMS.register(ID_PERIDOT_AXE,  () -> createAxeItem(PERIDOT_ITEM_TIER, 5.0F, -3.0F));

        ITEMS.register(ID_RUBY_PICKAXE,     () -> createPickaxeItem(RUBY_ITEM_TIER, 1, -2.8F));
        ITEMS.register(ID_SAPPHIRE_PICKAXE, () -> createPickaxeItem(SAPPHIRE_ITEM_TIER, 1, -2.8F));
        ITEMS.register(ID_PERIDOT_PICKAXE,  () -> createPickaxeItem(PERIDOT_ITEM_TIER, 1, -2.8F));

        ITEMS.register(ID_RUBY_SHOVEL,     () -> createShovelItem(RUBY_ITEM_TIER, 1.5F, -3.0F));
        ITEMS.register(ID_SAPPHIRE_SHOVEL, () -> createShovelItem(SAPPHIRE_ITEM_TIER, 1.5F, -3.0F));
        ITEMS.register(ID_PERIDOT_SHOVEL,  () -> createShovelItem(PERIDOT_ITEM_TIER, 1.5F, -3.0F));

        ITEMS.register(ID_RUBY_HOE,     () -> createHoeItem(RUBY_ITEM_TIER, -3, 0.0F));
        ITEMS.register(ID_SAPPHIRE_HOE, () -> createHoeItem(SAPPHIRE_ITEM_TIER, -3, 0.0F));
        ITEMS.register(ID_PERIDOT_HOE,  () -> createHoeItem(PERIDOT_ITEM_TIER, -3, 0.0F));

        ITEMS.register(ID_RUBY_SWORD,     () -> createSwordItem(RUBY_ITEM_TIER, 3, -2.4F));
        ITEMS.register(ID_SAPPHIRE_SWORD, () -> createSwordItem(SAPPHIRE_ITEM_TIER, 3, -2.4F));
        ITEMS.register(ID_PERIDOT_SWORD,  () -> createSwordItem(PERIDOT_ITEM_TIER, 3, -2.4F));

        ITEMS.register(ID_GOLD_SAW,     () -> createSawItem(ItemTier.GOLD));
        ITEMS.register(ID_RUBY_SAW,     () -> createSawItem(RUBY_ITEM_TIER));
        ITEMS.register(ID_SAPPHIRE_SAW, () -> createSawItem(SAPPHIRE_ITEM_TIER));
        ITEMS.register(ID_PERIDOT_SAW,  () -> createSawItem(PERIDOT_ITEM_TIER));

        ITEMS.register(ID_WOOD_SICKLE,    () -> createSickleItem(ItemTier.WOOD, 1, -2.8F));
        ITEMS.register(ID_STONE_SICKLE,   () -> createSickleItem(ItemTier.STONE, 1, -2.8F));
        ITEMS.register(ID_IRON_SICKLE,    () -> createSickleItem(ItemTier.IRON, 1, -2.8F));
        ITEMS.register(ID_GOLD_SICKLE,    () -> createSickleItem(ItemTier.GOLD, 1, -2.8F));
        ITEMS.register(ID_DIAMOND_SICKLE, () -> createSickleItem(ItemTier.DIAMOND, 1, -2.8F));
        ITEMS.register(ID_RUBY_SICKLE,    () -> createSickleItem(RUBY_ITEM_TIER, 1, -2.8F));
        ITEMS.register(ID_SAPPHIRE_SICKLE,() -> createSickleItem(SAPPHIRE_ITEM_TIER, 1, -2.8F));
        ITEMS.register(ID_PERIDOT_SICKLE, () -> createSickleItem(PERIDOT_ITEM_TIER, 1, -2.8F));

        /* Armmor */

        ITEMS.register(ID_RUBY_HELMET,           () -> createArmorItem(RUBY_ARMOR_MATERIAL, EquipmentSlotType.HEAD));
        ITEMS.register(ID_SAPPHIRE_HELMET,       () -> createArmorItem(SAPPHIRE_ARMOR_MATERIAL, EquipmentSlotType.HEAD));
        ITEMS.register(ID_PERIDOT_HELMET,        () -> createArmorItem(PERIDOT_ARMOR_MATERIAL, EquipmentSlotType.HEAD));

        ITEMS.register(ID_RUBY_CHESTPLATE,       () -> createArmorItem(RUBY_ARMOR_MATERIAL, EquipmentSlotType.CHEST));
        ITEMS.register(ID_SAPPHIRE_CHESTPLATE,   () -> createArmorItem(SAPPHIRE_ARMOR_MATERIAL, EquipmentSlotType.CHEST));
        ITEMS.register(ID_PERIDOT_CHESTPLATE,    () -> createArmorItem(PERIDOT_ARMOR_MATERIAL, EquipmentSlotType.CHEST));

        ITEMS.register(ID_RUBY_LEGGINGS,         () -> createArmorItem(RUBY_ARMOR_MATERIAL, EquipmentSlotType.LEGS));
        ITEMS.register(ID_SAPPHIRE_LEGGINGS,     () -> createArmorItem(SAPPHIRE_ARMOR_MATERIAL, EquipmentSlotType.LEGS));
        ITEMS.register(ID_PERIDOT_LEGGINGS,      () -> createArmorItem(PERIDOT_ARMOR_MATERIAL, EquipmentSlotType.LEGS));

        ITEMS.register(ID_RUBY_BOOTS,            () -> createArmorItem(RUBY_ARMOR_MATERIAL, EquipmentSlotType.FEET));
        ITEMS.register(ID_SAPPHIRE_BOOTS,        () -> createArmorItem(SAPPHIRE_ARMOR_MATERIAL, EquipmentSlotType.FEET));
        ITEMS.register(ID_PERIDOT_BOOTS,         () -> createArmorItem(PERIDOT_ARMOR_MATERIAL, EquipmentSlotType.FEET));

        /* Backpacks */

        ITEMS.register(ID_WHITE_BACKPACK,       () -> new BackpackItem(0));
        ITEMS.register(ID_ORANGE_BACKPACK,      () -> new BackpackItem(1));
        ITEMS.register(ID_MAGENTA_BACKPACK,     () -> new BackpackItem(2));
        ITEMS.register(ID_LIGHT_BLUE_BACKPACK,  () -> new BackpackItem(3));
        ITEMS.register(ID_YELLOW_BACKPACK,      () -> new BackpackItem(4));
        ITEMS.register(ID_LIME_BACKPACK,        () -> new BackpackItem(5));
        ITEMS.register(ID_PINK_BACKPACK,        () -> new BackpackItem(6));
        ITEMS.register(ID_GRAY_BACKPACK,        () -> new BackpackItem(7));
        ITEMS.register(ID_LIGHT_GRAY_BACKPACK,  () -> new BackpackItem(8));
        ITEMS.register(ID_CYAN_BACKPACK,        () -> new BackpackItem(9));
        ITEMS.register(ID_PURPLE_BACKPACK,      () -> new BackpackItem(10));
        ITEMS.register(ID_BLUE_BACKPACK,        () -> new BackpackItem(11));
        ITEMS.register(ID_BROWN_BACKPACK,       () -> new BackpackItem(12));
        ITEMS.register(ID_GREEN_BACKPACK,       () -> new BackpackItem(13));
        ITEMS.register(ID_RED_BACKPACK,         () -> new BackpackItem(14));
        ITEMS.register(ID_BLACK_BACKPACK,       () -> new BackpackItem(15));
    }

    private static Item createAxeItem(SimpleItemTier tier, float attackDamage, float attackSpeed) {
        return new AxeItem(tier, attackDamage, attackSpeed, new Item.Properties().tab(EXPLORATION_GROUP));
    }

    private static Item createPickaxeItem(SimpleItemTier tier, int attackDamage, float attackSpeed) {
        return new PickaxeItem(tier, attackDamage, attackSpeed, new Item.Properties().tab(EXPLORATION_GROUP));
    }

    private static Item createShovelItem(SimpleItemTier tier, float attackDamage, float attackSpeed) {
        return new ShovelItem(tier, attackDamage, attackSpeed, new Item.Properties().tab(EXPLORATION_GROUP));
    }

    private static Item createHoeItem(SimpleItemTier tier, int attackDamage, float attackSpeed) {
        return new HoeItem(tier, attackDamage, attackSpeed, new Item.Properties().tab(EXPLORATION_GROUP));
    }

    private static Item createSwordItem(SimpleItemTier tier, int attackDamage, float attackSpeed) {
        return new SwordItem(tier, attackDamage, attackSpeed, new Item.Properties().tab(EXPLORATION_GROUP));
    }

    private static Item createSawItem(IItemTier tier) {
        return new SawItem(tier, new Item.Properties().tab(EXPLORATION_GROUP));
    }

    private static Item createSickleItem(IItemTier tier, int attackDamage, float attackSpeed) {
        return new SickleItem(tier, attackDamage, attackSpeed, new Item.Properties().tab(EXPLORATION_GROUP));
    }

    private static Item createArmorItem(SimpleArmorMaterial material, EquipmentSlotType slot) {
        return new ArmorItem(material, slot, new Item.Properties().tab(EXPLORATION_GROUP));
    }
}
