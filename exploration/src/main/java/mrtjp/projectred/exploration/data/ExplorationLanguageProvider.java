package mrtjp.projectred.exploration.data;

import net.minecraft.data.DataGenerator;
import net.minecraftforge.common.data.LanguageProvider;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.exploration.init.ExplorationReferences.*;

public class ExplorationLanguageProvider extends LanguageProvider {

    public ExplorationLanguageProvider(DataGenerator gen) {
        super(gen, MOD_ID, "en_us");
    }

    @Override
    public String getName() {
        return "ProjectRed-Exploration Language Provider: en_us";
    }

    @Override
    protected void addTranslations() {

        // Creative tab
        add("itemGroup." + MOD_ID, "Project Red: Exploration");

        // Ores
        add(RUBY_ORE_BLOCK, "Ruby Ore");
        add(DEEPSLATE_RUBY_ORE_BLOCK, "Deepslate Ruby Ore");
        add(SAPPHIRE_ORE_BLOCK, "Sapphire Ore");
        add(DEEPSLATE_SAPPHIRE_ORE_BLOCK, "Deepslate Sapphire Ore");
        add(PERIDOT_ORE_BLOCK, "Peridot Ore");
        add(DEEPSLATE_PERIDOT_ORE_BLOCK, "Deepslate Peridot Ore");
        add(TIN_ORE_BLOCK, "Tin Ore");
        add(DEEPSLATE_TIN_ORE_BLOCK, "Deepslate Tin Ore");
        add(SILVER_ORE_BLOCK, "Silver Ore");
        add(DEEPSLATE_SILVER_ORE_BLOCK, "Deepslate Silver Ore");
        add(ELECTROTINE_ORE_BLOCK, "Electrotine Ore");
        add(DEEPSLATE_ELECTROTINE_ORE_BLOCK, "Deepslate Electrotine Ore");

        // Decorative blocks
        add(MARBLE_BLOCK, "Marble");
        add(MARBLE_BRICK_BLOCK, "Marble Brick");
        add(BASALT_BLOCK, "Basalt");
        add(BASALT_COBBLE_BLOCK, "Basalt Cobblestone");
        add(BASALT_BRICK_BLOCK, "Basalt Brick");
        add(RUBY_BLOCK, "Ruby Block");
        add(SAPPHIRE_BLOCK, "Sapphire Block");
        add(PERIDOT_BLOCK, "Peridot Block");
        add(ELECTROTINE_BLOCK, "Electrotine Block");
        add(RAW_TIN_BLOCK, "Raw Tin Block");
        add(RAW_SILVER_BLOCK, "Raw Silver Block");
        add(TIN_BLOCK, "Tin Block");
        add(SILVER_BLOCK, "Silver Block");

        // Walls
        add(MARBLE_WALL, "Marble Wall");
        add(MARBLE_BRICK_WALL, "Marble Brick Wall");
        add(BASALT_WALL, "Basalt Wall");
        add(BASALT_COBBLE_WALL, "Basalt Cobblestone Wall");
        add(BASALT_BRICK_WALL, "Basalt Brick Wall");
        add(RUBY_BLOCK_WALL, "Ruby Wall");
        add(SAPPHIRE_BLOCK_WALL, "Sapphire Wall");
        add(PERIDOT_BLOCK_WALL, "Peridot Wall");
        add(ELECTROTINE_BLOCK_WALL, "Electrotine Wall");

        // Items
        add(RAW_TIN_ITEM, "Raw Tin");
        add(TIN_INGOT_ITEM, "Tin Ingot");
        add(RAW_SILVER_ITEM, "Raw Silver");
        add(SILVER_INGOT_ITEM, "Silver Ingot");
        add(WOOL_GIN, "Wool Gin");
        add(ATHAME, "Athame");

        // Axes
        add(RUBY_AXE, "Ruby Axe");
        add(SAPPHIRE_AXE, "Sapphire Axe");
        add(PERIDOT_AXE, "Peridot Axe");

        // Pickaxes
        add(RUBY_PICKAXE, "Ruby Pickaxe");
        add(SAPPHIRE_PICKAXE, "Sapphire Pickaxe");
        add(PERIDOT_PICKAXE, "Peridot Pickaxe");

        // Shovels
        add(RUBY_SHOVEL, "Ruby Shovel");
        add(SAPPHIRE_SHOVEL, "Sapphire Shovel");
        add(PERIDOT_SHOVEL, "Peridot Shovel");

        // Hoes
        add(RUBY_HOE, "Ruby Hoe");
        add(SAPPHIRE_HOE, "Sapphire Hoe");
        add(PERIDOT_HOE, "Peridot Hoe");

        // Swords
        add(RUBY_SWORD, "Ruby Sword");
        add(SAPPHIRE_SWORD, "Sapphire Sword");
        add(PERIDOT_SWORD, "Peridot Sword");

        // Helmets
        add(RUBY_HELMET, "Ruby Helmet");
        add(SAPPHIRE_HELMET, "Sapphire Helmet");
        add(PERIDOT_HELMET, "Peridot Helmet");

        // Chestplates
        add(RUBY_CHESTPLATE, "Ruby Chestplate");
        add(SAPPHIRE_CHESTPLATE, "Sapphire Chestplate");
        add(PERIDOT_CHESTPLATE, "Peridot Chestplate");

        // Leggings
        add(RUBY_LEGGINGS, "Ruby Leggings");
        add(SAPPHIRE_LEGGINGS, "Sapphire Leggings");
        add(PERIDOT_LEGGINGS, "Peridot Leggings");

        // Boots
        add(RUBY_BOOTS, "Ruby Boots");
        add(SAPPHIRE_BOOTS, "Sapphire Boots");
        add(PERIDOT_BOOTS, "Peridot Boots");

        // Saws
        add(GOLD_SAW, "Gold Saw");
        add(RUBY_SAW, "Ruby Saw");
        add(SAPPHIRE_SAW, "Sapphire Saw");
        add(PERIDOT_SAW, "Peridot Saw");

        // Sickles
        add(WOOD_SICKLE, "Wood Sickle");
        add(STONE_SICKLE, "Stone Sickle");
        add(IRON_SICKLE, "Iron Sickle");
        add(GOLD_SICKLE, "Gold Sickle");
        add(DIAMOND_SICKLE, "Diamond Sickle");
        add(RUBY_SICKLE, "Ruby Sickle");
        add(SAPPHIRE_SICKLE, "Sapphire Sickle");
        add(PERIDOT_SICKLE, "Peridot Sickle");

        // Backpacks
        add(WHITE_BACKPACK, "White Backpack");
        add(ORANGE_BACKPACK, "Orange Backpack");
        add(MAGENTA_BACKPACK, "Magenta Backpack");
        add(LIGHT_BLUE_BACKPACK, "Light Blue Backpack");
        add(YELLOW_BACKPACK, "Yellow Backpack");
        add(LIME_BACKPACK, "Lime Backpack");
        add(PINK_BACKPACK, "Pink Backpack");
        add(GRAY_BACKPACK, "Gray Backpack");
        add(LIGHT_GRAY_BACKPACK, "Light Gray Backpack");
        add(CYAN_BACKPACK, "Cyan Backpack");
        add(PURPLE_BACKPACK, "Purple Backpack");
        add(BLUE_BACKPACK, "Blue Backpack");
        add(BROWN_BACKPACK, "Brown Backpack");
        add(GREEN_BACKPACK, "Green Backpack");
        add(RED_BACKPACK, "Red Backpack");
        add(BLACK_BACKPACK, "Black Backpack");
    }
}
