package mrtjp.projectred.exploration.data;

import net.minecraft.data.DataGenerator;
import net.minecraftforge.common.data.LanguageProvider;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.exploration.init.ExplorationBlocks.*;
import static mrtjp.projectred.exploration.init.ExplorationItems.*;

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
        add(RUBY_ORE_BLOCK.get(), "Ruby Ore");
        add(DEEPSLATE_RUBY_ORE_BLOCK.get(), "Deepslate Ruby Ore");
        add(SAPPHIRE_ORE_BLOCK.get(), "Sapphire Ore");
        add(DEEPSLATE_SAPPHIRE_ORE_BLOCK.get(), "Deepslate Sapphire Ore");
        add(PERIDOT_ORE_BLOCK.get(), "Peridot Ore");
        add(DEEPSLATE_PERIDOT_ORE_BLOCK.get(), "Deepslate Peridot Ore");
        add(TIN_ORE_BLOCK.get(), "Tin Ore");
        add(DEEPSLATE_TIN_ORE_BLOCK.get(), "Deepslate Tin Ore");
        add(SILVER_ORE_BLOCK.get(), "Silver Ore");
        add(DEEPSLATE_SILVER_ORE_BLOCK.get(), "Deepslate Silver Ore");
        add(ELECTROTINE_ORE_BLOCK.get(), "Electrotine Ore");
        add(DEEPSLATE_ELECTROTINE_ORE_BLOCK.get(), "Deepslate Electrotine Ore");

        // Decorative blocks
        add(MARBLE_BLOCK.get(), "Marble");
        add(MARBLE_BRICK_BLOCK.get(), "Marble Brick");
        add(BASALT_BLOCK.get(), "Basalt");
        add(BASALT_COBBLE_BLOCK.get(), "Basalt Cobblestone");
        add(BASALT_BRICK_BLOCK.get(), "Basalt Brick");
        add(RUBY_BLOCK.get(), "Ruby Block");
        add(SAPPHIRE_BLOCK.get(), "Sapphire Block");
        add(PERIDOT_BLOCK.get(), "Peridot Block");
        add(ELECTROTINE_BLOCK.get(), "Electrotine Block");
        add(RAW_TIN_BLOCK.get(), "Raw Tin Block");
        add(RAW_SILVER_BLOCK.get(), "Raw Silver Block");
        add(TIN_BLOCK.get(), "Tin Block");
        add(SILVER_BLOCK.get(), "Silver Block");

        // Walls
        add(MARBLE_WALL.get(), "Marble Wall");
        add(MARBLE_BRICK_WALL.get(), "Marble Brick Wall");
        add(BASALT_WALL.get(), "Basalt Wall");
        add(BASALT_COBBLE_WALL.get(), "Basalt Cobblestone Wall");
        add(BASALT_BRICK_WALL.get(), "Basalt Brick Wall");
        add(RUBY_BLOCK_WALL.get(), "Ruby Wall");
        add(SAPPHIRE_BLOCK_WALL.get(), "Sapphire Wall");
        add(PERIDOT_BLOCK_WALL.get(), "Peridot Wall");
        add(ELECTROTINE_BLOCK_WALL.get(), "Electrotine Wall");

        // Items
        add(RAW_TIN_ITEM.get(), "Raw Tin");
        add(TIN_INGOT_ITEM.get(), "Tin Ingot");
        add(RAW_SILVER_ITEM.get(), "Raw Silver");
        add(SILVER_INGOT_ITEM.get(), "Silver Ingot");
        add(WOOL_GIN.get(), "Wool Gin");
        add(ATHAME.get(), "Athame");

        // Axes
        add(RUBY_AXE.get(), "Ruby Axe");
        add(SAPPHIRE_AXE.get(), "Sapphire Axe");
        add(PERIDOT_AXE.get(), "Peridot Axe");

        // Pickaxes
        add(RUBY_PICKAXE.get(), "Ruby Pickaxe");
        add(SAPPHIRE_PICKAXE.get(), "Sapphire Pickaxe");
        add(PERIDOT_PICKAXE.get(), "Peridot Pickaxe");

        // Shovels
        add(RUBY_SHOVEL.get(), "Ruby Shovel");
        add(SAPPHIRE_SHOVEL.get(), "Sapphire Shovel");
        add(PERIDOT_SHOVEL.get(), "Peridot Shovel");

        // Hoes
        add(RUBY_HOE.get(), "Ruby Hoe");
        add(SAPPHIRE_HOE.get(), "Sapphire Hoe");
        add(PERIDOT_HOE.get(), "Peridot Hoe");

        // Swords
        add(RUBY_SWORD.get(), "Ruby Sword");
        add(SAPPHIRE_SWORD.get(), "Sapphire Sword");
        add(PERIDOT_SWORD.get(), "Peridot Sword");

        // Helmets
        add(RUBY_HELMET.get(), "Ruby Helmet");
        add(SAPPHIRE_HELMET.get(), "Sapphire Helmet");
        add(PERIDOT_HELMET.get(), "Peridot Helmet");

        // Chestplates
        add(RUBY_CHESTPLATE.get(), "Ruby Chestplate");
        add(SAPPHIRE_CHESTPLATE.get(), "Sapphire Chestplate");
        add(PERIDOT_CHESTPLATE.get(), "Peridot Chestplate");

        // Leggings
        add(RUBY_LEGGINGS.get(), "Ruby Leggings");
        add(SAPPHIRE_LEGGINGS.get(), "Sapphire Leggings");
        add(PERIDOT_LEGGINGS.get(), "Peridot Leggings");

        // Boots
        add(RUBY_BOOTS.get(), "Ruby Boots");
        add(SAPPHIRE_BOOTS.get(), "Sapphire Boots");
        add(PERIDOT_BOOTS.get(), "Peridot Boots");

        // Saws
        add(GOLD_SAW.get(), "Gold Saw");
        add(RUBY_SAW.get(), "Ruby Saw");
        add(SAPPHIRE_SAW.get(), "Sapphire Saw");
        add(PERIDOT_SAW.get(), "Peridot Saw");

        // Sickles
        add(WOOD_SICKLE.get(), "Wood Sickle");
        add(STONE_SICKLE.get(), "Stone Sickle");
        add(IRON_SICKLE.get(), "Iron Sickle");
        add(GOLD_SICKLE.get(), "Gold Sickle");
        add(DIAMOND_SICKLE.get(), "Diamond Sickle");
        add(RUBY_SICKLE.get(), "Ruby Sickle");
        add(SAPPHIRE_SICKLE.get(), "Sapphire Sickle");
        add(PERIDOT_SICKLE.get(), "Peridot Sickle");

        // Backpacks
        add(WHITE_BACKPACK.get(), "White Backpack");
        add(ORANGE_BACKPACK.get(), "Orange Backpack");
        add(MAGENTA_BACKPACK.get(), "Magenta Backpack");
        add(LIGHT_BLUE_BACKPACK.get(), "Light Blue Backpack");
        add(YELLOW_BACKPACK.get(), "Yellow Backpack");
        add(LIME_BACKPACK.get(), "Lime Backpack");
        add(PINK_BACKPACK.get(), "Pink Backpack");
        add(GRAY_BACKPACK.get(), "Gray Backpack");
        add(LIGHT_GRAY_BACKPACK.get(), "Light Gray Backpack");
        add(CYAN_BACKPACK.get(), "Cyan Backpack");
        add(PURPLE_BACKPACK.get(), "Purple Backpack");
        add(BLUE_BACKPACK.get(), "Blue Backpack");
        add(BROWN_BACKPACK.get(), "Brown Backpack");
        add(GREEN_BACKPACK.get(), "Green Backpack");
        add(RED_BACKPACK.get(), "Red Backpack");
        add(BLACK_BACKPACK.get(), "Black Backpack");
    }
}
