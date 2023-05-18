package mrtjp.projectred.exploration.data;

import codechicken.lib.datagen.recipe.RecipeProvider;
import codechicken.lib.datagen.recipe.ShapedRecipeBuilder;
import codechicken.microblock.handler.MicroblockModContent;
import mrtjp.projectred.exploration.item.BackpackItem;
import net.minecraft.data.DataGenerator;
import net.minecraft.item.DyeColor;
import net.minecraft.item.Item;
import net.minecraft.item.Items;
import net.minecraft.tags.ITag;
import net.minecraft.tags.ItemTags;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.common.Tags;

import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.core.init.CoreReferences.*;
import static mrtjp.projectred.core.init.CoreTags.*;
import static mrtjp.projectred.exploration.init.ExplorationReferences.*;
import static mrtjp.projectred.exploration.init.ExplorationTags.*;

public class ExplorationRecipeProvider extends RecipeProvider {

    public ExplorationRecipeProvider(DataGenerator generatorIn) {
        super(generatorIn);
    }

    @Override
    public String getName() {
        return "ProjectRed-Exploration Recipes";
    }

    @Override
    protected void registerRecipes() {

        // Ores
        smelting(BASALT_BLOCK)
                .ingredient(BASALT_COBBLE_BLOCK)
                .experience(0.1F);

        smelting(RUBY_ITEM, 1, new ResourceLocation(MOD_ID, "ruby_from_ore")) //Note: Special names for json bc multiple recipes here producing same item
                .ingredient(RUBY_ORE_BLOCK)
                .experience(1F);

        smelting(SAPPHIRE_ITEM, 1, new ResourceLocation(MOD_ID, "sapphire_from_ore"))
                .ingredient(SAPPHIRE_ORE_BLOCK)
                .experience(1F);

        smelting(PERIDOT_ITEM, 1, new ResourceLocation(MOD_ID, "peridot_from_ore"))
                .ingredient(PERIDOT_ORE_BLOCK)
                .experience(1F);

        smelting(COPPER_INGOT_ITEM, 1, new ResourceLocation(MOD_ID, "copper_from_ore"))
                .ingredient(COPPER_ORE_BLOCK)
                .experience(0.7F);

        smelting(TIN_INGOT_ITEM, 1, new ResourceLocation(MOD_ID, "tin_from_ore"))
                .ingredient(TIN_ORE_BLOCK)
                .experience(0.7F);

        smelting(SILVER_INGOT_ITEM, 1, new ResourceLocation(MOD_ID, "silver_from_ore"))
                .ingredient(SILVER_ORE_BLOCK)
                .experience(0.7F);

        smelting(ELECTROTINE_DUST_ITEM, 1, new ResourceLocation(MOD_ID, "electrotine_from_ore"))
                .ingredient(ELECTROTINE_ORE_BLOCK)
                .experience(0.7F);

        // Decorative blocks
        shapedRecipe(MARBLE_BRICK_BLOCK, 4)
                .key('B', MARBLE_BLOCK)
                .patternLine("BB")
                .patternLine("BB");

        shapedRecipe(BASALT_BRICK_BLOCK, 4)
                .key('B', BASALT_BLOCK)
                .patternLine("BB")
                .patternLine("BB");

        shapedRecipe(RUBY_BLOCK)
                .key('S', RUBY_GEM_TAG)
                .patternLine("SSS")
                .patternLine("SSS")
                .patternLine("SSS");

        shapelessRecipe(RUBY_ITEM, 9, new ResourceLocation(MOD_ID, "ruby_from_block"))
                .addIngredient(RUBY_STORAGE_BLOCK_ITEM_TAG);

        shapedRecipe(SAPPHIRE_BLOCK)
                .key('S', SAPPHIRE_GEM_TAG)
                .patternLine("SSS")
                .patternLine("SSS")
                .patternLine("SSS");

        shapelessRecipe(SAPPHIRE_ITEM, 9, new ResourceLocation(MOD_ID, "sapphire_from_block"))
                .addIngredient(SAPPHIRE_STORAGE_BLOCK_ITEM_TAG);

        shapedRecipe(PERIDOT_BLOCK)
                .key('S', PERIDOT_GEM_TAG)
                .patternLine("SSS")
                .patternLine("SSS")
                .patternLine("SSS");

        shapelessRecipe(PERIDOT_ITEM, 9, new ResourceLocation(MOD_ID, "peridot_from_block"))
                .addIngredient(PERIDOT_STORAGE_BLOCK_ITEM_TAG);

        shapedRecipe(COPPER_BLOCK)
                .key('S', COPPER_INGOT_TAG)
                .patternLine("SSS")
                .patternLine("SSS")
                .patternLine("SSS");

        shapelessRecipe(COPPER_INGOT_ITEM, 9, new ResourceLocation(MOD_ID, "copper_from_block"))
                .addIngredient(COPPER_STORAGE_BLOCK_ITEM_TAG);

        shapedRecipe(TIN_BLOCK)
                .key('S', TIN_INGOT_TAG)
                .patternLine("SSS")
                .patternLine("SSS")
                .patternLine("SSS");

        shapelessRecipe(TIN_INGOT_ITEM, 9, new ResourceLocation(MOD_ID, "tin_from_block"))
                .addIngredient(TIN_ORE_BLOCK_ITEM_TAG);

        shapedRecipe(SILVER_BLOCK)
                .key('S', SILVER_INGOT_TAG)
                .patternLine("SSS")
                .patternLine("SSS")
                .patternLine("SSS");

        shapelessRecipe(SILVER_INGOT_ITEM, 9, new ResourceLocation(MOD_ID, "silver_from_block"))
                .addIngredient(SILVER_STORAGE_BLOCK_ITEM_TAG);

        shapedRecipe(ELECTROTINE_BLOCK)
                .key('S', ELECTROTINE_DUST_TAG)
                .patternLine("SSS")
                .patternLine("SSS")
                .patternLine("SSS");

        shapelessRecipe(ELECTROTINE_DUST_ITEM, 9, new ResourceLocation(MOD_ID, "electrotine_from_block"))
                .addIngredient(ELECTROTINE_STORAGE_BLOCK_ITEM_TAG);

        // Walls
        shapedRecipe(MARBLE_WALL, 6)
                .key('S', MARBLE_BLOCK_ITEM_TAG)
                .patternLine("SSS")
                .patternLine("SSS");

        shapedRecipe(MARBLE_BRICK_WALL, 6)
                .key('S', MARBLE_BRICK_BLOCK)
                .patternLine("SSS")
                .patternLine("SSS");

        shapedRecipe(BASALT_WALL, 6)
                .key('S', BASALT_BLOCK_ITEM_TAG)
                .patternLine("SSS")
                .patternLine("SSS");

        shapedRecipe(BASALT_COBBLE_WALL, 6)
                .key('S', BASALT_COBBLE_BLOCK)
                .patternLine("SSS")
                .patternLine("SSS");

        shapedRecipe(BASALT_BRICK_WALL, 6)
                .key('S', BASALT_BRICK_BLOCK)
                .patternLine("SSS")
                .patternLine("SSS");

        shapedRecipe(RUBY_BLOCK_WALL, 6)
                .key('S', RUBY_STORAGE_BLOCK_ITEM_TAG)
                .patternLine("SSS")
                .patternLine("SSS");

        shapedRecipe(SAPPHIRE_BLOCK_WALL, 6)
                .key('S', SAPPHIRE_STORAGE_BLOCK_ITEM_TAG)
                .patternLine("SSS")
                .patternLine("SSS");

        shapedRecipe(PERIDOT_BLOCK_WALL, 6)
                .key('S', PERIDOT_STORAGE_BLOCK_ITEM_TAG)
                .patternLine("SSS")
                .patternLine("SSS");

        shapedRecipe(COPPER_BLOCK_WALL, 6)
                .key('S', COPPER_STORAGE_BLOCK_ITEM_TAG)
                .patternLine("SSS")
                .patternLine("SSS");

        shapedRecipe(TIN_BLOCK_WALL, 6)
                .key('S', TIN_STORAGE_BLOCK_ITEM_TAG)
                .patternLine("SSS")
                .patternLine("SSS");

        shapedRecipe(SILVER_BLOCK_WALL, 6)
                .key('S', SILVER_STORAGE_BLOCK_ITEM_TAG)
                .patternLine("SSS")
                .patternLine("SSS");

        shapedRecipe(ELECTROTINE_BLOCK_WALL, 6)
                .key('S', ELECTROTINE_STORAGE_BLOCK_ITEM_TAG)
                .patternLine("SSS")
                .patternLine("SSS");

        // Wool gin

        shapedRecipe(WOOL_GIN)
                .key('S', Tags.Items.RODS_WOODEN)
                .key('I', IRON_COIL_ITEM)
                .patternLine("SIS")
                .patternLine("SSS")
                .patternLine(" S ");

        shapedRecipe(Items.STRING, 4, new ResourceLocation(MOD_ID, "string_from_wool_gin"))
                .key('W', ItemTags.WOOL)
                .key('G', WOOL_GIN)
                .patternLine("GW");

        // Tools

        shapedRecipe(ATHAME)
                .key('W', Tags.Items.RODS_WOODEN)
                .key('S', SILVER_INGOT_TAG)
                .patternLine("S")
                .patternLine("W");

        axeRecipe(RUBY_AXE, RUBY_GEM_TAG);
        axeRecipe(SAPPHIRE_AXE, SAPPHIRE_GEM_TAG);
        axeRecipe(PERIDOT_AXE, PERIDOT_GEM_TAG);

        pickaxeRecipe(RUBY_PICKAXE, RUBY_GEM_TAG);
        pickaxeRecipe(SAPPHIRE_PICKAXE, SAPPHIRE_GEM_TAG);
        pickaxeRecipe(PERIDOT_PICKAXE, PERIDOT_GEM_TAG);

        shovelRecipe(RUBY_SHOVEL, RUBY_GEM_TAG);
        shovelRecipe(SAPPHIRE_SHOVEL, SAPPHIRE_GEM_TAG);
        shovelRecipe(PERIDOT_SHOVEL, PERIDOT_GEM_TAG);

        hoeRecipe(RUBY_HOE, RUBY_GEM_TAG);
        hoeRecipe(SAPPHIRE_HOE, SAPPHIRE_GEM_TAG);
        hoeRecipe(PERIDOT_HOE, PERIDOT_GEM_TAG);

        swordRecipe(RUBY_SWORD, RUBY_GEM_TAG);
        swordRecipe(SAPPHIRE_SWORD, SAPPHIRE_GEM_TAG);
        swordRecipe(PERIDOT_SWORD, PERIDOT_GEM_TAG);

        sawRecipe(GOLD_SAW, Tags.Items.INGOTS_GOLD);
        sawRecipe(RUBY_SAW, RUBY_GEM_TAG);
        sawRecipe(SAPPHIRE_SAW, SAPPHIRE_GEM_TAG);
        sawRecipe(PERIDOT_SAW, PERIDOT_GEM_TAG);

        sickleRecipe(WOOD_SICKLE, Tags.Items.RODS_WOODEN);
        sickleRecipe(STONE_SICKLE, Tags.Items.COBBLESTONE);
        sickleRecipe(IRON_SICKLE, Tags.Items.INGOTS_IRON);
        sickleRecipe(GOLD_SICKLE, Tags.Items.INGOTS_GOLD);
        sickleRecipe(DIAMOND_SICKLE, Tags.Items.GEMS_DIAMOND);
        sickleRecipe(RUBY_SICKLE, RUBY_GEM_TAG);
        sickleRecipe(SAPPHIRE_SICKLE, SAPPHIRE_GEM_TAG);
        sickleRecipe(PERIDOT_SICKLE, PERIDOT_GEM_TAG);

        // Armor

        helmetRecipe(RUBY_HELMET, RUBY_GEM_TAG);
        helmetRecipe(SAPPHIRE_HELMET, SAPPHIRE_GEM_TAG);
        helmetRecipe(PERIDOT_HELMET, PERIDOT_GEM_TAG);

        chestplateRecipe(RUBY_CHESTPLATE, RUBY_GEM_TAG);
        chestplateRecipe(SAPPHIRE_CHESTPLATE, SAPPHIRE_GEM_TAG);
        chestplateRecipe(PERIDOT_CHESTPLATE, PERIDOT_GEM_TAG);

        leggingsRecipe(RUBY_LEGGINGS, RUBY_GEM_TAG);
        leggingsRecipe(SAPPHIRE_LEGGINGS, SAPPHIRE_GEM_TAG);
        leggingsRecipe(PERIDOT_LEGGINGS, PERIDOT_GEM_TAG);

        bootsRecipe(RUBY_BOOTS, RUBY_GEM_TAG);
        bootsRecipe(SAPPHIRE_BOOTS, SAPPHIRE_GEM_TAG);
        bootsRecipe(PERIDOT_BOOTS, PERIDOT_GEM_TAG);

        // Backpacks
        for (int i = 0; i < 15; i++) {
            backpackRecipe(getBackpackByColor(i));
        }

        special(BACKPACK_DYE_RECIPE_SERIALIZER, new ResourceLocation(MOD_ID, "backpack_dye"));
    }

    private void axeRecipe(Item axe, ITag<Item> material) {
        shapedRecipe(axe)
                .key('M', material)
                .key('S', Tags.Items.RODS_WOODEN)
                .patternLine("MM")
                .patternLine("MS")
                .patternLine(" S");
    }

    private void pickaxeRecipe(Item pickaxe, ITag<Item> material) {
        shapedRecipe(pickaxe)
                .key('M', material)
                .key('S', Tags.Items.RODS_WOODEN)
                .patternLine("MMM")
                .patternLine(" S ")
                .patternLine(" S ");
    }

    private void shovelRecipe(Item shovel, ITag<Item> material) {
        shapedRecipe(shovel)
                .key('M', material)
                .key('S', Tags.Items.RODS_WOODEN)
                .patternLine("M")
                .patternLine("S")
                .patternLine("S");
    }

    private void hoeRecipe(Item hoe, ITag<Item> material) {
        shapedRecipe(hoe)
                .key('M', material)
                .key('S', Tags.Items.RODS_WOODEN)
                .patternLine("MM")
                .patternLine(" S")
                .patternLine(" S");
    }

    private void swordRecipe(Item sword, ITag<Item> material) {
        shapedRecipe(sword)
                .key('M', material)
                .key('S', Tags.Items.RODS_WOODEN)
                .patternLine("M")
                .patternLine("M")
                .patternLine("S");
    }

    private void sawRecipe(Item saw, ITag<Item> material) {
        shapedRecipe(saw)
                .key('M', material)
                .key('S', Tags.Items.RODS_WOODEN)
                .key('R', MicroblockModContent.stoneRodTag())
                .patternLine("SRR")
                .patternLine("SMM");
    }

    private void sickleRecipe(Item sickle, ITag<Item> material) {
        shapedRecipe(sickle)
                .key('M', material)
                .key('S', Tags.Items.RODS_WOODEN)
                .patternLine(" M ")
                .patternLine("  M")
                .patternLine("SM ");
    }

    private void helmetRecipe(Item helmet, ITag<Item> material) {
        shapedRecipe(helmet)
                .key('M', material)
                .patternLine("MMM")
                .patternLine("M M");
    }

    private void chestplateRecipe(Item chestplate, ITag<Item> material) {
        shapedRecipe(chestplate)
                .key('M', material)
                .patternLine("M M")
                .patternLine("MMM")
                .patternLine("MMM");
    }

    private void leggingsRecipe(Item leggings, ITag<Item> material) {
        shapedRecipe(leggings)
                .key('M', material)
                .patternLine("MMM")
                .patternLine("M M")
                .patternLine("M M");
    }

    private void bootsRecipe(Item boots, ITag<Item> material) {
        shapedRecipe(boots)
                .key('M', material)
                .patternLine("M M")
                .patternLine("M M");
    }

    private void backpackRecipe(Item backpack) {

        DyeColor color = ((BackpackItem) backpack).getDyeColor();

        ShapedRecipeBuilder builder = shapedRecipe(backpack)
                .key('C', WOVEN_CLOTH_ITEM);

        if (color != DyeColor.WHITE) { // White is default and doesn't need a dye
            builder.patternLine("CCC")
                    .patternLine("C C")
                    .patternLine("CCC");
        } else {
            builder.key('D', color.getTag())
                    .patternLine("CCC")
                    .patternLine("CDC")
                    .patternLine("CCC");
        }
    }
}
