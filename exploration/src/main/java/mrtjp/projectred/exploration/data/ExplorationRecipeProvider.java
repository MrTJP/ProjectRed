package mrtjp.projectred.exploration.data;

import codechicken.lib.datagen.recipe.RecipeProvider;
import codechicken.lib.datagen.recipe.ShapedRecipeBuilder;
import codechicken.microblock.init.CBMicroblockModContent;
import mrtjp.projectred.exploration.item.BackpackItem;
import net.minecraft.data.DataGenerator;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.tags.ItemTags;
import net.minecraft.tags.TagKey;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.ItemLike;
import net.minecraftforge.common.Tags;

import java.util.Collection;
import java.util.List;

import static mrtjp.projectred.core.init.CoreReferences.*;
import static mrtjp.projectred.core.init.CoreTags.*;
import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;
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
        oreSmeltingRecipe(RUBY_ITEM, List.of(RUBY_ORE_BLOCK, DEEPSLATE_RUBY_ORE_BLOCK), 1F);
        oreSmeltingRecipe(SAPPHIRE_ITEM, List.of(SAPPHIRE_ORE_BLOCK, DEEPSLATE_SAPPHIRE_ORE_BLOCK), 1F);
        oreSmeltingRecipe(PERIDOT_ITEM, List.of(PERIDOT_ORE_BLOCK, DEEPSLATE_PERIDOT_ORE_BLOCK), 1F);
        oreSmeltingRecipe(ELECTROTINE_DUST_ITEM, List.of(ELECTROTINE_ORE_BLOCK, DEEPSLATE_ELECTROTINE_ORE_BLOCK), 0.7F);
        oreSmeltingRecipe(TIN_INGOT_ITEM, List.of(TIN_ORE_BLOCK, DEEPSLATE_TIN_ORE_BLOCK, RAW_TIN_ITEM), 0.7F);
        oreSmeltingRecipe(SILVER_INGOT_ITEM, List.of(SILVER_ORE_BLOCK, DEEPSLATE_SILVER_ORE_BLOCK, RAW_TIN_ITEM), 0.7F);

        // Decorative blocks
        smelting(BASALT_BLOCK)
                .ingredient(BASALT_COBBLE_BLOCK)
                .experience(0.1F);

        shapedRecipe(MARBLE_BRICK_BLOCK, 4)
                .key('B', MARBLE_BLOCK)
                .patternLine("BB")
                .patternLine("BB");

        shapedRecipe(BASALT_BRICK_BLOCK, 4)
                .key('B', BASALT_BLOCK)
                .patternLine("BB")
                .patternLine("BB");

        nineBlockStorageRecipes(RUBY_GEM_TAG, RUBY_BLOCK, RUBY_ITEM);
        nineBlockStorageRecipes(SAPPHIRE_GEM_TAG, SAPPHIRE_BLOCK, SAPPHIRE_ITEM);
        nineBlockStorageRecipes(PERIDOT_GEM_TAG, PERIDOT_BLOCK, PERIDOT_ITEM);
        nineBlockStorageRecipes(ELECTROTINE_DUST_TAG, ELECTROTINE_BLOCK, ELECTROTINE_DUST_ITEM);
        nineBlockStorageRecipes(RAW_MATERIALS_TIN_TAG, RAW_TIN_BLOCK, RAW_TIN_ITEM);
        nineBlockStorageRecipes(TIN_INGOT_TAG, TIN_BLOCK, TIN_INGOT_ITEM);
        nineBlockStorageRecipes(RAW_MATERIALS_SILVER_TAG, RAW_SILVER_BLOCK, RAW_SILVER_ITEM);
        nineBlockStorageRecipes(SILVER_INGOT_TAG, SILVER_BLOCK, SILVER_INGOT_ITEM);

        // Walls
        wallRecipe(MARBLE_WALL, MARBLE_BLOCK_ITEM_TAG);
        wallRecipe(MARBLE_BRICK_WALL, MARBLE_BRICK_BLOCK);
        wallRecipe(BASALT_WALL, BASALT_BLOCK_ITEM_TAG);
        wallRecipe(BASALT_COBBLE_WALL, BASALT_COBBLE_BLOCK);
        wallRecipe(BASALT_BRICK_WALL, BASALT_BRICK_BLOCK);
        wallRecipe(RUBY_BLOCK_WALL, RUBY_STORAGE_BLOCK_ITEM_TAG);
        wallRecipe(SAPPHIRE_BLOCK_WALL, SAPPHIRE_STORAGE_BLOCK_ITEM_TAG);
        wallRecipe(PERIDOT_BLOCK_WALL, PERIDOT_STORAGE_BLOCK_ITEM_TAG);
        wallRecipe(ELECTROTINE_BLOCK_WALL, ELECTROTINE_STORAGE_BLOCK_ITEM_TAG);

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

    private void oreSmeltingRecipe(ItemLike result, Collection<ItemLike> sources, float xp) {
        String resultName = result.asItem().getRegistryName().getPath();

        for (ItemLike source : sources) {
            String sourceName = source.asItem().getRegistryName().getPath();

            smelting(result, 1, new ResourceLocation(MOD_ID, resultName + "_from_" + sourceName + "_smelting"))
                    .ingredient(source)
                    .experience(xp);
        }
    }

    private void nineBlockStorageRecipes(TagKey<Item> itemTag, ItemLike block, ItemLike item) {
        // Item to block
        shapedRecipe(block)
                .key('S', itemTag)
                .patternLine("SSS")
                .patternLine("SSS")
                .patternLine("SSS");

        // Block to item
        shapelessRecipe(item, 9, new ResourceLocation(MOD_ID, item.asItem().getRegistryName().getPath() + "_from_nineblock"))
                .addIngredient(block);
    }

    private void wallRecipe(ItemLike result, ItemLike input) {
        shapedRecipe(result, 6)
                .key('S', input)
                .patternLine("SSS")
                .patternLine("SSS");
    }

    private void wallRecipe(ItemLike result, TagKey<Item> input) {
        shapedRecipe(result, 6)
                .key('S', input)
                .patternLine("SSS")
                .patternLine("SSS");
    }

    private void axeRecipe(Item axe, TagKey<Item> material) {
        shapedRecipe(axe)
                .key('M', material)
                .key('S', Tags.Items.RODS_WOODEN)
                .patternLine("MM")
                .patternLine("MS")
                .patternLine(" S");
    }

    private void pickaxeRecipe(Item pickaxe, TagKey<Item> material) {
        shapedRecipe(pickaxe)
                .key('M', material)
                .key('S', Tags.Items.RODS_WOODEN)
                .patternLine("MMM")
                .patternLine(" S ")
                .patternLine(" S ");
    }

    private void shovelRecipe(Item shovel, TagKey<Item> material) {
        shapedRecipe(shovel)
                .key('M', material)
                .key('S', Tags.Items.RODS_WOODEN)
                .patternLine("M")
                .patternLine("S")
                .patternLine("S");
    }

    private void hoeRecipe(Item hoe, TagKey<Item> material) {
        shapedRecipe(hoe)
                .key('M', material)
                .key('S', Tags.Items.RODS_WOODEN)
                .patternLine("MM")
                .patternLine(" S")
                .patternLine(" S");
    }

    private void swordRecipe(Item sword, TagKey<Item> material) {
        shapedRecipe(sword)
                .key('M', material)
                .key('S', Tags.Items.RODS_WOODEN)
                .patternLine("M")
                .patternLine("M")
                .patternLine("S");
    }

    private void sawRecipe(Item saw, TagKey<Item> material) {
        shapedRecipe(saw)
                .key('M', material)
                .key('S', Tags.Items.RODS_WOODEN)
                .key('R', CBMicroblockModContent.STONE_ROD_ITEM.get())
                .patternLine("SRR")
                .patternLine("SMM");
    }

    private void sickleRecipe(Item sickle, TagKey<Item> material) {
        shapedRecipe(sickle)
                .key('M', material)
                .key('S', Tags.Items.RODS_WOODEN)
                .patternLine(" M ")
                .patternLine("  M")
                .patternLine("SM ");
    }

    private void helmetRecipe(Item helmet, TagKey<Item> material) {
        shapedRecipe(helmet)
                .key('M', material)
                .patternLine("MMM")
                .patternLine("M M");
    }

    private void chestplateRecipe(Item chestplate, TagKey<Item> material) {
        shapedRecipe(chestplate)
                .key('M', material)
                .patternLine("M M")
                .patternLine("MMM")
                .patternLine("MMM");
    }

    private void leggingsRecipe(Item leggings, TagKey<Item> material) {
        shapedRecipe(leggings)
                .key('M', material)
                .patternLine("MMM")
                .patternLine("M M")
                .patternLine("M M");
    }

    private void bootsRecipe(Item boots, TagKey<Item> material) {
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
