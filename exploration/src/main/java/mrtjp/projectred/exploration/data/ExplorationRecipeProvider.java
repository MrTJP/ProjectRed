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
import net.minecraftforge.registries.ForgeRegistries;

import java.util.Collection;
import java.util.List;

import static mrtjp.projectred.core.init.CoreItems.*;
import static mrtjp.projectred.core.init.CoreTags.*;
import static mrtjp.projectred.exploration.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.exploration.init.ExplorationBlocks.*;
import static mrtjp.projectred.exploration.init.ExplorationItems.*;
import static mrtjp.projectred.exploration.init.ExplorationRecipeSerializers.BACKPACK_DYE_RECIPE_SERIALIZER;
import static mrtjp.projectred.exploration.init.ExplorationTags.*;

@SuppressWarnings("DataFlowIssue")
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
        oreSmeltingRecipe(RUBY_ITEM.get(), List.of(RUBY_ORE_BLOCK.get(), DEEPSLATE_RUBY_ORE_BLOCK.get()), 1F);
        oreSmeltingRecipe(SAPPHIRE_ITEM.get(), List.of(SAPPHIRE_ORE_BLOCK.get(), DEEPSLATE_SAPPHIRE_ORE_BLOCK.get()), 1F);
        oreSmeltingRecipe(PERIDOT_ITEM.get(), List.of(PERIDOT_ORE_BLOCK.get(), DEEPSLATE_PERIDOT_ORE_BLOCK.get()), 1F);
        oreSmeltingRecipe(ELECTROTINE_DUST_ITEM.get(), List.of(ELECTROTINE_ORE_BLOCK.get(), DEEPSLATE_ELECTROTINE_ORE_BLOCK.get()), 0.7F);
        oreSmeltingRecipe(TIN_INGOT_ITEM.get(), List.of(TIN_ORE_BLOCK.get(), DEEPSLATE_TIN_ORE_BLOCK.get(), RAW_TIN_ITEM.get()), 0.7F);
        oreSmeltingRecipe(SILVER_INGOT_ITEM.get(), List.of(SILVER_ORE_BLOCK.get(), DEEPSLATE_SILVER_ORE_BLOCK.get(), RAW_TIN_ITEM.get()), 0.7F);

        // Decorative blocks
        smelting(BASALT_BLOCK.get())
                .ingredient(BASALT_COBBLE_BLOCK.get())
                .experience(0.1F);

        shapedRecipe(MARBLE_BRICK_BLOCK.get(), 4)
                .key('B', MARBLE_BLOCK.get())
                .patternLine("BB")
                .patternLine("BB");

        shapedRecipe(BASALT_BRICK_BLOCK.get(), 4)
                .key('B', BASALT_BLOCK.get())
                .patternLine("BB")
                .patternLine("BB");

        nineBlockStorageRecipes(RUBY_GEM_TAG, RUBY_BLOCK.get(), RUBY_ITEM.get());
        nineBlockStorageRecipes(SAPPHIRE_GEM_TAG, SAPPHIRE_BLOCK.get(), SAPPHIRE_ITEM.get());
        nineBlockStorageRecipes(PERIDOT_GEM_TAG, PERIDOT_BLOCK.get(), PERIDOT_ITEM.get());
        nineBlockStorageRecipes(ELECTROTINE_DUST_TAG, ELECTROTINE_BLOCK.get(), ELECTROTINE_DUST_ITEM.get());
        nineBlockStorageRecipes(RAW_MATERIALS_TIN_TAG, RAW_TIN_BLOCK.get(), RAW_TIN_ITEM.get());
        nineBlockStorageRecipes(TIN_INGOT_TAG, TIN_BLOCK.get(), TIN_INGOT_ITEM.get());
        nineBlockStorageRecipes(RAW_MATERIALS_SILVER_TAG, RAW_SILVER_BLOCK.get(), RAW_SILVER_ITEM.get());
        nineBlockStorageRecipes(SILVER_INGOT_TAG, SILVER_BLOCK.get(), SILVER_INGOT_ITEM.get());

        // Walls
        wallRecipe(MARBLE_WALL.get(), MARBLE_BLOCK_ITEM_TAG);
        wallRecipe(MARBLE_BRICK_WALL.get(), MARBLE_BRICK_BLOCK.get());
        wallRecipe(BASALT_WALL.get(), BASALT_BLOCK_ITEM_TAG);
        wallRecipe(BASALT_COBBLE_WALL.get(), BASALT_COBBLE_BLOCK.get());
        wallRecipe(BASALT_BRICK_WALL.get(), BASALT_BRICK_BLOCK.get());
        wallRecipe(RUBY_BLOCK_WALL.get(), RUBY_STORAGE_BLOCK_ITEM_TAG);
        wallRecipe(SAPPHIRE_BLOCK_WALL.get(), SAPPHIRE_STORAGE_BLOCK_ITEM_TAG);
        wallRecipe(PERIDOT_BLOCK_WALL.get(), PERIDOT_STORAGE_BLOCK_ITEM_TAG);
        wallRecipe(ELECTROTINE_BLOCK_WALL.get(), ELECTROTINE_STORAGE_BLOCK_ITEM_TAG);

        // Wool gin

        shapedRecipe(WOOL_GIN.get())
                .key('S', Tags.Items.RODS_WOODEN)
                .key('I', IRON_COIL_ITEM.get())
                .patternLine("SIS")
                .patternLine("SSS")
                .patternLine(" S ");

        shapedRecipe(Items.STRING, 4, new ResourceLocation(MOD_ID, "string_from_wool_gin"))
                .key('W', ItemTags.WOOL)
                .key('G', WOOL_GIN.get())
                .patternLine("GW");

        // Tools

        shapedRecipe(ATHAME.get())
                .key('W', Tags.Items.RODS_WOODEN)
                .key('S', SILVER_INGOT_TAG)
                .patternLine("S")
                .patternLine("W");

        axeRecipe(RUBY_AXE.get(), RUBY_GEM_TAG);
        axeRecipe(SAPPHIRE_AXE.get(), SAPPHIRE_GEM_TAG);
        axeRecipe(PERIDOT_AXE.get(), PERIDOT_GEM_TAG);

        pickaxeRecipe(RUBY_PICKAXE.get(), RUBY_GEM_TAG);
        pickaxeRecipe(SAPPHIRE_PICKAXE.get(), SAPPHIRE_GEM_TAG);
        pickaxeRecipe(PERIDOT_PICKAXE.get(), PERIDOT_GEM_TAG);

        shovelRecipe(RUBY_SHOVEL.get(), RUBY_GEM_TAG);
        shovelRecipe(SAPPHIRE_SHOVEL.get(), SAPPHIRE_GEM_TAG);
        shovelRecipe(PERIDOT_SHOVEL.get(), PERIDOT_GEM_TAG);

        hoeRecipe(RUBY_HOE.get(), RUBY_GEM_TAG);
        hoeRecipe(SAPPHIRE_HOE.get(), SAPPHIRE_GEM_TAG);
        hoeRecipe(PERIDOT_HOE.get(), PERIDOT_GEM_TAG);

        swordRecipe(RUBY_SWORD.get(), RUBY_GEM_TAG);
        swordRecipe(SAPPHIRE_SWORD.get(), SAPPHIRE_GEM_TAG);
        swordRecipe(PERIDOT_SWORD.get(), PERIDOT_GEM_TAG);

        sawRecipe(GOLD_SAW.get(), Tags.Items.INGOTS_GOLD);
        sawRecipe(RUBY_SAW.get(), RUBY_GEM_TAG);
        sawRecipe(SAPPHIRE_SAW.get(), SAPPHIRE_GEM_TAG);
        sawRecipe(PERIDOT_SAW.get(), PERIDOT_GEM_TAG);

        sickleRecipe(WOOD_SICKLE.get(), Tags.Items.RODS_WOODEN);
        sickleRecipe(STONE_SICKLE.get(), Tags.Items.COBBLESTONE);
        sickleRecipe(IRON_SICKLE.get(), Tags.Items.INGOTS_IRON);
        sickleRecipe(GOLD_SICKLE.get(), Tags.Items.INGOTS_GOLD);
        sickleRecipe(DIAMOND_SICKLE.get(), Tags.Items.GEMS_DIAMOND);
        sickleRecipe(RUBY_SICKLE.get(), RUBY_GEM_TAG);
        sickleRecipe(SAPPHIRE_SICKLE.get(), SAPPHIRE_GEM_TAG);
        sickleRecipe(PERIDOT_SICKLE.get(), PERIDOT_GEM_TAG);

        // Armor

        helmetRecipe(RUBY_HELMET.get(), RUBY_GEM_TAG);
        helmetRecipe(SAPPHIRE_HELMET.get(), SAPPHIRE_GEM_TAG);
        helmetRecipe(PERIDOT_HELMET.get(), PERIDOT_GEM_TAG);

        chestplateRecipe(RUBY_CHESTPLATE.get(), RUBY_GEM_TAG);
        chestplateRecipe(SAPPHIRE_CHESTPLATE.get(), SAPPHIRE_GEM_TAG);
        chestplateRecipe(PERIDOT_CHESTPLATE.get(), PERIDOT_GEM_TAG);

        leggingsRecipe(RUBY_LEGGINGS.get(), RUBY_GEM_TAG);
        leggingsRecipe(SAPPHIRE_LEGGINGS.get(), SAPPHIRE_GEM_TAG);
        leggingsRecipe(PERIDOT_LEGGINGS.get(), PERIDOT_GEM_TAG);

        bootsRecipe(RUBY_BOOTS.get(), RUBY_GEM_TAG);
        bootsRecipe(SAPPHIRE_BOOTS.get(), SAPPHIRE_GEM_TAG);
        bootsRecipe(PERIDOT_BOOTS.get(), PERIDOT_GEM_TAG);

        // Backpacks
        for (int i = 0; i < 15; i++) {
            backpackRecipe(getBackpackByColor(i));
        }

        special(BACKPACK_DYE_RECIPE_SERIALIZER.get(), new ResourceLocation(MOD_ID, "backpack_dye"));
    }

    private void oreSmeltingRecipe(ItemLike result, Collection<ItemLike> sources, float xp) {
        String resultName = ForgeRegistries.ITEMS.getKey(result.asItem()).getPath();

        for (ItemLike source : sources) {
            String sourceName = ForgeRegistries.ITEMS.getKey(source.asItem()).getPath();

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
        shapelessRecipe(item, 9, new ResourceLocation(MOD_ID, ForgeRegistries.ITEMS.getKey(item.asItem()).getPath() + "_from_nineblock"))
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
                .key('C', WOVEN_CLOTH_ITEM.get());

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
