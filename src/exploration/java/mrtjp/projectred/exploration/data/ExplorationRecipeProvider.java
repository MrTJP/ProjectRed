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

import static mrtjp.projectred.ProjectRedExploration.MOD_ID;
import static mrtjp.projectred.core.CoreContent.*;
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

        smelting(itemRuby().get(), 1, new ResourceLocation(MOD_ID, "ruby_from_ore")) //Note: Special names for json bc multiple recipes here producing same item
                .ingredient(RUBY_ORE_BLOCK)
                .experience(1F);

        smelting(itemSapphire().get(), 1, new ResourceLocation(MOD_ID, "sapphire_from_ore"))
                .ingredient(SAPPHIRE_ORE_BLOCK)
                .experience(1F);

        smelting(itemPeridot().get(), 1, new ResourceLocation(MOD_ID, "peridot_from_ore"))
                .ingredient(PERIDOT_ORE_BLOCK)
                .experience(1F);

        smelting(itemCopperIngot().get(), 1, new ResourceLocation(MOD_ID, "copper_from_ore"))
                .ingredient(COPPER_ORE_BLOCK)
                .experience(0.7F);

        smelting(itemTinIngot().get(), 1, new ResourceLocation(MOD_ID, "tin_from_ore"))
                .ingredient(TIN_ORE_BLOCK)
                .experience(0.7F);

        smelting(itemSilverIngot().get(), 1, new ResourceLocation(MOD_ID, "silver_from_ore"))
                .ingredient(SILVER_ORE_BLOCK)
                .experience(0.7F);

        smelting(itemElectrotineDust().get(), 1, new ResourceLocation(MOD_ID, "electrotine_from_ore"))
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
                .key('S', tagGemsRuby())
                .patternLine("SSS")
                .patternLine("SSS")
                .patternLine("SSS");

        shapelessRecipe(itemRuby().get(), 9, new ResourceLocation(MOD_ID, "ruby_from_block"))
                .addIngredient(RUBY_STORAGE_BLOCK_ITEM_TAG);

        shapedRecipe(SAPPHIRE_BLOCK)
                .key('S', tagGemsSapphire())
                .patternLine("SSS")
                .patternLine("SSS")
                .patternLine("SSS");

        shapelessRecipe(itemSapphire().get(), 9, new ResourceLocation(MOD_ID, "sapphire_from_block"))
                .addIngredient(SAPPHIRE_STORAGE_BLOCK_ITEM_TAG);

        shapedRecipe(PERIDOT_BLOCK)
                .key('S', tagGemsPeridot())
                .patternLine("SSS")
                .patternLine("SSS")
                .patternLine("SSS");

        shapelessRecipe(itemPeridot().get(), 9, new ResourceLocation(MOD_ID, "peridot_from_block"))
                .addIngredient(tagGemsPeridot());

        shapedRecipe(COPPER_BLOCK)
                .key('S', tagIngotsCopper())
                .patternLine("SSS")
                .patternLine("SSS")
                .patternLine("SSS");

        shapelessRecipe(itemCopperIngot().get(), 9, new ResourceLocation(MOD_ID, "copper_from_block"))
                .addIngredient(COPPER_STORAGE_BLOCK_ITEM_TAG);

        shapedRecipe(TIN_BLOCK)
                .key('S', tagIngotsTin())
                .patternLine("SSS")
                .patternLine("SSS")
                .patternLine("SSS");

        shapelessRecipe(itemTinIngot().get(), 9, new ResourceLocation(MOD_ID, "tin_from_block"))
                .addIngredient(TIN_ORE_BLOCK_ITEM_TAG);

        shapedRecipe(SILVER_BLOCK)
                .key('S', tagIngotsSilver())
                .patternLine("SSS")
                .patternLine("SSS")
                .patternLine("SSS");

        shapelessRecipe(itemSilverIngot().get(), 9, new ResourceLocation(MOD_ID, "silver_from_block"))
                .addIngredient(SILVER_STORAGE_BLOCK_ITEM_TAG);

        shapedRecipe(ELECTROTINE_BLOCK)
                .key('S', tagDustsElectrotine())
                .patternLine("SSS")
                .patternLine("SSS")
                .patternLine("SSS");

        shapelessRecipe(itemElectrotineDust().get(), 9, new ResourceLocation(MOD_ID, "electrotine_from_block"))
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
                .key('I', itemIronCoil().get())
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
                .key('S', tagIngotsSilver())
                .patternLine("S")
                .patternLine("W");

        axeRecipe(RUBY_AXE, tagGemsRuby());
        axeRecipe(SAPPHIRE_AXE, tagGemsSapphire());
        axeRecipe(PERIDOT_AXE, tagGemsPeridot());

        pickaxeRecipe(RUBY_PICKAXE, tagGemsRuby());
        pickaxeRecipe(SAPPHIRE_PICKAXE, tagGemsSapphire());
        pickaxeRecipe(PERIDOT_PICKAXE, tagGemsPeridot());

        shovelRecipe(RUBY_SHOVEL, tagGemsRuby());
        shovelRecipe(SAPPHIRE_SHOVEL, tagGemsSapphire());
        shovelRecipe(PERIDOT_SHOVEL, tagGemsPeridot());

        hoeRecipe(RUBY_HOE, tagGemsRuby());
        hoeRecipe(SAPPHIRE_HOE, tagGemsSapphire());
        hoeRecipe(PERIDOT_HOE, tagGemsPeridot());

        swordRecipe(RUBY_SWORD, tagGemsRuby());
        swordRecipe(SAPPHIRE_SWORD, tagGemsSapphire());
        swordRecipe(PERIDOT_SWORD, tagGemsPeridot());

        sawRecipe(GOLD_SAW, Tags.Items.INGOTS_GOLD);
        sawRecipe(RUBY_SAW, tagGemsRuby());
        sawRecipe(SAPPHIRE_SAW, tagGemsSapphire());
        sawRecipe(PERIDOT_SAW, tagGemsPeridot());

        sickleRecipe(WOOD_SICKLE, Tags.Items.RODS_WOODEN);
        sickleRecipe(STONE_SICKLE, Tags.Items.COBBLESTONE);
        sickleRecipe(IRON_SICKLE, Tags.Items.INGOTS_IRON);
        sickleRecipe(GOLD_SICKLE, Tags.Items.INGOTS_GOLD);
        sickleRecipe(DIAMOND_SICKLE, Tags.Items.GEMS_DIAMOND);
        sickleRecipe(RUBY_SICKLE, tagGemsRuby());
        sickleRecipe(SAPPHIRE_SICKLE, tagGemsSapphire());
        sickleRecipe(PERIDOT_SICKLE, tagGemsPeridot());

        // Armor

        helmetRecipe(RUBY_HELMET, tagGemsRuby());
        helmetRecipe(SAPPHIRE_HELMET, tagGemsSapphire());
        helmetRecipe(PERIDOT_HELMET, tagGemsPeridot());

        chestplateRecipe(RUBY_CHESTPLATE, tagGemsRuby());
        chestplateRecipe(SAPPHIRE_CHESTPLATE, tagGemsSapphire());
        chestplateRecipe(PERIDOT_CHESTPLATE, tagGemsPeridot());

        leggingsRecipe(RUBY_LEGGINGS, tagGemsRuby());
        leggingsRecipe(SAPPHIRE_LEGGINGS, tagGemsSapphire());
        leggingsRecipe(PERIDOT_LEGGINGS, tagGemsPeridot());

        bootsRecipe(RUBY_BOOTS, tagGemsRuby());
        bootsRecipe(SAPPHIRE_BOOTS, tagGemsSapphire());
        bootsRecipe(PERIDOT_BOOTS, tagGemsPeridot());

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
                .key('C', itemWovenCloth().get());

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
