package mrtjp.projectred.expansion.data;

import codechicken.lib.datagen.recipe.RecipeProvider;
import net.minecraft.block.Blocks;
import net.minecraft.data.DataGenerator;
import net.minecraft.item.Items;
import net.minecraft.tags.ItemTags;
import net.minecraftforge.common.Tags;

import static mrtjp.projectred.core.CoreContent.*;
import static mrtjp.projectred.expansion.init.ExpansionReferences.*;

public class ExpansionRecipeProvider extends RecipeProvider {

    public ExpansionRecipeProvider(DataGenerator generatorIn) {
        super(generatorIn);
    }

    @Override
    public String getName() {
        return "ProjectRed-Expansion Recipes";
    }

    @Override
    protected void registerRecipes() {

        //Blocks
        shapedRecipe(PROJECT_BENCH_BLOCK, 1)
                .key('S', Tags.Items.STONE)
                .key('W', ItemTags.PLANKS)
                .key('B', Blocks.CRAFTING_TABLE)
                .key('C', Blocks.CHEST)
                .patternLine("SSS")
                .patternLine("WBW")
                .patternLine("WCW");

        shapedRecipe(BATTERY_BOX_BLOCK, 1)
                .key('B', BATTERY_ITEM)
                .key('W', ItemTags.PLANKS)
                .key('I', Tags.Items.INGOTS_IRON)
                .key('E', tagIngotsElectrotineAlloy())
                .patternLine("BWB")
                .patternLine("BIB")
                .patternLine("IEI");

        shapedRecipe(CHARGING_BENCH_BLOCK, 1)
                .key('S', Tags.Items.STONE)
                .key('C', itemCopperCoil().get())
                .key('W', ItemTags.PLANKS)
                .key('B', BATTERY_ITEM)
                .key('I', Tags.Items.INGOTS_IRON)
                .key('E', tagIngotsElectrotineAlloy())
                .patternLine("SCS")
                .patternLine("WBW")
                .patternLine("IEI");

        shapedRecipe(AUTO_CRAFTER_BLOCK, 1)
                .key('S', Tags.Items.STONE)
                .key('B', Blocks.CRAFTING_TABLE)
                .key('I', Tags.Items.INGOTS_IRON)
                .key('C', Blocks.CHEST)
                .key('W', ItemTags.PLANKS)
                .key('E', tagIngotsElectrotineAlloy())
                .patternLine("SBS")
                .patternLine("ICI")
                .patternLine("WEW");

        shapedRecipe(FIRE_STARTER_BLOCK, 1)
                .key('N', Blocks.NETHERRACK)
                .key('F', Items.FLINT_AND_STEEL)
                .key('C', Blocks.COBBLESTONE)
                .key('R', Tags.Items.DUSTS_REDSTONE)
                .patternLine("NNN")
                .patternLine("CFC")
                .patternLine("CRC");

        // Items
        shapedRecipe(BATTERY_ITEM, 1)
                .key('E', tagDustsElectrotine())
                .key('T', tagIngotsTin())
                .key('C', tagIngotsCopper())
                .patternLine("ETE")
                .patternLine("ECE")
                .patternLine("ETE");

        shapelessRecipe(RECIPE_PLAN_ITEM, 1)
                .addIngredient(Tags.Items.DYES_BLUE)
                .addIngredient(Items.PAPER);

        shapedRecipe(ELECTRIC_SCREWDRIVER_ITEM, 1)
                .key('I', Tags.Items.INGOTS_IRON)
                .key('S', tagGemsSapphire())
                .key('B', BATTERY_ITEM)
                .patternLine("I  ")
                .patternLine(" S ")
                .patternLine("  B");
    }
}
