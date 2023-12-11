package mrtjp.projectred.expansion.data;

import codechicken.lib.datagen.recipe.RecipeProvider;
import net.minecraft.data.DataGenerator;
import net.minecraft.tags.ItemTags;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.block.Blocks;
import net.minecraftforge.common.Tags;

import static mrtjp.projectred.core.init.CoreItems.COPPER_COIL_ITEM;
import static mrtjp.projectred.core.init.CoreItems.MOTOR_ITEM;
import static mrtjp.projectred.core.init.CoreTags.*;
import static mrtjp.projectred.expansion.init.ExpansionBlocks.*;
import static mrtjp.projectred.expansion.init.ExpansionItems.*;
import static net.minecraftforge.common.Tags.Items.INGOTS_COPPER;

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
        shapedRecipe(FRAME_BLOCK.get(), 8)
                .key('S', Items.STICK)
                .key('L', ItemTags.LOGS)
                .patternLine("SLS")
                .patternLine("LSL")
                .patternLine("SLS");

        shapedRecipe(PROJECT_BENCH_BLOCK.get(), 1)
                .key('S', Tags.Items.STONE)
                .key('W', ItemTags.PLANKS)
                .key('B', Blocks.CRAFTING_TABLE)
                .key('C', Blocks.CHEST)
                .patternLine("SSS")
                .patternLine("WBW")
                .patternLine("WCW");

        shapedRecipe(BATTERY_BOX_BLOCK.get(), 1)
                .key('B', BATTERY_ITEM.get())
                .key('W', ItemTags.PLANKS)
                .key('I', Tags.Items.INGOTS_IRON)
                .key('E', ELECTROTINE_ALLOY_INGOT_TAG)
                .patternLine("BWB")
                .patternLine("BIB")
                .patternLine("IEI");

        shapedRecipe(CHARGING_BENCH_BLOCK.get(), 1)
                .key('S', Tags.Items.STONE)
                .key('C', COPPER_COIL_ITEM.get())
                .key('W', ItemTags.PLANKS)
                .key('B', BATTERY_ITEM.get())
                .key('I', Tags.Items.INGOTS_IRON)
                .key('E', ELECTROTINE_ALLOY_INGOT_TAG)
                .patternLine("SCS")
                .patternLine("WBW")
                .patternLine("IEI");

        shapedRecipe(AUTO_CRAFTER_BLOCK.get(), 1)
                .key('S', Tags.Items.STONE)
                .key('B', Blocks.CRAFTING_TABLE)
                .key('I', Tags.Items.INGOTS_IRON)
                .key('C', Blocks.CHEST)
                .key('W', ItemTags.PLANKS)
                .key('E', ELECTROTINE_ALLOY_INGOT_TAG)
                .patternLine("SBS")
                .patternLine("ICI")
                .patternLine("WEW");

        shapedRecipe(FIRE_STARTER_BLOCK.get(), 1)
                .key('N', Blocks.NETHERRACK)
                .key('F', Items.FLINT_AND_STEEL)
                .key('C', Blocks.COBBLESTONE)
                .key('R', Tags.Items.DUSTS_REDSTONE)
                .patternLine("NNN")
                .patternLine("CFC")
                .patternLine("CRC");

        shapedRecipe(FRAME_MOTOR_BLOCK.get(), 1)
                .key('W', ItemTags.PLANKS)
                .key('I', Tags.Items.INGOTS_IRON)
                .key('R', Tags.Items.DUSTS_REDSTONE)
                .key('M', MOTOR_ITEM.get())
                .key('S', Tags.Items.STONE)
                .key('E', ELECTROTINE_ALLOY_INGOT_TAG)
                .patternLine("WIW")
                .patternLine("RMR")
                .patternLine("SES");

        shapedRecipe(FRAME_ACTUATOR_BLOCK.get(), 1)
                .key('W', ItemTags.PLANKS)
                .key('I', Tags.Items.INGOTS_IRON)
                .key('C', COPPER_COIL_ITEM.get())
                .key('S', Tags.Items.STONE)
                .key('E', ELECTROTINE_ALLOY_INGOT_TAG)
                .patternLine("WIW")
                .patternLine("CIC")
                .patternLine("SES");

        // Items
        shapedRecipe(BATTERY_ITEM.get(), 1)
                .key('E', ELECTROTINE_DUST_TAG)
                .key('T', TIN_INGOT_TAG)
                .key('C', INGOTS_COPPER)
                .patternLine("ETE")
                .patternLine("ECE")
                .patternLine("ETE");

        shapelessRecipe(RECIPE_PLAN_ITEM.get(), 1)
                .addIngredient(Tags.Items.DYES_BLUE)
                .addIngredient(Items.PAPER);

        shapedRecipe(ELECTRIC_SCREWDRIVER_ITEM.get(), 1)
                .key('I', Tags.Items.INGOTS_IRON)
                .key('S', SAPPHIRE_GEM_TAG)
                .key('B', BATTERY_ITEM.get())
                .patternLine("I  ")
                .patternLine(" S ")
                .patternLine("  B");
    }
}
