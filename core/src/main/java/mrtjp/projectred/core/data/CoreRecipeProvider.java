package mrtjp.projectred.core.data;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.datagen.recipe.RecipeProvider;
import codechicken.microblock.api.BlockMicroMaterial;
import codechicken.microblock.init.CBMicroblockModContent;
import codechicken.microblock.item.ItemMicroBlock;
import codechicken.microblock.util.MicroMaterialRegistry;
import net.minecraft.data.DataGenerator;
import net.minecraft.tags.ItemTags;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.block.Blocks;
import net.minecraftforge.common.crafting.NBTIngredient;

import static mrtjp.projectred.core.init.CoreReferences.*;
import static mrtjp.projectred.core.init.CoreTags.*;
import static net.minecraftforge.common.Tags.Items.*;

public class CoreRecipeProvider extends RecipeProvider {

    private static final Item[] ILLUMAR_ITEMS = {
            WHITE_ILLUMAR_ITEM,
            ORANGE_ILLUMAR_ITEM,
            MAGENTA_ILLUMAR_ITEM,
            LIGHT_BLUE_ILLUMAR_ITEM,
            YELLOW_ILLUMAR_ITEM,
            LIME_ILLUMAR_ITEM,
            PINK_ILLUMAR_ITEM,
            GRAY_ILLUMAR_ITEM,
            LIGHT_GRAY_ILLUMAR_ITEM,
            CYAN_ILLUMAR_ITEM,
            PURPLE_ILLUMAR_ITEM,
            BLUE_ILLUMAR_ITEM,
            BROWN_ILLUMAR_ITEM,
            GREEN_ILLUMAR_ITEM,
            RED_ILLUMAR_ITEM,
            BLACK_ILLUMAR_ITEM,
    };

    public CoreRecipeProvider(DataGenerator gen) {
        super(gen);
    }

    @Override
    public String getName() {
        return "ProjectRed-Core Recipes";
    }

    @Override
    protected void registerRecipes() {

        shapedRecipe(ELECTROTINE_GENERATOR_BLOCK)
                .patternLine("bbb")
                .patternLine("bdb")
                .patternLine("cec")
                .key('b', Blocks.BRICKS)
                .key('d', ELECTROTINE_DUST_TAG)
                .key('c', Blocks.CLAY)
                .key('e', ELECTROTINE_ALLOY_INGOT_TAG);

        smelting(PLATE_ITEM, 2)
                .ingredient(Blocks.SMOOTH_STONE);

        smelting(SILICON_BOULE_ITEM)
                .ingredient(SAND_COAL_COMP_ITEM);

        smelting(INFUSED_SILICON_ITEM)
                .ingredient(RED_SILICON_COMP_ITEM);

        smelting(ENERGIZED_SILICON_ITEM)
                .ingredient(GLOW_SILICON_COMP_ITEM);

        smelting(RED_ALLOY_INGOT_ITEM)
                .ingredient(RED_IRON_COMP_ITEM);

        smelting(ELECTROTINE_ALLOY_INGOT_ITEM)
                .ingredient(ELECTROTINE_IRON_COMP_ITEM);

        smelting(ELECTROTINE_SILICON_ITEM)
                .ingredient(ELECTROTINE_SILICON_COMP_ITEM);

        shapedRecipe(CONDUCTIVE_PLATE_ITEM)
                .key('R', DUSTS_REDSTONE)
                .key('P', PLATE_ITEM)
                .patternLine("R")
                .patternLine("P");

        shapedRecipe(PLATFORMED_PLATE_ITEM)
                .key('R', CONDUCTIVE_PLATE_ITEM)
                .key('S', RODS_WOODEN)
                .key('P', PLATE_ITEM)
                .patternLine(" R ")
                .patternLine("SPS")
                .patternLine("PRP");

        shapedRecipe(ANODE_ITEM, 3)
                .key('R', DUSTS_REDSTONE)
                .key('P', PLATE_ITEM)
                .patternLine(" R ")
                .patternLine("RRR")
                .patternLine("PPP");

        shapedRecipe(CATHODE_ITEM)
                .key('T', Items.REDSTONE_TORCH)
                .key('P', PLATE_ITEM)
                .patternLine("T")
                .patternLine("P");

        shapedRecipe(POINTER_ITEM)
                .key('S', STONE)
                .key('T', Items.REDSTONE_TORCH)
                .key('P', PLATE_ITEM)
                .patternLine("S")
                .patternLine("T")
                .patternLine("P");

        shapedRecipe(SILICON_CHIP_ITEM)
                .key('S', INFUSED_SILICON_ITEM)
                .key('P', PLATE_ITEM)
                .patternLine(" S ")
                .patternLine("PPP");

        shapedRecipe(ENERGIZED_SILICON_CHIP_ITEM)
                .key('E', ENERGIZED_SILICON_ITEM)
                .key('P', PLATE_ITEM)
                .patternLine(" E ")
                .patternLine("PPP");

        shapedRecipe(SAND_COAL_COMP_ITEM)
                .key('S', SAND)
                .key('C', STORAGE_BLOCKS_COAL)
                .patternLine("SSS")
                .patternLine("SCS")
                .patternLine("SSS");

        shapedRecipe(RED_IRON_COMP_ITEM)
                .key('R', DUSTS_REDSTONE)
                .key('I', INGOTS_IRON)
                .patternLine("RRR")
                .patternLine("RIR")
                .patternLine("RRR");

        shapedRecipe(ELECTROTINE_IRON_COMP_ITEM)
                .key('B', ELECTROTINE_DUST_TAG)
                .key('I', INGOTS_IRON)
                .patternLine("BBB")
                .patternLine("BIB")
                .patternLine("BBB");

        shapedRecipe(SILICON_ITEM, 8)
                .key('S', CBMicroblockModContent.DIAMOND_SAW.get())
                .key('B', SILICON_BOULE_ITEM)
                .patternLine("S")
                .patternLine("B");

        shapedRecipe(RED_SILICON_COMP_ITEM)
                .key('R', DUSTS_REDSTONE)
                .key('S', SILICON_ITEM)
                .patternLine("RRR")
                .patternLine("RSR")
                .patternLine("RRR");

        shapedRecipe(GLOW_SILICON_COMP_ITEM)
                .key('G', DUSTS_GLOWSTONE)
                .key('S', SILICON_ITEM)
                .patternLine("GGG")
                .patternLine("GSG")
                .patternLine("GGG");

        shapedRecipe(ELECTROTINE_SILICON_COMP_ITEM)
                .key('E', ELECTROTINE_DUST_TAG)
                .key('S', SILICON_ITEM)
                .patternLine("EEE")
                .patternLine("ESE")
                .patternLine("EEE");

        shapedRecipe(COPPER_COIL_ITEM)
                .key('C', COPPER_INGOT_TAG)
                .key('D', DRAW_PLATE_ITEM)
                .patternLine("CD");

        shapedRecipe(IRON_COIL_ITEM)
                .key('I', INGOTS_IRON)
                .key('D', DRAW_PLATE_ITEM)
                .patternLine("ID");

        shapedRecipe(GOLD_COIL_ITEM)
                .key('G', INGOTS_GOLD)
                .key('D', DRAW_PLATE_ITEM)
                .patternLine("GD");

        shapedRecipe(MOTOR_ITEM)
                .key('I', INGOTS_IRON)
                .key('S', STONE)
                .key('C', COPPER_COIL_ITEM)
                .key('R', DUSTS_REDSTONE)
                .patternLine(" I ")
                .patternLine("SCS")
                .patternLine("RCR");

        shapedRecipe(WOVEN_CLOTH_ITEM)
                .key('S', STRING)
                .key('W', RODS_WOODEN)
                .patternLine("SSS")
                .patternLine("SWS")
                .patternLine("SSS");

        shapedRecipe(SAIL_ITEM)
                .key('S', WOVEN_CLOTH_ITEM)
                .patternLine("SS")
                .patternLine("SS")
                .patternLine("SS");

        for (int i = 0; i < 16; i++) {
            addIllumarRecipe(ILLUMAR_ITEMS[i], EnumColour.values()[i]);
        }

        shapedRecipe(DRAW_PLATE_ITEM)
                .key('I', new PublicNBTIngredient(ItemMicroBlock.create(3, 2, MicroMaterialRegistry.getMaterial(BlockMicroMaterial.makeMaterialKey(Blocks.IRON_BLOCK.defaultBlockState())))))
                .key('D', new PublicNBTIngredient(ItemMicroBlock.create(0, 2, MicroMaterialRegistry.getMaterial(BlockMicroMaterial.makeMaterialKey(Blocks.DIAMOND_BLOCK.defaultBlockState())))))
                .patternLine(" I ")
                .patternLine("IDI")
                .patternLine(" I ");

        shapedRecipe(SCREWDRIVER_ITEM)
                .key('I', INGOTS_IRON)
                .key('B', DYES_BLUE)
                .patternLine("I  ")
                .patternLine(" IB")
                .patternLine(" BI");

        shapedRecipe(MULTIMETER_ITEM)
                .key('A', RED_ALLOY_INGOT_TAG)
                .key('B', DYES_BLACK)
                .key('E', DYES_GREEN)
                .key('R', DYES_RED)
                .key('G', DUSTS_GLOWSTONE)
                .patternLine("A A")
                .patternLine("BER")
                .patternLine("BGR");
    }

    private void addIllumarRecipe(Item illumarItem, EnumColour color) {
        shapelessRecipe(illumarItem)
                .addIngredient(DUSTS_GLOWSTONE, 2)
                .addIngredient(ItemTags.create(color.getDyeTagName()), 2);
    }

    // Forge PLS :(
    private static class PublicNBTIngredient extends NBTIngredient {
        public PublicNBTIngredient(ItemStack stack) {
            super(stack);
        }
    }

}
