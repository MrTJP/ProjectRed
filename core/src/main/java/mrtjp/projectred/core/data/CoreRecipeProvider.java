package mrtjp.projectred.core.data;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.datagen.recipe.RecipeProvider;
import codechicken.microblock.api.BlockMicroMaterial;
import codechicken.microblock.init.CBMicroblockModContent;
import codechicken.microblock.item.ItemMicroBlock;
import codechicken.microblock.util.MicroMaterialRegistry;
import mrtjp.projectred.core.ProjectRedCore;
import net.minecraft.core.HolderLookup;
import net.minecraft.data.PackOutput;
import net.minecraft.tags.ItemTags;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.block.Blocks;
import net.neoforged.neoforge.common.crafting.DataComponentIngredient;

import java.util.Objects;
import java.util.concurrent.CompletableFuture;

import static mrtjp.projectred.core.init.CoreBlocks.ELECTROTINE_GENERATOR_BLOCK;
import static mrtjp.projectred.core.init.CoreItems.*;
import static mrtjp.projectred.core.init.CoreTags.*;
import static net.neoforged.neoforge.common.Tags.Items.*;

public class CoreRecipeProvider extends RecipeProvider {

    public CoreRecipeProvider(CompletableFuture<HolderLookup.Provider> registries, PackOutput output) {
        super(registries, output, ProjectRedCore.MOD_ID);
    }

    @Override
    protected void registerRecipes() {

        shapedRecipe(ELECTROTINE_GENERATOR_BLOCK.get())
                .patternLine("bbb")
                .patternLine("bdb")
                .patternLine("cec")
                .key('b', Blocks.BRICKS)
                .key('d', ELECTROTINE_DUST_TAG)
                .key('c', Blocks.CLAY)
                .key('e', ELECTROTINE_ALLOY_INGOT_TAG);

        smelting(PLATE_ITEM.get(), 2)
                .ingredient(Blocks.SMOOTH_STONE);

        smelting(SILICON_BOULE_ITEM.get())
                .ingredient(SAND_COAL_COMP_ITEM.get());

        smelting(INFUSED_SILICON_ITEM.get())
                .ingredient(RED_SILICON_COMP_ITEM.get());

        smelting(ENERGIZED_SILICON_ITEM.get())
                .ingredient(GLOW_SILICON_COMP_ITEM.get());

        smelting(RED_ALLOY_INGOT_ITEM.get())
                .ingredient(RED_IRON_COMP_ITEM.get());

        smelting(ELECTROTINE_ALLOY_INGOT_ITEM.get())
                .ingredient(ELECTROTINE_IRON_COMP_ITEM.get());

        smelting(ELECTROTINE_SILICON_ITEM.get())
                .ingredient(ELECTROTINE_SILICON_COMP_ITEM.get());

        shapedRecipe(CONDUCTIVE_PLATE_ITEM.get())
                .key('R', DUSTS_REDSTONE)
                .key('P', PLATE_ITEM.get())
                .patternLine("R")
                .patternLine("P");

        shapedRecipe(PLATFORMED_PLATE_ITEM.get())
                .key('R', CONDUCTIVE_PLATE_ITEM.get())
                .key('S', RODS_WOODEN)
                .key('P', PLATE_ITEM.get())
                .patternLine(" R ")
                .patternLine("SPS")
                .patternLine("PRP");

        shapedRecipe(ANODE_ITEM.get(), 3)
                .key('R', DUSTS_REDSTONE)
                .key('P', PLATE_ITEM.get())
                .patternLine(" R ")
                .patternLine("RRR")
                .patternLine("PPP");

        shapedRecipe(CATHODE_ITEM.get())
                .key('T', Items.REDSTONE_TORCH)
                .key('P', PLATE_ITEM.get())
                .patternLine("T")
                .patternLine("P");

        shapedRecipe(POINTER_ITEM.get())
                .key('S', STONES)
                .key('T', Items.REDSTONE_TORCH)
                .key('P', PLATE_ITEM.get())
                .patternLine("S")
                .patternLine("T")
                .patternLine("P");

        shapedRecipe(SILICON_CHIP_ITEM.get())
                .key('S', INFUSED_SILICON_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .patternLine(" S ")
                .patternLine("PPP");

        shapedRecipe(ENERGIZED_SILICON_CHIP_ITEM.get())
                .key('E', ENERGIZED_SILICON_ITEM.get())
                .key('P', PLATE_ITEM.get())
                .patternLine(" E ")
                .patternLine("PPP");

        shapedRecipe(SAND_COAL_COMP_ITEM.get())
                .key('S', SANDS)
                .key('C', STORAGE_BLOCKS_COAL)
                .patternLine("SSS")
                .patternLine("SCS")
                .patternLine("SSS");

        shapedRecipe(RED_IRON_COMP_ITEM.get())
                .key('R', DUSTS_REDSTONE)
                .key('I', INGOTS_IRON)
                .patternLine("RRR")
                .patternLine("RIR")
                .patternLine("RRR");

        shapedRecipe(ELECTROTINE_IRON_COMP_ITEM.get())
                .key('B', ELECTROTINE_DUST_TAG)
                .key('I', INGOTS_IRON)
                .patternLine("BBB")
                .patternLine("BIB")
                .patternLine("BBB");

        shapedRecipe(SILICON_ITEM.get(), 8)
                .key('S', CBMicroblockModContent.DIAMOND_SAW.get())
                .key('B', SILICON_BOULE_ITEM.get())
                .patternLine("S")
                .patternLine("B");

        shapedRecipe(RED_SILICON_COMP_ITEM.get())
                .key('R', DUSTS_REDSTONE)
                .key('S', SILICON_ITEM.get())
                .patternLine("RRR")
                .patternLine("RSR")
                .patternLine("RRR");

        shapedRecipe(GLOW_SILICON_COMP_ITEM.get())
                .key('G', DUSTS_GLOWSTONE)
                .key('S', SILICON_ITEM.get())
                .patternLine("GGG")
                .patternLine("GSG")
                .patternLine("GGG");

        shapedRecipe(ELECTROTINE_SILICON_COMP_ITEM.get())
                .key('E', ELECTROTINE_DUST_TAG)
                .key('S', SILICON_ITEM.get())
                .patternLine("EEE")
                .patternLine("ESE")
                .patternLine("EEE");

        shapedRecipe(COPPER_COIL_ITEM.get())
                .key('C', INGOTS_COPPER)
                .key('D', DRAW_PLATE_ITEM.get())
                .patternLine("CD");

        shapedRecipe(IRON_COIL_ITEM.get())
                .key('I', INGOTS_IRON)
                .key('D', DRAW_PLATE_ITEM.get())
                .patternLine("ID");

        shapedRecipe(GOLD_COIL_ITEM.get())
                .key('G', INGOTS_GOLD)
                .key('D', DRAW_PLATE_ITEM.get())
                .patternLine("GD");

        shapedRecipe(MOTOR_ITEM.get())
                .key('I', INGOTS_IRON)
                .key('S', STONES)
                .key('C', COPPER_COIL_ITEM.get())
                .key('R', DUSTS_REDSTONE)
                .patternLine(" I ")
                .patternLine("SCS")
                .patternLine("RCR");

        shapedRecipe(WOVEN_CLOTH_ITEM.get())
                .key('S', STRINGS)
                .key('W', RODS_WOODEN)
                .patternLine("SSS")
                .patternLine("SWS")
                .patternLine("SSS");

        shapedRecipe(SAIL_ITEM.get())
                .key('S', WOVEN_CLOTH_ITEM.get())
                .patternLine("SS")
                .patternLine("SS")
                .patternLine("SS");

        for (int i = 0; i < 16; i++) {
            addIllumarRecipe(getIllumarByIndex(i), EnumColour.values()[i]);
        }

        shapedRecipe(DRAW_PLATE_ITEM.get())
                .key('I', DataComponentIngredient.of(true, ItemMicroBlock.create(3, 2, Objects.requireNonNull(MicroMaterialRegistry.getMaterial(BlockMicroMaterial.makeMaterialKey(Blocks.IRON_BLOCK.defaultBlockState()))))))
                .key('D', DataComponentIngredient.of(true, ItemMicroBlock.create(0, 2, Objects.requireNonNull(MicroMaterialRegistry.getMaterial(BlockMicroMaterial.makeMaterialKey(Blocks.DIAMOND_BLOCK.defaultBlockState()))))))
                .patternLine(" I ")
                .patternLine("IDI")
                .patternLine(" I ");

        shapedRecipe(SCREWDRIVER_ITEM.get())
                .key('I', INGOTS_IRON)
                .key('B', DYES_BLUE)
                .patternLine("I  ")
                .patternLine(" IB")
                .patternLine(" BI");

        shapedRecipe(MULTIMETER_ITEM.get())
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

}
