package mrtjp.projectred.fabrication.data;

import codechicken.lib.datagen.recipe.RecipeProvider;
import net.minecraft.core.HolderLookup;
import net.minecraft.data.PackOutput;
import net.minecraft.tags.ItemTags;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.block.Blocks;
import net.neoforged.neoforge.common.Tags;

import java.util.concurrent.CompletableFuture;

import static mrtjp.projectred.core.init.CoreItems.PLATE_ITEM;
import static mrtjp.projectred.core.init.CoreItems.SILICON_ITEM;
import static mrtjp.projectred.core.init.CoreTags.ELECTROTINE_ALLOY_INGOT_TAG;
import static mrtjp.projectred.core.init.CoreTags.SAPPHIRE_GEM_TAG;
import static mrtjp.projectred.fabrication.ProjectRedFabrication.MOD_ID;
import static mrtjp.projectred.fabrication.init.FabricationBlocks.*;
import static mrtjp.projectred.fabrication.init.FabricationItems.*;

public class FabricationRecipeProvider extends RecipeProvider {

    public FabricationRecipeProvider(CompletableFuture<HolderLookup.Provider> registries, PackOutput output) {
        super(registries, output, MOD_ID);
    }

    @Override
    protected void registerRecipes() {

        shapedRecipe(IC_WORKBENCH_BLOCK.get())
                .key('s', Blocks.STONE)
                .key('w', ItemTags.PLANKS)
                .key('b', IC_BLUEPRINT_ITEM.get())
                .patternLine("sss")
                .patternLine("wbw")
                .patternLine("www");

        shapedRecipe(PLOTTING_TABLE_BLOCK.get())
                .key('g', Blocks.GLASS)
                .key('t', SAPPHIRE_GEM_TAG)
                .key('s', Blocks.STONE)
                .key('p', BLANK_PHOTOMASK_ITEM.get())
                .key('w', ItemTags.PLANKS)
                .key('b', ELECTROTINE_ALLOY_INGOT_TAG)
                .patternLine("gtg")
                .patternLine("sps")
                .patternLine("wbw");

        shapedRecipe(LITHOGRAPHY_TABLE_BLOCK.get())
                .key('g', Blocks.GLASS)
                .key('t', Tags.Items.GEMS_EMERALD)
                .key('o', Tags.Items.OBSIDIANS)
                .key('i', Tags.Items.INGOTS_IRON)
                .key('w', ItemTags.PLANKS)
                .key('b', ELECTROTINE_ALLOY_INGOT_TAG)
                .patternLine("gtg")
                .patternLine("oio")
                .patternLine("wbw");

        shapedRecipe(PACKAGING_TABLE_BLOCK.get())
                .key('g', Blocks.GLASS)
                .key('t', Tags.Items.DUSTS_REDSTONE)
                .key('p', PLATE_ITEM.get())
                .key('w', ItemTags.PLANKS)
                .key('b', ELECTROTINE_ALLOY_INGOT_TAG)
                .patternLine("gtg")
                .patternLine("ppp")
                .patternLine("wbw");

        shapedRecipe(IC_BLUEPRINT_ITEM.get())
                .key('p', Items.PAPER)
                .key('b', Tags.Items.DYES_BLUE)
                .key('r', Tags.Items.DUSTS_REDSTONE)
                .patternLine("pbp")
                .patternLine("brb")
                .patternLine("pbp");

        shapedRecipe(BLANK_PHOTOMASK_ITEM.get())
                .key('g', Tags.Items.GLASS_PANES)
                .key('q', Tags.Items.GEMS_QUARTZ)
                .patternLine("ggg")
                .patternLine("gqg")
                .patternLine("ggg");

        smelting(ROUGH_SILICON_WAFER_ITEM.get())
                .ingredient(SILICON_ITEM.get());
    }
}
