package mrtjp.projectred.illumination.data;

import codechicken.lib.datagen.recipe.RecipeProvider;
import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.MultipartLightType;
import net.minecraft.data.DataGenerator;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.block.Blocks;
import net.minecraftforge.common.Tags;

import static mrtjp.projectred.core.init.CoreItems.*;

public class IlluminationRecipeProvider extends RecipeProvider {

    public IlluminationRecipeProvider(DataGenerator generatorIn) {
        super(generatorIn);
    }

    @Override
    public String getName() {
        return "ProjectRed-Illumination Recipes";
    }

    @Override
    protected void registerRecipes() {

        //Lamps
        for (int c = 0; c < 16; c++) {
            shapedRecipe(BlockLightType.ILLUMAR_LAMP.getBlock(c, false), 1)
                    .key('G', Tags.Items.GLASS_PANES_COLORLESS)
                    .key('I', getIllumarByIndex(c))
                    .key('R', Tags.Items.DUSTS_REDSTONE)
                    .patternLine("GIG")
                    .patternLine("GIG")
                    .patternLine("GRG");

            shapedRecipe(BlockLightType.ILLUMAR_LAMP.getBlock(c, true), 1)
                    .key('G', Tags.Items.GLASS_PANES_COLORLESS)
                    .key('I', getIllumarByIndex(c))
                    .key('R', Items.REDSTONE_TORCH)
                    .patternLine("GIG")
                    .patternLine("GIG")
                    .patternLine("GRG");
        }

        //Lanterns
        for (int c = 0; c < 16; c++) {
            shapedRecipe(MultipartLightType.LANTERN.getItem(c, false), 1)
                    .key('P', PLATE_ITEM.get())
                    .key('N', Tags.Items.NUGGETS_GOLD)
                    .key('G', Tags.Items.GLASS_PANES_COLORLESS)
                    .key('I', getIllumarByIndex(c))
                    .key('R', Tags.Items.DUSTS_REDSTONE)
                    .patternLine("PNP")
                    .patternLine("GIG")
                    .patternLine("PRP");

            shapedRecipe(MultipartLightType.LANTERN.getItem(c, true), 1)
                    .key('P', PLATE_ITEM.get())
                    .key('N', Tags.Items.NUGGETS_GOLD)
                    .key('G', Tags.Items.GLASS_PANES_COLORLESS)
                    .key('I', getIllumarByIndex(c))
                    .key('R', Items.REDSTONE_TORCH)
                    .patternLine("PNP")
                    .patternLine("GIG")
                    .patternLine("PRP");
        }

        //Fallout lights
        for (int c = 0; c < 16; c++) {
            shapedRecipe(MultipartLightType.FALLOUT.getItem(c, false), 1)
                    .key('C', Blocks.IRON_BARS)
                    .key('I', getIllumarByIndex(c))
                    .key('N', Tags.Items.NUGGETS_GOLD)
                    .key('P', CONDUCTIVE_PLATE_ITEM.get())
                    .patternLine("CCC")
                    .patternLine("CIC")
                    .patternLine("NPN");

            shapedRecipe(MultipartLightType.FALLOUT.getItem(c, true), 1)
                    .key('C', Blocks.IRON_BARS)
                    .key('I', getIllumarByIndex(c))
                    .key('N', Tags.Items.NUGGETS_GOLD)
                    .key('P', CATHODE_ITEM.get())
                    .patternLine("CCC")
                    .patternLine("CIC")
                    .patternLine("NPN");
        }

        //Cage lights
        for (int c = 0; c < 16; c++) {
            shapedRecipe(MultipartLightType.CAGE.getItem(c, false), 1)
                    .key('C', Blocks.IRON_BARS)
                    .key('I', getIllumarByIndex(c))
                    .key('N', PLATE_ITEM.get())
                    .key('P', CONDUCTIVE_PLATE_ITEM.get())
                    .patternLine(" C ")
                    .patternLine("CIC")
                    .patternLine("NPN");

            shapedRecipe(MultipartLightType.CAGE.getItem(c, true), 1)
                    .key('C', Blocks.IRON_BARS)
                    .key('I', getIllumarByIndex(c))
                    .key('N', PLATE_ITEM.get())
                    .key('P', CATHODE_ITEM.get())
                    .patternLine(" C ")
                    .patternLine("CIC")
                    .patternLine("NPN");
        }

        //Fixture lights
        for (int c = 0; c < 16; c++) {
            shapedRecipe(MultipartLightType.FIXTURE.getItem(c, false), 1)
                    .key('G', Tags.Items.GLASS_PANES_COLORLESS)
                    .key('I', getIllumarByIndex(c))
                    .key('P', PLATE_ITEM.get())
                    .key('C', CONDUCTIVE_PLATE_ITEM.get())
                    .patternLine("GGG")
                    .patternLine("GIG")
                    .patternLine("PCP");

            shapedRecipe(MultipartLightType.FIXTURE.getItem(c, true), 1)
                    .key('G', Tags.Items.GLASS_PANES_COLORLESS)
                    .key('I', getIllumarByIndex(c))
                    .key('P', PLATE_ITEM.get())
                    .key('C', CATHODE_ITEM.get())
                    .patternLine("GGG")
                    .patternLine("GIG")
                    .patternLine("PCP");
        }
    }
}
