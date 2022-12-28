package mrtjp.projectred.illumination.data;

import codechicken.lib.datagen.recipe.RecipeProvider;
import mrtjp.projectred.core.CoreContent;
import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.MultipartLightType;
import net.minecraft.block.Blocks;
import net.minecraft.data.DataGenerator;
import net.minecraft.item.Items;
import net.minecraftforge.common.Tags;

import static mrtjp.projectred.core.CoreContent.*;

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
                    .key('I', illumars().apply(c))
                    .key('R', Tags.Items.DUSTS_REDSTONE)
                    .patternLine("GIG")
                    .patternLine("GIG")
                    .patternLine("GRG");

            shapedRecipe(BlockLightType.ILLUMAR_LAMP.getBlock(c, true), 1)
                    .key('G', Tags.Items.GLASS_PANES_COLORLESS)
                    .key('I', illumars().apply(c))
                    .key('R', Items.REDSTONE_TORCH)
                    .patternLine("GIG")
                    .patternLine("GIG")
                    .patternLine("GRG");
        }

        //Lanterns
        for (int c = 0; c < 16; c++) {
            shapedRecipe(MultipartLightType.LANTERN.getItem(c, false), 1)
                    .key('P', itemPlate().get())
                    .key('N', Tags.Items.NUGGETS_GOLD)
                    .key('G', Tags.Items.GLASS_PANES_COLORLESS)
                    .key('I', illumars().apply(c))
                    .key('R', Tags.Items.DUSTS_REDSTONE)
                    .patternLine("PNP")
                    .patternLine("GIG")
                    .patternLine("PRP");

            shapedRecipe(MultipartLightType.LANTERN.getItem(c, true), 1)
                    .key('P', itemPlate().get())
                    .key('N', Tags.Items.NUGGETS_GOLD)
                    .key('G', Tags.Items.GLASS_PANES_COLORLESS)
                    .key('I', illumars().apply(c))
                    .key('R', Items.REDSTONE_TORCH)
                    .patternLine("PNP")
                    .patternLine("GIG")
                    .patternLine("PRP");
        }

        //Fallout lights
        for (int c = 0; c < 16; c++) {
            shapedRecipe(MultipartLightType.FALLOUT.getItem(c, false), 1)
                    .key('C', Blocks.IRON_BARS)
                    .key('I', illumars().apply(c))
                    .key('N', Tags.Items.NUGGETS_GOLD)
                    .key('P', CoreContent.itemConductivePlate().get())
                    .patternLine("CCC")
                    .patternLine("CIC")
                    .patternLine("NPN");

            shapedRecipe(MultipartLightType.FALLOUT.getItem(c, true), 1)
                    .key('C', Blocks.IRON_BARS)
                    .key('I', illumars().apply(c))
                    .key('N', Tags.Items.NUGGETS_GOLD)
                    .key('P', CoreContent.itemCathode().get())
                    .patternLine("CCC")
                    .patternLine("CIC")
                    .patternLine("NPN");
        }

        //Cage lights
        for (int c = 0; c < 16; c++) {
            shapedRecipe(MultipartLightType.CAGE.getItem(c, false), 1)
                    .key('C', Blocks.IRON_BARS)
                    .key('I', illumars().apply(c))
                    .key('N', itemPlate().get())
                    .key('P', itemConductivePlate().get())
                    .patternLine(" C ")
                    .patternLine("CIC")
                    .patternLine("NPN");

            shapedRecipe(MultipartLightType.CAGE.getItem(c, true), 1)
                    .key('C', Blocks.IRON_BARS)
                    .key('I', illumars().apply(c))
                    .key('N', itemPlate().get())
                    .key('P', itemCathode().get())
                    .patternLine(" C ")
                    .patternLine("CIC")
                    .patternLine("NPN");
        }

        //Fixture lights
        for (int c = 0; c < 16; c++) {
            shapedRecipe(MultipartLightType.FIXTURE.getItem(c, false), 1)
                    .key('G', Tags.Items.GLASS_PANES_COLORLESS)
                    .key('I', illumars().apply(c))
                    .key('P', itemPlate().get())
                    .key('C', itemConductivePlate().get())
                    .patternLine("GGG")
                    .patternLine("GIG")
                    .patternLine("PCP");

            shapedRecipe(MultipartLightType.FIXTURE.getItem(c, true), 1)
                    .key('G', Tags.Items.GLASS_PANES_COLORLESS)
                    .key('I', illumars().apply(c))
                    .key('P', itemPlate().get())
                    .key('C', itemCathode().get())
                    .patternLine("GGG")
                    .patternLine("GIG")
                    .patternLine("PCP");
        }
    }
}
