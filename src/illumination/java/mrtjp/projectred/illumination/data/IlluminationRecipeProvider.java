package mrtjp.projectred.illumination.data;

import codechicken.lib.datagen.recipe.RecipeProvider;
import mrtjp.projectred.illumination.BlockLightType;
import mrtjp.projectred.illumination.MultipartLightType;
import net.minecraft.block.Blocks;
import net.minecraft.data.DataGenerator;
import net.minecraft.item.Item;
import net.minecraft.item.Items;
import net.minecraftforge.common.Tags;

import static mrtjp.projectred.core.init.CoreReferences.*;

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

        Item[] illumars = {
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

        //Lamps
        for (int c = 0; c < 16; c++) {
            shapedRecipe(BlockLightType.ILLUMAR_LAMP.getBlock(c, false), 1)
                    .key('G', Tags.Items.GLASS_PANES_COLORLESS)
                    .key('I', illumars[c])
                    .key('R', Tags.Items.DUSTS_REDSTONE)
                    .patternLine("GIG")
                    .patternLine("GIG")
                    .patternLine("GRG");

            shapedRecipe(BlockLightType.ILLUMAR_LAMP.getBlock(c, true), 1)
                    .key('G', Tags.Items.GLASS_PANES_COLORLESS)
                    .key('I', illumars[c])
                    .key('R', Items.REDSTONE_TORCH)
                    .patternLine("GIG")
                    .patternLine("GIG")
                    .patternLine("GRG");
        }

        //Lanterns
        for (int c = 0; c < 16; c++) {
            shapedRecipe(MultipartLightType.LANTERN.getItem(c, false), 1)
                    .key('P', PLATE_ITEM)
                    .key('N', Tags.Items.NUGGETS_GOLD)
                    .key('G', Tags.Items.GLASS_PANES_COLORLESS)
                    .key('I', illumars[c])
                    .key('R', Tags.Items.DUSTS_REDSTONE)
                    .patternLine("PNP")
                    .patternLine("GIG")
                    .patternLine("PRP");

            shapedRecipe(MultipartLightType.LANTERN.getItem(c, true), 1)
                    .key('P', PLATE_ITEM)
                    .key('N', Tags.Items.NUGGETS_GOLD)
                    .key('G', Tags.Items.GLASS_PANES_COLORLESS)
                    .key('I', illumars[c])
                    .key('R', Items.REDSTONE_TORCH)
                    .patternLine("PNP")
                    .patternLine("GIG")
                    .patternLine("PRP");
        }

        //Fallout lights
        for (int c = 0; c < 16; c++) {
            shapedRecipe(MultipartLightType.FALLOUT.getItem(c, false), 1)
                    .key('C', Blocks.IRON_BARS)
                    .key('I', illumars[c])
                    .key('N', Tags.Items.NUGGETS_GOLD)
                    .key('P', CONDUCTIVE_PLATE_ITEM)
                    .patternLine("CCC")
                    .patternLine("CIC")
                    .patternLine("NPN");

            shapedRecipe(MultipartLightType.FALLOUT.getItem(c, true), 1)
                    .key('C', Blocks.IRON_BARS)
                    .key('I', illumars[c])
                    .key('N', Tags.Items.NUGGETS_GOLD)
                    .key('P', CATHODE_ITEM)
                    .patternLine("CCC")
                    .patternLine("CIC")
                    .patternLine("NPN");
        }

        //Cage lights
        for (int c = 0; c < 16; c++) {
            shapedRecipe(MultipartLightType.CAGE.getItem(c, false), 1)
                    .key('C', Blocks.IRON_BARS)
                    .key('I', illumars[c])
                    .key('N', PLATE_ITEM)
                    .key('P', CONDUCTIVE_PLATE_ITEM)
                    .patternLine(" C ")
                    .patternLine("CIC")
                    .patternLine("NPN");

            shapedRecipe(MultipartLightType.CAGE.getItem(c, true), 1)
                    .key('C', Blocks.IRON_BARS)
                    .key('I', illumars[c])
                    .key('N', PLATE_ITEM)
                    .key('P', CATHODE_ITEM)
                    .patternLine(" C ")
                    .patternLine("CIC")
                    .patternLine("NPN");
        }

        //Fixture lights
        for (int c = 0; c < 16; c++) {
            shapedRecipe(MultipartLightType.FIXTURE.getItem(c, false), 1)
                    .key('G', Tags.Items.GLASS_PANES_COLORLESS)
                    .key('I', illumars[c])
                    .key('P', PLATE_ITEM)
                    .key('C', CONDUCTIVE_PLATE_ITEM)
                    .patternLine("GGG")
                    .patternLine("GIG")
                    .patternLine("PCP");

            shapedRecipe(MultipartLightType.FIXTURE.getItem(c, true), 1)
                    .key('G', Tags.Items.GLASS_PANES_COLORLESS)
                    .key('I', illumars[c])
                    .key('P', PLATE_ITEM)
                    .key('C', CATHODE_ITEM)
                    .patternLine("GGG")
                    .patternLine("GIG")
                    .patternLine("PCP");
        }
    }
}
