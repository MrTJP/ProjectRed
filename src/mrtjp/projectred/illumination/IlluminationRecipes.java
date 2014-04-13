package mrtjp.projectred.illumination;

import mrtjp.projectred.ProjectRedIllumination;
import mrtjp.projectred.core.ItemPart.EnumPart;
import net.minecraft.block.Block;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import cpw.mods.fml.common.registry.GameRegistry;

public class IlluminationRecipes
{
    public static void initRecipes()
    {
        initLightingRecipes();
    }

    private static void initLightingRecipes()
    {
        /** Lamps **/
        for (int i = 0; i < 16; i++)
        {
            GameRegistry.addRecipe(new ItemStack(ProjectRedIllumination.blockLamp(), 1, i),  // Regular
                    "gIg",
                    "gIg",
                    "gtg",
                    'g', Block.thinGlass,
                    'I', EnumPart.ILLUMAR_PARTS[i].getItemStack(),
                    't', Item.redstone
                    );
            GameRegistry.addRecipe(new ItemStack(ProjectRedIllumination.blockLamp(), 1, i+16), // Inverted
                    "gIg",
                    "gIg",
                    "gtg",
                    'g', Block.thinGlass,
                    'I', EnumPart.ILLUMAR_PARTS[i].getItemStack(),
                    't', Block.torchRedstoneActive
                    );
        }

        /** Lanterns **/
        for (int i = 0; i < 16; i++)
        {
            GameRegistry.addRecipe(new ItemStack(ProjectRedIllumination.itemPartLantern(), 1, i),
                    "PNP",
                    "GIG",
                    "PRP",
                    'P', EnumPart.PLATE.getItemStack(),
                    'N', Item.goldNugget,
                    'G', Block.thinGlass,
                    'I', EnumPart.ILLUMAR_PARTS[i].getItemStack(),
                    'R', Item.redstone
                    );
            GameRegistry.addRecipe(new ItemStack(ProjectRedIllumination.itemPartInvLantern(), 1, i),
                    "PNP",
                    "GIG",
                    "PRP",
                    'P', EnumPart.PLATE.getItemStack(),
                    'N', Item.goldNugget,
                    'G', Block.thinGlass,
                    'I', EnumPart.ILLUMAR_PARTS[i].getItemStack(),
                    'R', Block.torchRedstoneActive
                    );
        }

        /** Buttons **/
        for (int i = 0; i < 16; i++)
            GameRegistry.addShapelessRecipe(
                    new ItemStack(ProjectRedIllumination.itemPartIllumarButton(), 1, i),
                    Block.stoneButton,
                    EnumPart.ILLUMAR_PARTS[i].getItemStack(),
                    EnumPart.ILLUMAR_PARTS[i].getItemStack()
                    );

        /** Cage Lamps **/
        for (int i = 0; i < 16; i++)
        {
            GameRegistry.addRecipe(new ItemStack(ProjectRedIllumination.itemPartCageLamp(), 1, i),
                    "CCC",
                    "CIC",
                    "NPN",
                    'C', Block.fenceIron,
                    'I', EnumPart.ILLUMAR_PARTS[i].getItemStack(),
                    'N', Item.goldNugget,
                    'P', EnumPart.CONDUCTIVEPLATE.getItemStack()
                    );
            GameRegistry.addRecipe(new ItemStack(ProjectRedIllumination.itemPartInvCageLamp(), 1, i),
                    "CCC",
                    "CIC",
                    "NPN",
                    'C', Block.fenceIron,
                    'I', EnumPart.ILLUMAR_PARTS[i].getItemStack(),
                    'N', Item.goldNugget,
                    'P', EnumPart.CATHODE.getItemStack()
                    );
        }

        /** Fixtures **/
        for (int i = 0; i < 16; i++)
        {
            GameRegistry.addRecipe(new ItemStack(ProjectRedIllumination.itemPartFixture(), 1, i),
                    "ggg",
                    "gIg",
                    "pPp",
                    'g', Block.thinGlass,
                    'I', EnumPart.ILLUMAR_PARTS[i].getItemStack(),
                    'p', EnumPart.PLATE.getItemStack(),
                    'P', EnumPart.CONDUCTIVEPLATE.getItemStack()
                    );
            GameRegistry.addRecipe(new ItemStack(ProjectRedIllumination.itemPartInvFixture(), 1, i),
                    "ggg",
                    "gIg",
                    "pPp",
                    'g', Block.thinGlass,
                    'I', EnumPart.ILLUMAR_PARTS[i].getItemStack(),
                    'p', EnumPart.PLATE.getItemStack(),
                    'P', EnumPart.CATHODE.getItemStack()
                    );
        }
    }
}

