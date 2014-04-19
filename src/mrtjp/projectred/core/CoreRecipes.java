package mrtjp.projectred.core;

import codechicken.microblock.ItemMicroPart;
import codechicken.microblock.handler.MicroblockProxy;
import cpw.mods.fml.common.registry.GameRegistry;
import mrtjp.projectred.ProjectRedCore;
import mrtjp.projectred.core.libmc.PRColors;
import net.minecraft.block.Block;
import net.minecraft.init.Blocks;
import net.minecraft.init.Items;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.FurnaceRecipes;
import net.minecraftforge.oredict.OreDictionary;
import net.minecraftforge.oredict.ShapedOreRecipe;
import net.minecraftforge.oredict.ShapelessOreRecipe;
import scala.collection.Iterator;

import static mrtjp.projectred.core.PartCollection.*;

public class CoreRecipes
{
    public static void initCoreRecipes()
    {
        initPartRecipes();
        initToolRecipes();
    }

    private static void initToolRecipes()
    {
        /** Draw Plate **/
        GameRegistry.addRecipe(new ShapedOreNBTRecipe(
                new ItemStack(ProjectRedCore.itemDrawPlate(), 1),
                " i ",
                "idi",
                " i ",
                'i', ItemMicroPart.create(770, Blocks.iron_block.getUnlocalizedName()),
                'd', ItemMicroPart.create(2, Blocks.diamond_block.getUnlocalizedName())
                ).setCheckNBT());

        /** Screw Driver **/
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedCore.itemScrewdriver()),
                "i  ",
                " ib",
                " bi",
                'i', Items.iron_ingot,
                'b', PRColors.BLUE.getOreDict()
                ));

        /** Wire debugger **/
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedCore.itemWireDebugger()),
                "a a",
                "ber",
                "bgr",
                'a', REDINGOT().makeStack(),
                'b', PRColors.BLACK.getOreDict(),
                'e', Items.emerald,
                'r', PRColors.RED.getOreDict(),
                'g', Items.glowstone_dust
                ));

        /** Data Card **/
        GameRegistry.addRecipe(new ItemStack(ProjectRedCore.itemDataCard()),
                "pp ",
                "prp",
                "prp",
                'p', Items.paper,
                'r', Items.redstone
                );


    }

    private static void initPartRecipes()
    {
        /** Circuit Plate **/
        GameRegistry.addSmelting(Blocks.stone, PLATE().makeStack(2), 0f);

        /** Conductive Plate **/
        GameRegistry.addRecipe(CONDUCTIVEPLATE().makeStack(),
                "r",
                "p",
                'r', Items.redstone,
                'p', PLATE().makeStack()
                );

        /** Anode **/
        GameRegistry.addRecipe(ANODE().makeStack(3),
                " r ",
                "rrr",
                "ppp",
                'r', Items.redstone,
                'p', PLATE().makeStack()
                );

        /** Cathode **/
        GameRegistry.addRecipe(CATHODE().makeStack(),
                "t",
                "p",
                't', Blocks.redstone_torch,
                'p', PLATE().makeStack()
                );

        /** Pointer **/
        GameRegistry.addRecipe(POINTER().makeStack(),
                "b",
                "m",
                "c",
                'b', Blocks.stone,
                'm', Blocks.redstone_torch,
                'c', PLATE().makeStack()
                );

        /** Silicon Chip **/
        GameRegistry.addRecipe(SILICONCHIP().makeStack(),
                " s ",
                "ppp",
                's', INFUSEDSILICON().makeStack(),
                'p', PLATE().makeStack()
                );

        /** Energized Silicon Chip **/
        GameRegistry.addRecipe(ENERGIZEDSILICONCHIP().makeStack(),
                " e ",
                "ppp",
                'e', ENERGIZEDSILICON().makeStack(),
                'p', PLATE().makeStack()
                );

        /** Platformed Plate **/
        GameRegistry.addRecipe(PLATFORMEDPLATE().makeStack(),
                " r ",
                "sps",
                "prp",
                'r', WIREDPLATE().makeStack(),
                's', Items.stick,
                'p', PLATE().makeStack()
                );


        /** Silicon Boule **/
        FurnaceRecipes.smelting().addSmelting(ProjectRedCore.itemPart(), SANDYCOALCOMPOUND().meta(), SILICONBOULE().makeStack(), 0);

        /** Silicon **/
        GameRegistry.addRecipe(SILICON().makeStack(8),
                "s",
                "b",
                's', new ItemStack(MicroblockProxy.sawDiamond(), 1, OreDictionary.WILDCARD_VALUE),
                'b', SILICONBOULE().makeStack()
                );

        /** Infused Silicon **/
        FurnaceRecipes.smelting().addSmelting(ProjectRedCore.itemPart(), REDSILICONCOMPOUND().meta(), INFUSEDSILICON().makeStack(), 0);

        /** Energized Silicon **/
        FurnaceRecipes.smelting().addSmelting(ProjectRedCore.itemPart(), GLOWINGSILICONCOMPOUND().meta(), ENERGIZEDSILICON().makeStack(), 0);

        /** Motor **/
        GameRegistry.addRecipe(MOTOR().makeStack(),
                " i ",
                "scs",
                "rcr",
                'i', Items.iron_ingot,
                's', Blocks.stone,
                'c', COPPERCOIL().makeStack(),
                'r', Items.redstone
                );

        /** Copper Coil **/
        GameRegistry.addRecipe(new ShapedOreRecipe(COPPERCOIL().makeStack(),
                "cd",
                'c', "ingotCopper",
                'd', new ItemStack(ProjectRedCore.itemDrawPlate(), 1, OreDictionary.WILDCARD_VALUE)
                ));

        /** Iron Coil **/
        GameRegistry.addRecipe(IRONCOIL().makeStack(),
                "cd",
                'c', Items.iron_ingot,
                'd', new ItemStack(ProjectRedCore.itemDrawPlate(), 1, OreDictionary.WILDCARD_VALUE)
                );

        /** Gold Coil **/
        GameRegistry.addRecipe(GOLDCOIL().makeStack(),
                "cd",
                'c', Items.gold_ingot,
                'd', new ItemStack(ProjectRedCore.itemDrawPlate(), 1, OreDictionary.WILDCARD_VALUE)
                );

        /** Red Alloy Ingot **/
        FurnaceRecipes.smelting().addSmelting(ProjectRedCore.itemPart(), REDIRONCOMPOUND().meta(), REDINGOT().makeStack(), 0);

        /** Illumar **/
        Iterator it = ILLUMARS().iterator();
        for (int i = 0; i < ILLUMARS().size(); i++) {
            PartVal p = (PartVal)it.next();
            GameRegistry.addRecipe(new ShapelessOreRecipe(p.makeStack(),
                    new ItemStack(Items.glowstone_dust),
                    new ItemStack(Items.glowstone_dust),
                    PRColors.get(i).getOreDict(),
                    PRColors.get(i).getOreDict()
                    ));
        }

        /** Woven Cloth **/
        GameRegistry.addRecipe(WOVENCLOTH().makeStack(),
                "sss",
                "sws",
                "sss",
                's', Item.itemRegistry.getObject("silk"),
                'w', Item.itemRegistry.getObject("stick")
                );

        /** Sail **/
        GameRegistry.addRecipe(SAIL().makeStack(),
                "ss",
                "ss",
                "ss",
                's', WOVENCLOTH().makeStack()
                );

        /** Red Iron Compound **/
        GameRegistry.addRecipe(REDIRONCOMPOUND().makeStack(),
                "rrr",
                "rir",
                "rrr",
                'r', Items.redstone,
                'i', Items.iron_ingot
                );

        /** Sandy Coal Compound **/
        GameRegistry.addRecipe(SANDYCOALCOMPOUND().makeStack(),
                "sss",
                "scs",
                "sss",
                'c', Block.getBlockFromName("coal_block"),
                's', Block.getBlockFromName("sand")
                );

        /** Red Silicon Compound **/
        GameRegistry.addRecipe(REDSILICONCOMPOUND().makeStack(),
                "rrr",
                "rsr",
                "rrr",
                'r', Items.redstone,
                's', SILICON().makeStack()
                );

        /** Glowing Silicon Compound **/
        GameRegistry.addRecipe(GLOWINGSILICONCOMPOUND().makeStack(),
                "ggg",
                "gsg",
                "ggg",
                'g', Items.glowstone_dust,
                's', SILICON().makeStack()
                );

    }

}
