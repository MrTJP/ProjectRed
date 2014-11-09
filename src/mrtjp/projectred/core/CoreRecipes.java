package mrtjp.projectred.core;

import codechicken.microblock.handler.MicroblockProxy;
import cpw.mods.fml.common.registry.GameRegistry;
import mrtjp.projectred.ProjectRedCore;
import mrtjp.core.color.Colors;
import mrtjp.projectred.core.libmc.recipe.*;
import net.minecraft.init.Blocks;
import net.minecraft.init.Items;
import net.minecraft.item.ItemStack;
import net.minecraftforge.oredict.OreDictionary;
import net.minecraftforge.oredict.ShapedOreRecipe;
import net.minecraftforge.oredict.ShapelessOreRecipe;
import scala.collection.Iterator;

import static mrtjp.projectred.core.PartDefs.*;

public class CoreRecipes
{
    public static void initCoreRecipes()
    {
        PartDefs.initOreDict();
        initPartRecipes();
        initToolRecipes();
    }

    private static void initToolRecipes()
    {
        ShapedRecipeBuilder b = RecipeLib.newShapedBuilder();
        /** Draw Plate **/
        b.$less$minus$greater(" i "+"idi"+" i ")
                .$plus$eq((Input)new MicroIn(MicroIn.edge(), MicroIn.fourth(), Blocks.iron_block).to("i"))
                .$plus$eq((Input)new MicroIn(MicroIn.face(), MicroIn.fourth(), Blocks.diamond_block).to("d"))
                .$plus$eq((Output)new ItemOut(ProjectRedCore.itemDrawPlate()));
        b.registerResult();

        /** Screw Driver **/
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedCore.itemScrewdriver()),
                "i  ",
                " ib",
                " bi",
                'i', Items.iron_ingot,
                'b', Colors.BLUE.getOreDict()
        ));

        /** Wire debugger **/
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedCore.itemWireDebugger()),
                "a a",
                "ber",
                "bgr",
                'a', REDINGOT().makeStack(),
                'b', Colors.BLACK.getOreDict(),
                'e', Items.emerald,
                'r', Colors.RED.getOreDict(),
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
                's', "stickWood",
                'p', PLATE().makeStack()
                );


        /** Silicon Boule **/
        GameRegistry.addSmelting(SANDYCOALCOMPOUND().makeStack(), SILICONBOULE().makeStack(), 0);

        /** Silicon **/
        GameRegistry.addRecipe(SILICON().makeStack(8),
                "s",
                "b",
                's', new ItemStack(MicroblockProxy.sawDiamond(), 1, OreDictionary.WILDCARD_VALUE),
                'b', SILICONBOULE().makeStack()
                );

        /** Infused Silicon **/
        GameRegistry.addSmelting(REDSILICONCOMPOUND().makeStack(), INFUSEDSILICON().makeStack(), 0);

        /** Energized Silicon **/
        GameRegistry.addSmelting(GLOWINGSILICONCOMPOUND().makeStack(), ENERGIZEDSILICON().makeStack(), 0);

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
        GameRegistry.addSmelting(REDIRONCOMPOUND().makeStack(), REDINGOT().makeStack(), 0);

        /** Illumar **/
        Iterator it = ILLUMARS().iterator();
        for (int i = 0; i < ILLUMARS().size(); i++) {
            PartVal p = (PartVal)it.next();
            GameRegistry.addRecipe(new ShapelessOreRecipe(p.makeStack(),
                    new ItemStack(Items.glowstone_dust),
                    new ItemStack(Items.glowstone_dust),
                    Colors.get(i).getOreDict(),
                    Colors.get(i).getOreDict()
                    ));
        }

        /** Woven Cloth **/
        GameRegistry.addRecipe(WOVENCLOTH().makeStack(),
                "sss",
                "sws",
                "sss",
                's', Items.string,
                'w', "stickWood"
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
                'c', Blocks.coal_block,
                's', Blocks.sand
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
