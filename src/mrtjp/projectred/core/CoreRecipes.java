package mrtjp.projectred.core;

import codechicken.microblock.handler.MicroblockProxy;
import cpw.mods.fml.common.registry.GameRegistry;
import mrtjp.core.color.Colors_old;
import mrtjp.projectred.ProjectRedCore;
import mrtjp.projectred.core.libmc.recipe.*;
import net.minecraft.init.Blocks;
import net.minecraft.init.Items;
import net.minecraft.item.ItemStack;
import net.minecraftforge.oredict.OreDictionary;
import net.minecraftforge.oredict.ShapedOreRecipe;
import net.minecraftforge.oredict.ShapelessOreRecipe;

import static mrtjp.projectred.core.PartDefs.*;

public class CoreRecipes
{
    public static void initCoreRecipes()
    {
        initOreDict();
        initPartRecipes();
        initToolRecipes();
    }

    private static void initOreDict()
    {
        for (int i = 0; i < 16; i++)
            OreDictionary.registerOre(PartDefs.oreDictDefinitionIllumar(), PartDefs.ILLUMARS()[i].makeStack());

        OreDictionary.registerOre("gemRuby", PartDefs.RUBY().makeStack());
        OreDictionary.registerOre("gemSapphire", PartDefs.SAPPHIRE().makeStack());
        OreDictionary.registerOre("gemPeridot", PartDefs.PERIDOT().makeStack());

        OreDictionary.registerOre("ingotRedAlloy", PartDefs.REDINGOT().makeStack());
        OreDictionary.registerOre("ingotCopper", PartDefs.COPPERINGOT().makeStack());
        OreDictionary.registerOre("ingotTin", PartDefs.TININGOT().makeStack());
        OreDictionary.registerOre("ingotSilver", PartDefs.SILVERINGOT().makeStack());
        OreDictionary.registerOre("ingotElectrotine", PartDefs.ELECTROTINEINGOT().makeStack());

        OreDictionary.registerOre("dustElectrotine", PartDefs.ELECTROTINE().makeStack());
    }

    private static void initToolRecipes()
    {
        /** Draw Plate **/
        ShapedRecipeBuilder b = RecipeLib.newShapedBuilder();
        b.$less$minus$greater(" i "+"idi"+" i ")
                .$plus$eq((Input)new MicroIn(MicroIn.edge(), MicroIn.fourth(), Blocks.iron_block).to("i"))
                .$plus$eq((Input)new MicroIn(MicroIn.face(), MicroIn.fourth(), Blocks.diamond_block).to("d"))
                .$plus$eq((Output)new ItemOut(ProjectRedCore.itemDrawPlate()));
        b.registerResult();
        /** Panel Reset recipe **/
        ShapelessRecipeBuilder s = RecipeLib.newShapelessBuilder();
        s.$plus$eq((Input) new MicroIn(MicroIn.face(), MicroIn.fourth(), Blocks.diamond_block))
                .$plus$eq((Output) new ItemOut(new ItemStack(Items.diamond, 2)));
        s.registerResult();

        /** Screwdriver **/
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedCore.itemScrewdriver()),
                "i  ",
                " ib",
                " bi",
                'i', "ingotIron",
                'b', "dyeBlue"
        ));

        /** Wire Debugger **/
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedCore.itemWireDebugger()),
                "a a",
                "ber",
                "bgr",
                'a', "ingotRedAlloy",
                'b', "dyeBlack",
                'e', "gemEmerald",
                'r', "dyeRed",
                'g', "dustGlowstone"
                ));

        /** Data Card **/
        GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedCore.itemDataCard()),
                "pp ",
                "prp",
                "prp",
                'p', Items.paper,
                'r', "dustRedstone"
                ));


    }

    private static void initPartRecipes()
    {
        /** Circuit Plate **/
        GameRegistry.addSmelting(Blocks.stone, PLATE().makeStack(2), 0f);

        /** Conductive Plate **/
        GameRegistry.addRecipe(new ShapedOreRecipe(CONDUCTIVEPLATE().makeStack(),
                "r",
                "p",
                'r', "dustRedstone",
                'p', PLATE().makeStack()
                ));

        /** Anode **/
        GameRegistry.addRecipe(new ShapedOreRecipe(ANODE().makeStack(3),
                " r ",
                "rrr",
                "ppp",
                'r', "dustRedstone",
                'p', PLATE().makeStack()
                ));

        /** Cathode **/
        GameRegistry.addRecipe(CATHODE().makeStack(),
                "t",
                "p",
                't', Blocks.redstone_torch,
                'p', PLATE().makeStack()
                );

        /** Pointer **/
        GameRegistry.addRecipe(new ShapedOreRecipe(POINTER().makeStack(),
                "b",
                "m",
                "c",
                'b', "stone",
                'm', Blocks.redstone_torch,
                'c', PLATE().makeStack()
                ));

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
        GameRegistry.addRecipe(new ShapedOreRecipe(PLATFORMEDPLATE().makeStack(),
                " r ",
                "sps",
                "prp",
                'r', WIREDPLATE().makeStack(),
                's', "stickWood",
                'p', PLATE().makeStack()
                ));


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
        GameRegistry.addRecipe(new ShapedOreRecipe(MOTOR().makeStack(),
                " i ",
                "scs",
                "rcr",
                'i', "ingotIron",
                's', "stone",
                'c', COPPERCOIL().makeStack(),
                'r', "dustRedstone"
                ));

        /** Copper Coil **/
        GameRegistry.addRecipe(new ShapedOreRecipe(COPPERCOIL().makeStack(),
                "cd",
                'c', "ingotCopper",
                'd', new ItemStack(ProjectRedCore.itemDrawPlate(), 1, OreDictionary.WILDCARD_VALUE)
                ));

        /** Iron Coil **/
        GameRegistry.addRecipe(new ShapedOreRecipe(IRONCOIL().makeStack(),
                "cd",
                'c', "ingotIron",
                'd', new ItemStack(ProjectRedCore.itemDrawPlate(), 1, OreDictionary.WILDCARD_VALUE)
                ));

        /** Gold Coil **/
        GameRegistry.addRecipe(new ShapedOreRecipe(GOLDCOIL().makeStack(),
                "cd",
                'c', "ingotGold",
                'd', new ItemStack(ProjectRedCore.itemDrawPlate(), 1, OreDictionary.WILDCARD_VALUE)
                ));

        /** Red Alloy Ingot **/
        GameRegistry.addSmelting(REDIRONCOMPOUND().makeStack(), REDINGOT().makeStack(), 0);

        /** Illumar **/
        for (int i = 0; i < 16; i++) {
            PartVal p = PartDefs.ILLUMARS()[i];
            GameRegistry.addRecipe(new ShapelessOreRecipe(p.makeStack(),
                    "dustGlowstone",
                    "dustGlowstone",
                    Colors_old.get(i).getOreDict(),
                    Colors_old.get(i).getOreDict()
                    ));
        }

        /** Woven Cloth **/
        GameRegistry.addRecipe(new ShapedOreRecipe(WOVENCLOTH().makeStack(),
                "sss",
                "sws",
                "sss",
                's', Items.string,
                'w', "stickWood"
                ));

        /** Sail **/
        GameRegistry.addRecipe(SAIL().makeStack(),
                "ss",
                "ss",
                "ss",
                's', WOVENCLOTH().makeStack()
                );

        /** Red Iron Compound **/
        GameRegistry.addRecipe(new ShapedOreRecipe(REDIRONCOMPOUND().makeStack(),
                "rrr",
                "rir",
                "rrr",
                'r', "dustRedstone",
                'i', "ingotIron"
                ));

        /** Sandy Coal Compound **/
        GameRegistry.addRecipe(new ShapedOreRecipe(SANDYCOALCOMPOUND().makeStack(),
                "sss",
                "scs",
                "sss",
                'c', "blockCoal",
                's', Blocks.sand
                ));

        /** Red Silicon Compound **/
        GameRegistry.addRecipe(new ShapedOreRecipe(REDSILICONCOMPOUND().makeStack(),
                "rrr",
                "rsr",
                "rrr",
                'r', "dustRedstone",
                's', SILICON().makeStack()
                ));

        /** Glowing Silicon Compound **/
        GameRegistry.addRecipe(new ShapedOreRecipe(GLOWINGSILICONCOMPOUND().makeStack(),
                "ggg",
                "gsg",
                "ggg",
                'g', "dustGlowstone",
                's', SILICON().makeStack()
                ));

        /** Electrotine Ingot **/
        GameRegistry.addSmelting(ELECTROTINEIRONCOMPOUND().makeStack(), ELECTROTINEINGOT().makeStack(), 0);

        /** Electrotine Iron Compound **/
        GameRegistry.addRecipe(new ShapedOreRecipe(ELECTROTINEIRONCOMPOUND().makeStack(),
                "bbb",
                "bib",
                "bbb",
                'b', "dustElectrotine",
                'i', "ingotIron"
        ));

        /** Electrotine Silicon Compound **/
        GameRegistry.addRecipe(new ShapedOreRecipe(ELECTROTINESILICONCOMPOUND().makeStack(),
                "bbb",
                "bsb",
                "bbb",
                'b', "dustElectrotine",
                's', SILICON().makeStack()
        ));

        /** Electrosilicon **/
        GameRegistry.addSmelting(ELECTROTINESILICONCOMPOUND().makeStack(), ELECTROSILICON().makeStack(), 0);
    }
}