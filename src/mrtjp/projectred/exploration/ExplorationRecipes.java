package mrtjp.projectred.exploration;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.core.libmc.PRColors;
import mrtjp.projectred.core.ShapelessOreNBTRecipe;
import mrtjp.projectred.exploration.BlockOre.EnumOre;
import mrtjp.projectred.exploration.BlockSpecialStone.EnumSpecialStone;
import mrtjp.projectred.exploration.ItemBackpack.EnumBackpack;
import net.minecraft.block.Block;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.FurnaceRecipes;
import net.minecraftforge.oredict.OreDictionary;
import net.minecraftforge.oredict.ShapedOreRecipe;
import cpw.mods.fml.common.registry.GameRegistry;

public class ExplorationRecipes
{
    public static void initRecipes()
    {
        initOtherRecipes();
        initGemToolRecipes();
        initToolRecipes();
        initWorldRecipes();
    }

    public static void initOreDict()
    {
        EnumBackpack.initOreDictDefinitions();

        OreDictionary.registerOre("gemRuby", EnumPart.RUBY.getItemStack());
        OreDictionary.registerOre("gemSapphire", EnumPart.SAPPHIRE.getItemStack());
        OreDictionary.registerOre("gemPeridot", EnumPart.PERIDOT.getItemStack());

        OreDictionary.registerOre("oreRuby", EnumOre.ORERUBY.getItemStack(1));
        OreDictionary.registerOre("oreSapphire", EnumOre.ORESAPPHIRE.getItemStack(1));
        OreDictionary.registerOre("orePeridot", EnumOre.OREPERIDOT.getItemStack(1));
    }

    private static void initGemToolRecipes()
    {
        /** Axes **/
        addAxeRecipe(new ItemStack(ProjectRedExploration.itemRubyAxe()), "gemRuby");
        addAxeRecipe(new ItemStack(ProjectRedExploration.itemSapphireAxe()), "gemSapphire");
        addAxeRecipe(new ItemStack(ProjectRedExploration.itemPeridotAxe()), "gemPeridot");

        /** Hoes **/
        addHoeRecipe(new ItemStack(ProjectRedExploration.itemRubyHoe()), "gemRuby");
        addHoeRecipe(new ItemStack(ProjectRedExploration.itemSapphireHoe()), "gemSapphire");
        addHoeRecipe(new ItemStack(ProjectRedExploration.itemPeridotHoe()), "gemPeridot");

        /** Pickaxe **/
        addPickaxeRecipe(new ItemStack(ProjectRedExploration.itemRubyPickaxe()), "gemRuby");
        addPickaxeRecipe(new ItemStack(ProjectRedExploration.itemSapphirePickaxe()), "gemSapphire");
        addPickaxeRecipe(new ItemStack(ProjectRedExploration.itemPeridotPickaxe()), "gemPeridot");

        /** Shovel **/
        addShovelRecipe(new ItemStack(ProjectRedExploration.itemRubyShovel()), "gemRuby");
        addShovelRecipe(new ItemStack(ProjectRedExploration.itemSapphireShovel()), "gemSapphire");
        addShovelRecipe(new ItemStack(ProjectRedExploration.itemPeridotShovel()), "gemPeridot");

        /** Sword **/
        addSwordRecipe(new ItemStack(ProjectRedExploration.itemRubySword()), "gemRuby");
        addSwordRecipe(new ItemStack(ProjectRedExploration.itemSapphireSword()), "gemSapphire");
        addSwordRecipe(new ItemStack(ProjectRedExploration.itemPeridotSword()), "gemPeridot");

        /** Saw **/
        addSawRecipe(new ItemStack(ProjectRedExploration.itemGoldSaw()), new ItemStack(Item.ingotGold));
        addSawRecipe(new ItemStack(ProjectRedExploration.itemRubySaw()), "gemRuby");
        addSawRecipe(new ItemStack(ProjectRedExploration.itemSapphireSaw()), "gemSapphire");
        addSawRecipe(new ItemStack(ProjectRedExploration.itemPeridotSaw()), "gemPeridot");

        /** Sickle **/
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemWoodSickle()), "plankWood");
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemStoneSickle()), new ItemStack(Item.flint));
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemIronSickle()), new ItemStack(Item.ingotIron));
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemGoldSickle()), new ItemStack(Item.ingotGold));
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemRubySickle()), "gemRuby");
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemSapphireSickle()), "gemSapphire");
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemPeridotSickle()), "gemPeridot");
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemDiamondSickle()), new ItemStack(Item.diamond));

    }

    private static void addAxeRecipe(ItemStack o, Object m)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
                "mm",
                "ms",
                " s",
                'm', m,
                's', Item.stick
                ));
    }
    private static void addHoeRecipe(ItemStack o, Object m)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
                "mm",
                " s",
                " s",
                'm', m,
                's', Item.stick
                ));
    }
    private static void addPickaxeRecipe(ItemStack o, Object m)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
                "mmm",
                " s ",
                " s ",
                'm', m,
                's', Item.stick
                ));
    }
    private static void addShovelRecipe(ItemStack o, Object m)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
                "m",
                "s",
                "s",
                'm', m,
                's', Item.stick
                ));
    }
    private static void addSwordRecipe(ItemStack o, Object m)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
                "m",
                "m",
                "s",
                'm', m,
                's', Item.stick
                ));
    }
    private static void addSawRecipe(ItemStack o, Object m)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
                "srr",
                "sbb",
                's', Item.stick,
                'r', "stoneRod",
                'b', m
                ));
    }
    private static void addSickleRecipe(ItemStack o, Object m)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
                " m ",
                "  m",
                "sm ",
                's', Item.stick,
                'm', m
                ));
    }


    private static void initOtherRecipes()
    {
        /** Wool Gin to string recipe **/
        GameRegistry.addRecipe(new ItemStack(Item.silk, 4),
                "gw",
                'g', new ItemStack(ProjectRedExploration.itemWoolGin(), 1, OreDictionary.WILDCARD_VALUE),
                'w', Block.cloth
                );
    }

    private static void initToolRecipes()
    {
        /** Wool Gin **/
        GameRegistry.addRecipe(new ItemStack(ProjectRedExploration.itemWoolGin()),
                "sis",
                "sss",
                " s ",
                's', Item.stick,
                'i', EnumPart.IRONCOIL.getItemStack()
                );

        /** Backpacks **/
        for (int i = 0; i < EnumBackpack.VALID_BP.length; i++) {
            GameRegistry.addRecipe(new ShapedOreRecipe(EnumBackpack.get(i).getItemStack(),
                    "ccc",
                    "cdc",
                    "ccc",
                    'c', EnumPart.WOVENCLOTH.getItemStack(),
                    'd', PRColors.get(i).getOreDict()
                    ));
            GameRegistry.addRecipe(new ShapelessOreNBTRecipe(EnumBackpack.get(i).getItemStack(),
                    EnumBackpack.oreDictDefinition,
                    PRColors.get(i).getOreDict()
                    ).setKeepNBT());
        }
    }

    private static void initWorldRecipes()
    {
        /** Marble brick **/
        GameRegistry.addRecipe(EnumSpecialStone.MARBLEBRICK.getItemStack(4),
                "bb",
                "bb",
                'b', EnumSpecialStone.MARBLE.getItemStack()
                );
        /** Basalt brick **/
        GameRegistry.addRecipe(EnumSpecialStone.BASALTBRICK.getItemStack(4),
                "bb",
                "bb",
                'b', EnumSpecialStone.BASALT.getItemStack()
                );
        /** Basalt **/
        FurnaceRecipes.smelting().addSmelting(ProjectRedExploration.blockStones().blockID, EnumSpecialStone.BASALTCOBBLE.meta, EnumSpecialStone.BASALT.getItemStack(), 0);

        /** Ruby block **/
        GameRegistry.addRecipe(EnumSpecialStone.RUBYBLOCK.getItemStack(),
                "xxx",
                "xxx",
                "xxx",
                'x', EnumPart.RUBY.getItemStack()
                );
        /** Sapphire block **/
        GameRegistry.addRecipe(EnumSpecialStone.SAPPHIREBLOCK.getItemStack(),
                "xxx",
                "xxx",
                "xxx",
                'x', EnumPart.SAPPHIRE.getItemStack()
                );
        /** Peridot block **/
        GameRegistry.addRecipe(EnumSpecialStone.PERIDOTBLOCK.getItemStack(),
                "xxx",
                "xxx",
                "xxx",
                'x', EnumPart.PERIDOT.getItemStack()
                );

        /** Ruby **/
        GameRegistry.addShapelessRecipe(EnumPart.RUBY.getItemStack(9), EnumSpecialStone.RUBYBLOCK.getItemStack());
        /** Sapphire **/
        GameRegistry.addShapelessRecipe(EnumPart.SAPPHIRE.getItemStack(9), EnumSpecialStone.SAPPHIREBLOCK.getItemStack());
        /** Peridot **/
        GameRegistry.addShapelessRecipe(EnumPart.PERIDOT.getItemStack(9), EnumSpecialStone.PERIDOTBLOCK.getItemStack());

        /** Walls **/
        for (EnumSpecialStone s : EnumSpecialStone.VALID_STONE)
            addWallRecipe(new ItemStack(ProjectRedExploration.blockStoneWalls(), 6, s.meta), s.getItemStack());
    }

    private static void addWallRecipe(ItemStack o, ItemStack m)
    {
        GameRegistry.addRecipe(o,
                "mmm",
                "mmm",
                'm', m
                );
    }
}
