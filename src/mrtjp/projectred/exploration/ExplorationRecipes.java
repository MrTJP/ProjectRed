package mrtjp.projectred.exploration;

import cpw.mods.fml.common.registry.GameRegistry;
import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.PartDefs;
import mrtjp.projectred.core.ShapelessOreNBTRecipe;
import mrtjp.core.color.Colors;
import mrtjp.projectred.exploration.DecorativeStoneDefs.StoneVal;
import net.minecraft.init.Blocks;
import net.minecraft.init.Items;
import net.minecraft.item.ItemStack;
import net.minecraftforge.oredict.OreDictionary;
import net.minecraftforge.oredict.ShapedOreRecipe;

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
        for (int i = 0; i < 16; i++) OreDictionary.registerOre(ItemBackpack.oreDictionaryVal(),
                new ItemStack(ProjectRedExploration.itemBackpack(), 1, i));

        OreDictionary.registerOre("gemRuby", PartDefs.RUBY().makeStack());
        OreDictionary.registerOre("gemSapphire", PartDefs.SAPPHIRE().makeStack());
        OreDictionary.registerOre("gemPeridot", PartDefs.PERIDOT().makeStack());

        OreDictionary.registerOre("oreRuby", OreDefs.ORERUBY().makeStack());
        OreDictionary.registerOre("oreSapphire", OreDefs.ORESAPPHIRE().makeStack());
        OreDictionary.registerOre("orePeridot", OreDefs.OREPERIDOT().makeStack());

        OreDictionary.registerOre("blockMarble", DecorativeStoneDefs.MARBLE().makeStack());
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
        addSawRecipe(new ItemStack(ProjectRedExploration.itemGoldSaw()), new ItemStack(Items.gold_ingot));
        addSawRecipe(new ItemStack(ProjectRedExploration.itemRubySaw()), "gemRuby");
        addSawRecipe(new ItemStack(ProjectRedExploration.itemSapphireSaw()), "gemSapphire");
        addSawRecipe(new ItemStack(ProjectRedExploration.itemPeridotSaw()), "gemPeridot");

        /** Sickle **/
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemWoodSickle()), "plankWood");
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemStoneSickle()), new ItemStack(Items.flint));
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemIronSickle()), new ItemStack(Items.iron_ingot));
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemGoldSickle()), new ItemStack(Items.gold_ingot));
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemRubySickle()), "gemRuby");
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemSapphireSickle()), "gemSapphire");
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemPeridotSickle()), "gemPeridot");
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemDiamondSickle()), new ItemStack(Items.diamond));

    }

    private static void addAxeRecipe(ItemStack o, Object m)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
                "mm",
                "ms",
                " s",
                'm', m,
                's', Items.stick
                ));
    }
    private static void addHoeRecipe(ItemStack o, Object m)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
                "mm",
                " s",
                " s",
                'm', m,
                's', Items.stick
                ));
    }
    private static void addPickaxeRecipe(ItemStack o, Object m)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
                "mmm",
                " s ",
                " s ",
                'm', m,
                's', Items.stick
                ));
    }
    private static void addShovelRecipe(ItemStack o, Object m)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
                "m",
                "s",
                "s",
                'm', m,
                's', Items.stick
                ));
    }
    private static void addSwordRecipe(ItemStack o, Object m)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
                "m",
                "m",
                "s",
                'm', m,
                's', Items.stick
                ));
    }
    private static void addSawRecipe(ItemStack o, Object m)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
                "srr",
                "sbb",
                's', Items.stick,
                'r', "rodStone",
                'b', m
                ));
    }
    private static void addSickleRecipe(ItemStack o, Object m)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
                " m ",
                "  m",
                "sm ",
                's', Items.stick,
                'm', m
                ));
    }


    private static void initOtherRecipes()
    {
        /** Wool Gin to string recipe **/
        GameRegistry.addRecipe(new ItemStack(Items.string, 4),
                "gw",
                'g', new ItemStack(ProjectRedExploration.itemWoolGin(), 1, OreDictionary.WILDCARD_VALUE),
                'w', Blocks.wool
                );
    }

    private static void initToolRecipes()
    {
        /** Wool Gin **/
        GameRegistry.addRecipe(new ItemStack(ProjectRedExploration.itemWoolGin()),
                "sis",
                "sss",
                " s ",
                's', Items.stick,
                'i', PartDefs.IRONCOIL().makeStack()
                );

        /** Backpacks **/
        for (int i = 0; i < 16; i++) {
            GameRegistry.addRecipe(new ShapedOreRecipe(new ItemStack(ProjectRedExploration.itemBackpack(), 1, i),
                    "ccc",
                    "cdc",
                    "ccc",
                    'c', PartDefs.WOVENCLOTH().makeStack(),
                    'd', Colors.get(i).getOreDict()
                    ));
            GameRegistry.addRecipe(new ShapelessOreNBTRecipe(new ItemStack(ProjectRedExploration.itemBackpack(), 1, i),
                    ItemBackpack.oreDictionaryVal(),
                    Colors.get(i).getOreDict()
                    ).setKeepNBT());
        }
    }

    private static void initWorldRecipes()
    {
        /** Marble brick **/
        GameRegistry.addRecipe(new ShapedOreRecipe(DecorativeStoneDefs.MARBLEBRICK().makeStack(4),
                "bb",
                "bb",
                'b', "blockMarble"
                ));
        /** Basalt brick **/
        GameRegistry.addRecipe(DecorativeStoneDefs.BASALTBRICK().makeStack(4),
                "bb",
                "bb",
                'b', DecorativeStoneDefs.BASALT().makeStack()
                );
        /** Basalt **/
        GameRegistry.addSmelting(DecorativeStoneDefs.BASALTCOBBLE().makeStack(), DecorativeStoneDefs.BASALT().makeStack(), 0);

        /** Ruby block **/
        GameRegistry.addRecipe(DecorativeStoneDefs.RUBYBLOCK().makeStack(),
                "xxx",
                "xxx",
                "xxx",
                'x', PartDefs.RUBY().makeStack()
                );
        /** Sapphire block **/
        GameRegistry.addRecipe(DecorativeStoneDefs.SAPPHIREBLOCK().makeStack(),
                "xxx",
                "xxx",
                "xxx",
                'x', PartDefs.SAPPHIRE().makeStack()
                );
        /** Peridot block **/
        GameRegistry.addRecipe(DecorativeStoneDefs.PERIDOTBLOCK().makeStack(),
                "xxx",
                "xxx",
                "xxx",
                'x', PartDefs.PERIDOT().makeStack()
                );

        /** Ruby **/
        GameRegistry.addShapelessRecipe(PartDefs.RUBY().makeStack(9), DecorativeStoneDefs.RUBYBLOCK().makeStack());
        /** Sapphire **/
        GameRegistry.addShapelessRecipe(PartDefs.SAPPHIRE().makeStack(9), DecorativeStoneDefs.SAPPHIREBLOCK().makeStack());
        /** Peridot **/
        GameRegistry.addShapelessRecipe(PartDefs.PERIDOT().makeStack(9), DecorativeStoneDefs.PERIDOTBLOCK().makeStack());

        /** Walls **/
        for (int i = 0; i < DecorativeStoneDefs.values().size(); i++)
        {
            StoneVal s = (StoneVal) DecorativeStoneDefs.values().apply(i);
            addWallRecipe(new ItemStack(ProjectRedExploration.blockDecorativeWalls(), 6, s.meta()), s.makeStack());
        }
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
