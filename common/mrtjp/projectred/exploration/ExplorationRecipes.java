package mrtjp.projectred.exploration;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.exploration.BlockSpecialStone.EnumSpecialStone;
import mrtjp.projectred.exploration.ItemBackpack.EnumBackpack;
import net.minecraft.block.Block;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.FurnaceRecipes;
import net.minecraftforge.oredict.ShapedOreRecipe;
import cpw.mods.fml.common.registry.GameRegistry;

public class ExplorationRecipes {
    public static void initRecipes() {
        initOtherRecipes();
        initGemToolRecipes();
        initToolRecipes();
        initStorageRecipes();
        initWorldRecipes();
    }

    private static void initGemToolRecipes() {
        /** Axes **/
        addAxeRecipe(new ItemStack(ProjectRedExploration.itemRubyAxe), EnumPart.RUBY.getItemStack());
        addAxeRecipe(new ItemStack(ProjectRedExploration.itemSapphireAxe), EnumPart.SAPPHIRE.getItemStack());
        addAxeRecipe(new ItemStack(ProjectRedExploration.itemPeridotAxe), EnumPart.PERIDOT.getItemStack());

        /** Hoes **/
        addHoeRecipe(new ItemStack(ProjectRedExploration.itemRubyHoe), EnumPart.RUBY.getItemStack());
        addHoeRecipe(new ItemStack(ProjectRedExploration.itemSapphireHoe), EnumPart.SAPPHIRE.getItemStack());
        addHoeRecipe(new ItemStack(ProjectRedExploration.itemPeridotHoe), EnumPart.PERIDOT.getItemStack());

        /** Pickaxe **/
        addPickaxeRecipe(new ItemStack(ProjectRedExploration.itemRubyPickaxe), EnumPart.RUBY.getItemStack());
        addPickaxeRecipe(new ItemStack(ProjectRedExploration.itemSapphirePickaxe), EnumPart.SAPPHIRE.getItemStack());
        addPickaxeRecipe(new ItemStack(ProjectRedExploration.itemPeridotPickaxe), EnumPart.PERIDOT.getItemStack());

        /** Shovel **/
        addShovelRecipe(new ItemStack(ProjectRedExploration.itemRubyShovel), EnumPart.RUBY.getItemStack());
        addShovelRecipe(new ItemStack(ProjectRedExploration.itemSapphireShovel), EnumPart.SAPPHIRE.getItemStack());
        addShovelRecipe(new ItemStack(ProjectRedExploration.itemPeridotShovel), EnumPart.PERIDOT.getItemStack());

        /** Sword **/
        addSwordRecipe(new ItemStack(ProjectRedExploration.itemRubySword), EnumPart.RUBY.getItemStack());
        addSwordRecipe(new ItemStack(ProjectRedExploration.itemSapphireSword), EnumPart.SAPPHIRE.getItemStack());
        addSwordRecipe(new ItemStack(ProjectRedExploration.itemPeridotSword), EnumPart.PERIDOT.getItemStack());

        /** Saw **/
        addSawRecipe(new ItemStack(ProjectRedExploration.itemWoodSaw), new ItemStack(Block.planks));
        addSawRecipe(new ItemStack(ProjectRedExploration.itemStoneSaw), new ItemStack(Item.flint));
        addSawRecipe(new ItemStack(ProjectRedExploration.itemIronSaw), new ItemStack(Item.ingotIron));
        addSawRecipe(new ItemStack(ProjectRedExploration.itemGoldSaw), new ItemStack(Item.ingotGold));
        addSawRecipe(new ItemStack(ProjectRedExploration.itemRubySaw), EnumPart.RUBY.getItemStack());
        addSawRecipe(new ItemStack(ProjectRedExploration.itemSapphireSaw), EnumPart.SAPPHIRE.getItemStack());
        addSawRecipe(new ItemStack(ProjectRedExploration.itemPeridotSaw), EnumPart.PERIDOT.getItemStack());
        addSawRecipe(new ItemStack(ProjectRedExploration.itemDiamondSaw), new ItemStack(Item.diamond));

        /** Sickle **/
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemWoodSickle), new ItemStack(Block.planks));
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemStoneSickle), new ItemStack(Item.flint));
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemIronSickle), new ItemStack(Item.ingotIron));
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemGoldSickle), new ItemStack(Item.ingotGold));
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemRubySickle), EnumPart.RUBY.getItemStack());
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemSapphireSickle), EnumPart.SAPPHIRE.getItemStack());
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemPeridotSickle), EnumPart.PERIDOT.getItemStack());
        addSickleRecipe(new ItemStack(ProjectRedExploration.itemDiamondSickle), new ItemStack(Item.diamond));

    }

    private static void addAxeRecipe(ItemStack o, ItemStack m) {
        GameRegistry.addRecipe(o,
                "mm",
                "ms",
                " s",
                'm', m,
                's', Item.stick
        );
    }
    private static void addHoeRecipe(ItemStack o, ItemStack m) {
        GameRegistry.addRecipe(o,
                "mm",
                " s",
                " s",
                'm', m,
                's', Item.stick
        );
    }
    private static void addPickaxeRecipe(ItemStack o, ItemStack m) {
        GameRegistry.addRecipe(o,
                "mmm",
                " s ",
                " s ",
                'm', m,
                's', Item.stick
        );
    }
    private static void addShovelRecipe(ItemStack o, ItemStack m) {
        GameRegistry.addRecipe(o,
                "m",
                "s",
                "s",
                'm', m,
                's', Item.stick
        );
    }
    private static void addSwordRecipe(ItemStack o, ItemStack m) {
        GameRegistry.addRecipe(o,
                "m",
                "m",
                "s",
                'm', m,
                's', Item.stick
        );
    }
    private static void addSawRecipe(ItemStack o, ItemStack m) {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
                "srr",
                "sbb",
                's', Item.stick,
                'r', "stoneRod",
                'b', m
        ));
    }
    private static void addSickleRecipe(ItemStack o, ItemStack m) {
        GameRegistry.addRecipe(new ShapedOreRecipe(o,
                " m ",
                "  m",
                "sm ",
                's', Item.stick,
                'm', m
                ));
    }

    private static void initStorageRecipes() {
        GameRegistry.addRecipe(
            new ItemStack(ProjectRedExploration.blockStores, 1, 0),
            "xxx",
            "xxx",
            "xxx",
            'x', EnumPart.RUBY.getItemStack()
        );
        GameRegistry.addShapelessRecipe(
            EnumPart.RUBY.getItemStack(9),
            new ItemStack(ProjectRedExploration.blockStores, 1, 0)
        );

        GameRegistry.addRecipe(
            new ItemStack(ProjectRedExploration.blockStores, 1, 1),
            "xxx",
            "xxx",
            "xxx",
            'x', EnumPart.SAPPHIRE.getItemStack()
        );
        GameRegistry.addShapelessRecipe(
            EnumPart.SAPPHIRE.getItemStack(9),
            new ItemStack(ProjectRedExploration.blockStores, 1, 1)
        );

        GameRegistry.addRecipe(
            new ItemStack(ProjectRedExploration.blockStores, 1, 2),
            "xxx",
            "xxx",
            "xxx",
            'x', EnumPart.PERIDOT.getItemStack()
        );
        GameRegistry.addShapelessRecipe(
            EnumPart.PERIDOT.getItemStack(9),
            new ItemStack(ProjectRedExploration.blockStores, 1, 2)
        );
    }

    private static void initOtherRecipes() {
        /** Wool Gin to string recipe **/
        GameRegistry.addRecipe(new ItemStack(Item.silk, 4),
                "gw",
                'g', new ItemStack(ProjectRedExploration.itemWoolGin, 1, Short.MAX_VALUE),
                'w', Block.cloth
        );
    }

    private static void initToolRecipes() {
        /** Wool Gin **/
        GameRegistry.addRecipe(new ItemStack(ProjectRedExploration.itemWoolGin),
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
        }
        GameRegistry.addRecipe(new BackpackRecolouringRecipe());

    }

    private static void initWorldRecipes() {
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
        FurnaceRecipes.smelting().addSmelting(ProjectRedExploration.blockStones.blockID, EnumSpecialStone.BASALTCOBBLE.meta, EnumSpecialStone.BASALT.getItemStack(), 0);
    }


}
