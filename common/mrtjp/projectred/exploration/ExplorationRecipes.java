package mrtjp.projectred.exploration;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.exploration.BlockSpecialStone.EnumSpecialStone;
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
    }
    
    private static void initGemToolRecipes() {
        /** Axes **/
        addAxeRecipe(new ItemStack(ProjectRed.itemRubyAxe), EnumPart.RUBY.getItemStack());
        addAxeRecipe(new ItemStack(ProjectRed.itemSapphireAxe), EnumPart.SAPPHIRE.getItemStack());
        addAxeRecipe(new ItemStack(ProjectRed.itemPeridotAxe), EnumPart.PERIDOT.getItemStack());
        
        /** Hoes **/
        addHoeRecipe(new ItemStack(ProjectRed.itemRubyHoe), EnumPart.RUBY.getItemStack());
        addHoeRecipe(new ItemStack(ProjectRed.itemSapphireHoe), EnumPart.SAPPHIRE.getItemStack());
        addHoeRecipe(new ItemStack(ProjectRed.itemPeridotHoe), EnumPart.PERIDOT.getItemStack());
        
        /** Pickaxe **/
        addPickaxeRecipe(new ItemStack(ProjectRed.itemRubyPickaxe), EnumPart.RUBY.getItemStack());
        addPickaxeRecipe(new ItemStack(ProjectRed.itemSapphirePickaxe), EnumPart.SAPPHIRE.getItemStack());
        addPickaxeRecipe(new ItemStack(ProjectRed.itemPeridotPickaxe), EnumPart.PERIDOT.getItemStack());
        
        /** Shovel **/
        addShovelRecipe(new ItemStack(ProjectRed.itemRubyShovel), EnumPart.RUBY.getItemStack());
        addShovelRecipe(new ItemStack(ProjectRed.itemSapphireShovel), EnumPart.SAPPHIRE.getItemStack());
        addShovelRecipe(new ItemStack(ProjectRed.itemPeridotShovel), EnumPart.PERIDOT.getItemStack());
        
        /** Sword **/
        addSwordRecipe(new ItemStack(ProjectRed.itemRubySword), EnumPart.RUBY.getItemStack());
        addSwordRecipe(new ItemStack(ProjectRed.itemSapphireSword), EnumPart.SAPPHIRE.getItemStack());
        addSwordRecipe(new ItemStack(ProjectRed.itemPeridotSword), EnumPart.PERIDOT.getItemStack());
    
        /** Saw **/
        addSawRecipe(new ItemStack(ProjectRed.itemWoodSaw), new ItemStack(Block.planks));
        addSawRecipe(new ItemStack(ProjectRed.itemStoneSaw), new ItemStack(Item.flint));
        addSawRecipe(new ItemStack(ProjectRed.itemIronSaw), new ItemStack(Item.ingotIron));
        addSawRecipe(new ItemStack(ProjectRed.itemGoldSaw), new ItemStack(Item.ingotGold));
        addSawRecipe(new ItemStack(ProjectRed.itemRubySaw), EnumPart.RUBY.getItemStack());
        addSawRecipe(new ItemStack(ProjectRed.itemSapphireSaw), EnumPart.SAPPHIRE.getItemStack());
        addSawRecipe(new ItemStack(ProjectRed.itemPeridotSaw), EnumPart.PERIDOT.getItemStack());
        addSawRecipe(new ItemStack(ProjectRed.itemDiamondSaw), new ItemStack(Item.diamond));
   
        /** Sickle **/
        addSickleRecipe(new ItemStack(ProjectRed.itemWoodSickle), new ItemStack(Block.planks));
        addSickleRecipe(new ItemStack(ProjectRed.itemStoneSickle), new ItemStack(Item.flint));
        addSickleRecipe(new ItemStack(ProjectRed.itemIronSickle), new ItemStack(Item.ingotIron));
        addSickleRecipe(new ItemStack(ProjectRed.itemGoldSickle), new ItemStack(Item.ingotGold));
        addSickleRecipe(new ItemStack(ProjectRed.itemRubySickle), EnumPart.RUBY.getItemStack());
        addSickleRecipe(new ItemStack(ProjectRed.itemSapphireSickle), EnumPart.SAPPHIRE.getItemStack());
        addSickleRecipe(new ItemStack(ProjectRed.itemPeridotSickle), EnumPart.PERIDOT.getItemStack());
        addSickleRecipe(new ItemStack(ProjectRed.itemDiamondSickle), new ItemStack(Item.diamond));

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
                'b', m
                ));
    }
    

    private static void initOtherRecipes() {
        /** Wool Gin to string recipe **/
        GameRegistry.addRecipe(new ItemStack(Item.silk, 4), 
                "gw",
                'g', new ItemStack(ProjectRed.itemWoolGin, 1, Short.MAX_VALUE),
                'w', Block.cloth
        );
    }
    
    private static void initToolRecipes() {
        /** Wool Gin **/
        GameRegistry.addRecipe(new ItemStack(ProjectRed.itemWoolGin), 
                "sis",
                "sss",
                " s ",
                's', Item.stick,
                'i', EnumPart.IRONCOIL.getItemStack()
        );
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
        FurnaceRecipes.smelting().addSmelting(ProjectRed.blockStones.blockID, EnumSpecialStone.BASALTCOBBLE.meta, EnumSpecialStone.BASALT.getItemStack(), 0);
    }


}
