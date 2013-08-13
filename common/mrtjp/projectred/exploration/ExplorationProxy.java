package mrtjp.projectred.exploration;

import static mrtjp.projectred.ProjectRed.blockOres;
import static mrtjp.projectred.ProjectRed.blockStones;
import static mrtjp.projectred.ProjectRed.itemDiamondSaw;
import static mrtjp.projectred.ProjectRed.itemDiamondSickle;
import static mrtjp.projectred.ProjectRed.itemGoldSaw;
import static mrtjp.projectred.ProjectRed.itemGoldSickle;
import static mrtjp.projectred.ProjectRed.itemIronSaw;
import static mrtjp.projectred.ProjectRed.itemIronSickle;
import static mrtjp.projectred.ProjectRed.itemPeridotAxe;
import static mrtjp.projectred.ProjectRed.itemPeridotHoe;
import static mrtjp.projectred.ProjectRed.itemPeridotPickaxe;
import static mrtjp.projectred.ProjectRed.itemPeridotSaw;
import static mrtjp.projectred.ProjectRed.itemPeridotShovel;
import static mrtjp.projectred.ProjectRed.itemPeridotSickle;
import static mrtjp.projectred.ProjectRed.itemPeridotSword;
import static mrtjp.projectred.ProjectRed.itemRubyAxe;
import static mrtjp.projectred.ProjectRed.itemRubyHoe;
import static mrtjp.projectred.ProjectRed.itemRubyPickaxe;
import static mrtjp.projectred.ProjectRed.itemRubySaw;
import static mrtjp.projectred.ProjectRed.itemRubyShovel;
import static mrtjp.projectred.ProjectRed.itemRubySickle;
import static mrtjp.projectred.ProjectRed.itemRubySword;
import static mrtjp.projectred.ProjectRed.itemSapphireAxe;
import static mrtjp.projectred.ProjectRed.itemSapphireHoe;
import static mrtjp.projectred.ProjectRed.itemSapphirePickaxe;
import static mrtjp.projectred.ProjectRed.itemSapphireSaw;
import static mrtjp.projectred.ProjectRed.itemSapphireShovel;
import static mrtjp.projectred.ProjectRed.itemSapphireSickle;
import static mrtjp.projectred.ProjectRed.itemSapphireSword;
import static mrtjp.projectred.ProjectRed.itemStoneSaw;
import static mrtjp.projectred.ProjectRed.itemStoneSickle;
import static mrtjp.projectred.ProjectRed.itemWoodSaw;
import static mrtjp.projectred.ProjectRed.itemWoodSickle;
import static mrtjp.projectred.ProjectRed.itemWoolGin;
import static mrtjp.projectred.ProjectRed.toolMaterialPeridot;
import static mrtjp.projectred.ProjectRed.toolMaterialRuby;
import static mrtjp.projectred.ProjectRed.toolMaterialSapphire;

import java.util.ArrayList;
import java.util.List;

import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.exploration.BlockOre.EnumOre;
import net.minecraft.block.Block;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.CraftingManager;
import net.minecraft.item.crafting.IRecipe;
import net.minecraft.util.Icon;
import net.minecraftforge.common.EnumHelper;
import net.minecraftforge.common.MinecraftForge;
import net.minecraftforge.oredict.ShapedOreRecipe;
import scala.collection.Seq;
import scala.collection.mutable.LinkedList;
import codechicken.microblock.BlockMicroMaterial;
import codechicken.microblock.ItemSaw;
import codechicken.microblock.MicroMaterialRegistry;
import codechicken.microblock.MicroMaterialRegistry.IMicroMaterial;
import cpw.mods.fml.common.registry.GameRegistry;

public class ExplorationProxy implements IProxy {

    @Override
    public void preinit() {
        GameRegistry.registerWorldGenerator(GenerationManager.instance);

        itemWoolGin = new ItemWoolGin(Configurator.item_woolginID.getInt());

        blockOres = new BlockOre(Configurator.block_oresID.getInt());
        GameRegistry.registerBlock(blockOres, ItemBlockOre.class, "projectred.exploration.ore");
        for (EnumOre o : EnumOre.VALID_ORES) {
            MinecraftForge.setBlockHarvestLevel(blockOres, "pickaxe", o.harvesLevel);
        }

        blockStones = new BlockSpecialStone(Configurator.block_stonesID.getInt());
        GameRegistry.registerBlock(blockStones, ItemBlockSpecialStone.class, "projectred.exploration.stone");

        toolMaterialRuby = EnumHelper.addToolMaterial("RUBY", 2, 500, 8.0F, 4, 12);
        toolMaterialSapphire = EnumHelper.addToolMaterial("SAPPHIRE", 2, 500, 8.0F, 3, 16);
        toolMaterialPeridot = EnumHelper.addToolMaterial("PERIDOT", 2, 500, 8.75F, 3.25F, 12);

        itemRubyAxe = new ItemGemAxe(Configurator.item_rubyAxe.getInt(), EnumSpecialTool.RUBYAXE);
        itemSapphireAxe = new ItemGemAxe(Configurator.item_sapphireAxe.getInt(), EnumSpecialTool.SAPPHIREAXE);
        itemPeridotAxe = new ItemGemAxe(Configurator.item_peridotAxe.getInt(), EnumSpecialTool.PERIDOTAXE);
        MinecraftForge.setToolClass(itemRubyAxe, "axe", 2);
        MinecraftForge.setToolClass(itemSapphireAxe, "axe", 2);
        MinecraftForge.setToolClass(itemPeridotAxe, "axe", 2);

        itemRubyHoe = new ItemGemHoe(Configurator.item_rubyHoe.getInt(), EnumSpecialTool.RUBYHOE);
        itemSapphireHoe = new ItemGemHoe(Configurator.item_sapphireHoe.getInt(), EnumSpecialTool.SAPPHIREHOE);
        itemPeridotHoe = new ItemGemHoe(Configurator.item_peridotHoe.getInt(), EnumSpecialTool.PERIDOTHOE);
        MinecraftForge.setToolClass(itemRubyHoe, "hoe", 2);
        MinecraftForge.setToolClass(itemSapphireHoe, "hoe", 2);
        MinecraftForge.setToolClass(itemPeridotHoe, "hoe", 2);

        itemRubyPickaxe = new ItemGemPickaxe(Configurator.item_rubyPickaxe.getInt(), EnumSpecialTool.RUBYPICKAXE);
        itemSapphirePickaxe = new ItemGemPickaxe(Configurator.item_sapphirePickaxe.getInt(), EnumSpecialTool.SAPPHIREPICKAXE);
        itemPeridotPickaxe = new ItemGemPickaxe(Configurator.item_peridotPickaxe.getInt(), EnumSpecialTool.PERIDOTPICKAXE);
        MinecraftForge.setToolClass(itemRubyPickaxe, "pickaxe", 2);
        MinecraftForge.setToolClass(itemSapphirePickaxe, "pickaxe", 2);
        MinecraftForge.setToolClass(itemPeridotPickaxe, "pickaxe", 2);

        itemRubyShovel = new ItemGemShovel(Configurator.item_rubyShovel.getInt(), EnumSpecialTool.RUBYSHOVEL);
        itemSapphireShovel = new ItemGemShovel(Configurator.item_sapphireShovel.getInt(), EnumSpecialTool.SAPPHIRESHOVEL);
        itemPeridotShovel = new ItemGemShovel(Configurator.item_peridotShovel.getInt(), EnumSpecialTool.PERIDOTSHOVEL);
        MinecraftForge.setToolClass(itemRubyShovel, "shovel", 2);
        MinecraftForge.setToolClass(itemSapphireShovel, "shovel", 2);
        MinecraftForge.setToolClass(itemPeridotShovel, "shovel", 2);

        itemRubySword = new ItemGemSword(Configurator.item_rubySword.getInt(), EnumSpecialTool.RUBYSWORD);
        itemSapphireSword = new ItemGemSword(Configurator.item_sapphireSword.getInt(), EnumSpecialTool.SAPPHIRESWORD);
        itemPeridotSword = new ItemGemSword(Configurator.item_peridotSword.getInt(), EnumSpecialTool.PERIDOTSWORD);
        MinecraftForge.setToolClass(itemRubySword, "sword", 2);
        MinecraftForge.setToolClass(itemSapphireSword, "sword", 2);
        MinecraftForge.setToolClass(itemPeridotSword, "sword", 2);

        itemWoodSaw = new ItemGemSaw(Configurator.item_woodSaw.getInt(), EnumSpecialTool.WOODSAW);
        itemStoneSaw = new ItemGemSaw(Configurator.item_stoneSaw.getInt(), EnumSpecialTool.STONESAW);
        itemIronSaw = new ItemGemSaw(Configurator.item_ironSaw.getInt(), EnumSpecialTool.IRONSAW);
        itemGoldSaw = new ItemGemSaw(Configurator.item_goldSaw.getInt(), EnumSpecialTool.GOLDSAW);
        itemRubySaw = new ItemGemSaw(Configurator.item_rubySaw.getInt(), EnumSpecialTool.RUBYSAW);
        itemSapphireSaw = new ItemGemSaw(Configurator.item_sapphireSaw.getInt(), EnumSpecialTool.SAPPHIRESAW);
        itemPeridotSaw = new ItemGemSaw(Configurator.item_peridotSaw.getInt(), EnumSpecialTool.PERIDOTSAW);
        itemDiamondSaw = new ItemGemSaw(Configurator.item_diamondSaw.getInt(), EnumSpecialTool.DIAMONDSAW);

        itemWoodSickle = new ItemGemSickle(Configurator.item_woodSickle.getInt(), EnumSpecialTool.WOODSICKLE);
        itemStoneSickle = new ItemGemSickle(Configurator.item_stoneSickle.getInt(), EnumSpecialTool.STONESICKLE);
        itemIronSickle = new ItemGemSickle(Configurator.item_ironSickle.getInt(), EnumSpecialTool.IRONSICKLE);
        itemGoldSickle = new ItemGemSickle(Configurator.item_goldSickle.getInt(), EnumSpecialTool.GOLDSICKLE);
        itemRubySickle = new ItemGemSickle(Configurator.item_rubySickle.getInt(), EnumSpecialTool.RUBYSICKLE);
        itemSapphireSickle = new ItemGemSickle(Configurator.item_sapphireSickle.getInt(), EnumSpecialTool.SAPPHIRESICKLE);
        itemPeridotSickle = new ItemGemSickle(Configurator.item_peridotSickle.getInt(), EnumSpecialTool.PERIDOTSICKLE);
        itemDiamondSickle = new ItemGemSickle(Configurator.item_diamondSickle.getInt(), EnumSpecialTool.DIAMONDSICKLE);
    }

    @Override
    public void init() {
        if (Configurator.gen_SpreadingMoss.getBoolean(true)) {
            int mc = Block.cobblestoneMossy.blockID;
            Block.blocksList[mc] = null;
            new BlockPhotosyntheticCobblestone(mc);

            int sb = Block.stoneBrick.blockID;
            Block.blocksList[sb] = null;
            new BlockPhotosyntheticStoneBrick(sb);
        }

        ExplorationRecipes.initRecipes();
    }

    @Override
    public void postinit() {
        // Remove default saw recipes
        ArrayList recipes = (ArrayList) CraftingManager.getInstance().getRecipeList();
        for (int i = 0; i < recipes.size(); i++) {
            IRecipe r = (IRecipe) recipes.get(i);
            if (r instanceof ShapedOreRecipe) {
                ItemStack result = ((ShapedOreRecipe) r).getRecipeOutput();
                if (result.getItem() instanceof ItemSaw) {
                    recipes.remove(i);
                }
            }
        }
    }

}
