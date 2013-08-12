package mrtjp.projectred.exploration;

import static mrtjp.projectred.ProjectRed.blockOres;
import static mrtjp.projectred.ProjectRed.blockStones;
import static mrtjp.projectred.ProjectRed.itemPeridotAxe;
import static mrtjp.projectred.ProjectRed.itemPeridotHoe;
import static mrtjp.projectred.ProjectRed.itemPeridotPickaxe;
import static mrtjp.projectred.ProjectRed.itemPeridotShovel;
import static mrtjp.projectred.ProjectRed.itemPeridotSword;
import static mrtjp.projectred.ProjectRed.itemRubyAxe;
import static mrtjp.projectred.ProjectRed.itemRubyHoe;
import static mrtjp.projectred.ProjectRed.itemRubyPickaxe;
import static mrtjp.projectred.ProjectRed.itemRubyShovel;
import static mrtjp.projectred.ProjectRed.itemRubySword;
import static mrtjp.projectred.ProjectRed.itemSapphireAxe;
import static mrtjp.projectred.ProjectRed.itemSapphireHoe;
import static mrtjp.projectred.ProjectRed.itemSapphirePickaxe;
import static mrtjp.projectred.ProjectRed.itemSapphireShovel;
import static mrtjp.projectred.ProjectRed.itemSapphireSword;
import static mrtjp.projectred.ProjectRed.itemWoolGin;
import static mrtjp.projectred.ProjectRed.toolMaterialPeridot;
import static mrtjp.projectred.ProjectRed.toolMaterialRuby;
import static mrtjp.projectred.ProjectRed.toolMaterialSapphire;
import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.IProxy;
import mrtjp.projectred.exploration.BlockOre.EnumOre;
import net.minecraft.block.Block;
import net.minecraftforge.common.EnumHelper;
import net.minecraftforge.common.MinecraftForge;
import cpw.mods.fml.common.registry.GameRegistry;

public class ExplorationProxy implements IProxy {

    @Override
    public void preinit() {
    }

    @Override
    public void init() {
        GameRegistry.registerWorldGenerator(GenerationManager.instance);

        itemWoolGin = new ItemWoolGin(Configurator.item_woolginID.getInt());

        blockOres = new BlockOre(Configurator.block_oresID.getInt());
        GameRegistry.registerBlock(blockOres, ItemBlockOre.class, "projectred.exploration.ore");
        for (EnumOre o : EnumOre.VALID_ORES) {
            MinecraftForge.setBlockHarvestLevel(blockOres, "pickaxe", o.harvesLevel);
        }

        blockStones = new BlockSpecialStone(Configurator.block_stonesID.getInt());
        GameRegistry.registerBlock(blockStones, ItemBlockSpecialStone.class, "projectred.exploration.stone");

        if (Configurator.gen_SpreadingMoss.getBoolean(true)) {
            int mc = Block.cobblestoneMossy.blockID;
            Block.blocksList[mc] = null;
            new BlockPhotosyntheticCobblestone(mc);

            int sb = Block.stoneBrick.blockID;
            Block.blocksList[sb] = null;
            new BlockPhotosyntheticStoneBrick(sb);
        }

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
        
        ExplorationRecipes.initRecipes();
    }

    @Override
    public void postinit() {
    }

}
