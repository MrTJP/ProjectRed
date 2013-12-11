package mrtjp.projectred.exploration;

import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.ItemPart.EnumPart;
import net.minecraft.block.Block;
import net.minecraft.item.EnumToolMaterial;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;

public enum EnumSpecialTool
{
    RUBYAXE("Ruby Axe", "axeruby", ProjectRedExploration.toolMaterialRuby, EnumPart.RUBY.getItemStack()),
    SAPPHIREAXE("Sapphire Axe", "axesapphire", ProjectRedExploration.toolMaterialSapphire, EnumPart.SAPPHIRE.getItemStack()),
    PERIDOTAXE("Peridot Axe", "axeperidot", ProjectRedExploration.toolMaterialPeridot, EnumPart.PERIDOT.getItemStack()),

    RUBYPICKAXE("Ruby Pickaxe", "pickaxeruby", ProjectRedExploration.toolMaterialRuby, EnumPart.RUBY.getItemStack()),
    SAPPHIREPICKAXE("Sapphire Pickaxe", "pickaxesapphire", ProjectRedExploration.toolMaterialSapphire, EnumPart.SAPPHIRE.getItemStack()),
    PERIDOTPICKAXE("Peridot Pickaxe", "pickaxeperidot", ProjectRedExploration.toolMaterialPeridot, EnumPart.PERIDOT.getItemStack()),

    RUBYSHOVEL("Ruby Shovel", "shovelruby", ProjectRedExploration.toolMaterialRuby, EnumPart.RUBY.getItemStack()),
    SAPPHIRESHOVEL("Sapphire Shovel", "shovelsapphire", ProjectRedExploration.toolMaterialSapphire, EnumPart.SAPPHIRE.getItemStack()),
    PERIDOTSHOVEL("Peridot Shovel", "shovelperidot", ProjectRedExploration.toolMaterialPeridot, EnumPart.PERIDOT.getItemStack()),

    RUBYSWORD("Ruby Sword", "swordruby", ProjectRedExploration.toolMaterialRuby, EnumPart.RUBY.getItemStack()),
    SAPPHIRESWORD("Sapphire Sword", "swordsapphire", ProjectRedExploration.toolMaterialSapphire, EnumPart.SAPPHIRE.getItemStack()),
    PERIDOTSWORD("Peridot Sword", "swordperidot", ProjectRedExploration.toolMaterialPeridot, EnumPart.PERIDOT.getItemStack()),

    RUBYHOE("Ruby Hoe", "hoeruby", ProjectRedExploration.toolMaterialRuby, EnumPart.RUBY.getItemStack()),
    SAPPHIREHOE("Sapphire Hoe", "hoesapphire", ProjectRedExploration.toolMaterialSapphire, EnumPart.SAPPHIRE.getItemStack()),
    PERIDOTHOE("Peridot Hoe", "hoeperidot", ProjectRedExploration.toolMaterialPeridot, EnumPart.PERIDOT.getItemStack()),

    WOODSAW("Wood Saw", "sawwood", EnumToolMaterial.WOOD, new ItemStack(Block.planks)),
    STONESAW("Stone Saw", "sawstone", EnumToolMaterial.STONE, new ItemStack(Item.flint)),
    IRONSAW("Iron Saw", "sawiron", EnumToolMaterial.IRON, new ItemStack(Item.ingotIron)),
    GOLDSAW("Gold Saw", "sawgold", EnumToolMaterial.GOLD, new ItemStack(Item.ingotGold)),
    RUBYSAW("Ruby Saw", "sawruby", ProjectRedExploration.toolMaterialRuby, EnumPart.RUBY.getItemStack()),
    SAPPHIRESAW("Sapphire Saw", "sawsapphire", ProjectRedExploration.toolMaterialSapphire, EnumPart.SAPPHIRE.getItemStack()),
    PERIDOTSAW("Peridot Saw", "sawperidot", ProjectRedExploration.toolMaterialPeridot, EnumPart.PERIDOT.getItemStack()),
    DIAMONDSAW("Diamond Saw", "sawdiamond", EnumToolMaterial.EMERALD, new ItemStack(Item.diamond)),

    WOODSICKLE("Wood Sickle", "sicklewood", EnumToolMaterial.WOOD, new ItemStack(Block.planks)),
    STONESICKLE("Stone Sickle", "sicklestone", EnumToolMaterial.STONE, new ItemStack(Item.flint)),
    IRONSICKLE("Iron Sickle", "sickleiron", EnumToolMaterial.IRON, new ItemStack(Item.ingotIron)),
    GOLDSICKLE("Gold Sickle", "sicklegold", EnumToolMaterial.GOLD, new ItemStack(Item.ingotGold)),
    RUBYSICKLE("Ruby Sickle", "sickleruby", ProjectRedExploration.toolMaterialRuby, EnumPart.RUBY.getItemStack()),
    SAPPHIRESICKLE("Sapphire Sickle", "sicklesapphire", ProjectRedExploration.toolMaterialSapphire, EnumPart.SAPPHIRE.getItemStack()),
    PERIDOTSICKLE("Peridot Sickle", "sickleperidot", ProjectRedExploration.toolMaterialPeridot, EnumPart.PERIDOT.getItemStack()),
    DIAMONDSICKLE("Diamond Sickle", "sicklediamond", EnumToolMaterial.EMERALD, new ItemStack(Item.diamond)), ;

    public final EnumToolMaterial material;
    public final String name;
    public final String unlocal;

    public final ItemStack repairStack;

    private EnumSpecialTool(String name, String unlocal, EnumToolMaterial material, ItemStack repair)
    {
        this.name = name;
        this.unlocal = unlocal;
        this.material = material;
        this.repairStack = repair;
    }
}
