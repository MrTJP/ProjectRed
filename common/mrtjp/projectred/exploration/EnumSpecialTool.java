package mrtjp.projectred.exploration;

import mrtjp.projectred.ProjectRed;
import mrtjp.projectred.core.ItemPart.EnumPart;
import net.minecraft.item.EnumToolMaterial;
import net.minecraft.item.ItemStack;

public enum EnumSpecialTool {
    RUBYAXE("Ruby Axe", "axeruby", ProjectRed.toolMaterialRuby, EnumPart.RUBY.getItemStack()),
    SAPPHIREAXE("Sapphire Axe", "axesapphire", ProjectRed.toolMaterialSapphire, EnumPart.SAPPHIRE.getItemStack()),
    PERIDOTAXE("Peridot Axe", "axeperidot", ProjectRed.toolMaterialPeridot, EnumPart.PERIDOT.getItemStack()),
   
    RUBYPICKAXE("Ruby Pickaxe", "pickaxeruby", ProjectRed.toolMaterialRuby, EnumPart.RUBY.getItemStack()),
    SAPPHIREPICKAXE("Sapphire Pickaxe", "pickaxesapphire", ProjectRed.toolMaterialSapphire, EnumPart.SAPPHIRE.getItemStack()),
    PERIDOTPICKAXE("Peridot Pickaxe", "pickaxeperidot", ProjectRed.toolMaterialPeridot, EnumPart.PERIDOT.getItemStack()),
    
    RUBYSHOVEL("Ruby Shovel", "shovelruby", ProjectRed.toolMaterialRuby, EnumPart.RUBY.getItemStack()),
    SAPPHIRESHOVEL("Sapphire Shovel", "shovelsapphire", ProjectRed.toolMaterialSapphire, EnumPart.SAPPHIRE.getItemStack()),
    PERIDOTSHOVEL("Peridot Shovel", "shovelperidot", ProjectRed.toolMaterialPeridot, EnumPart.PERIDOT.getItemStack()),
    
    RUBYSWORD("Ruby Sword", "swordruby", ProjectRed.toolMaterialRuby, EnumPart.RUBY.getItemStack()),
    SAPPHIRESWORD("Sapphire Sword", "swordsapphire", ProjectRed.toolMaterialSapphire, EnumPart.SAPPHIRE.getItemStack()),
    PERIDOTSWORD("Peridot Sword", "swordperidot", ProjectRed.toolMaterialPeridot, EnumPart.PERIDOT.getItemStack()),
   
    RUBYHOE("Ruby Hoe", "hoeruby", ProjectRed.toolMaterialRuby, EnumPart.RUBY.getItemStack()),
    SAPPHIREHOE("Sapphire Hoe", "hoesapphire", ProjectRed.toolMaterialSapphire, EnumPart.SAPPHIRE.getItemStack()),
    PERIDOTHOE("Peridot Hoe", "hoeperidot", ProjectRed.toolMaterialPeridot, EnumPart.PERIDOT.getItemStack()),
    ;
    
    public final EnumToolMaterial material;
    public final String name;
    public final String unlocal;
    
    public final ItemStack repairStack;
    
    private EnumSpecialTool(String name, String unlocal, EnumToolMaterial material, ItemStack repair) {
        this.name = name;
        this.unlocal = unlocal;
        this.material = material;
        this.repairStack = repair;
    }
}
