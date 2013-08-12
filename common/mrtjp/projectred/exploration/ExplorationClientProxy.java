package mrtjp.projectred.exploration;

import static mrtjp.projectred.ProjectRed.*;
import static mrtjp.projectred.ProjectRed.itemWoolGin;
import mrtjp.projectred.exploration.BlockOre.EnumOre;
import mrtjp.projectred.exploration.BlockSpecialStone.EnumSpecialStone;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class ExplorationClientProxy extends ExplorationProxy {

    @Override
    public void preinit(){}
    
    @Override
    public void init() {
        LanguageRegistry.addName(itemWoolGin, "Wool Gin");
        
        for (EnumOre o : EnumOre.VALID_ORES) {
            LanguageRegistry.addName(o.getItemStack(1), o.name);
        }

        for (EnumSpecialStone s : EnumSpecialStone.VALID_STONE) {
            LanguageRegistry.addName(s.getItemStack(), s.name);
        }
        
        LanguageRegistry.addName(itemRubyAxe, itemRubyAxe.tool.name);
        LanguageRegistry.addName(itemSapphireAxe, itemSapphireAxe.tool.name);
        LanguageRegistry.addName(itemPeridotAxe, itemPeridotAxe.tool.name);
        LanguageRegistry.addName(itemRubyHoe, itemRubyHoe.tool.name);
        LanguageRegistry.addName(itemSapphireHoe, itemSapphireHoe.tool.name);
        LanguageRegistry.addName(itemPeridotHoe, itemPeridotHoe.tool.name);
        LanguageRegistry.addName(itemRubyPickaxe, itemRubyPickaxe.tool.name);
        LanguageRegistry.addName(itemSapphirePickaxe, itemSapphirePickaxe.tool.name);
        LanguageRegistry.addName(itemPeridotPickaxe, itemPeridotPickaxe.tool.name);
        LanguageRegistry.addName(itemRubyShovel, itemRubyShovel.tool.name);
        LanguageRegistry.addName(itemSapphireShovel, itemSapphireShovel.tool.name);
        LanguageRegistry.addName(itemPeridotShovel, itemPeridotShovel.tool.name);
        LanguageRegistry.addName(itemRubySword, itemRubySword.tool.name);
        LanguageRegistry.addName(itemSapphireSword, itemSapphireSword.tool.name);
        LanguageRegistry.addName(itemPeridotSword, itemPeridotSword.tool.name);
    }
    
    @Override
    public void postinit() {}
}
