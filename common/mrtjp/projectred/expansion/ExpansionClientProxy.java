package mrtjp.projectred.expansion;

import static mrtjp.projectred.ProjectRedExpansion.itemPartTube;
import static mrtjp.projectred.ProjectRedExpansion.itemVAWT;
import net.minecraft.item.ItemStack;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class ExpansionClientProxy extends ExpansionProxy {
    
    @Override
    public void init() {
        super.init();
    }

    @Override
    public void postinit() {
        super.postinit();
        ExpansionRecipes.initRecipes();
    }
}
