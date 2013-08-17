package mrtjp.projectred.exploration;

import mrtjp.projectred.core.ProjectRedTabs;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.item.ItemSpade;
import net.minecraft.item.ItemStack;

public class ItemGemShovel extends ItemSpade {

    EnumSpecialTool tool;

    public ItemGemShovel(int par1, EnumSpecialTool tool) {
        super(par1, tool.material);
        this.tool = tool;
        this.setUnlocalizedName("projectred.exploration." + tool.unlocal);
        this.setCreativeTab(ProjectRedTabs.tabExploration);
    }

    @Override
    public boolean getIsRepairable(ItemStack ist1, ItemStack ist2) {
        if (tool.repairStack.isItemEqual(ist2)) {
            return true;
        }
        return super.getIsRepairable(ist1, ist2);
    }
    
    @Override
    public void registerIcons(IconRegister reg) {
        this.itemIcon = reg.registerIcon("projectred:gemtools/" + tool.unlocal);
    }
}
