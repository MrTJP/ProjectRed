package mrtjp.projectred.integration2;

import mrtjp.projectred.ProjectRedIntegration;
import net.minecraft.item.ItemStack;

public enum EnumGate
{
    OR("pr_sgate");

    public static EnumGate[] VALID_GATES = values();
    
    public String gateClass;
    public int meta = this.ordinal();
    
    private EnumGate(String gateClass) {
        this.gateClass = gateClass;
    }
    
    public ItemStack getItem() {
        return new ItemStack(ProjectRedIntegration.itemPartGate, 1, meta);
    }
}
