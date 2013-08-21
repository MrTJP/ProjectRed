package mrtjp.projectred.integration2;

import mrtjp.projectred.ProjectRedIntegration;
import net.minecraft.item.ItemStack;

public enum EnumGate
{
    OR("OR Gate", "pr_sgate"),
    NOR("NOR Gate", "pr_sgate"),
    ;

    public static EnumGate[] VALID_GATES = values();
    
    public String name;
    public String gateType;
    public int meta = this.ordinal();
    
    private EnumGate(String name, String gateClass) {
        this.name = name;
        this.gateType = gateClass;
    }
    
    public ItemStack getItemStack() {
        return new ItemStack(ProjectRedIntegration.itemPartGate, 1, meta);
    }
}
