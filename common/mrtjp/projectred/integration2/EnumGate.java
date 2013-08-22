package mrtjp.projectred.integration2;

import mrtjp.projectred.ProjectRedIntegration;
import net.minecraft.item.ItemStack;

public enum EnumGate
{
    OR("OR Gate", "pr_sgate"),
    NOR(null, "pr_sgate"),
    NOT("NOT Gate", "pr_sgate"),
    AND(null, "pr_sgate"),
    NAND(null, "pr_sgate"),
    XOR(null, "pr_sgate"),
    XNOR(null, "pr_sgate"),
    Buffer(null, "pr_sgate"),
    Multiplexer(null, "pr_sgate"),
    Pulse("Pulse Former", "pr_sgate")
    ;

    public static EnumGate[] VALID_GATES = values();
    
    public String name;
    public String gateType;
    public int meta = this.ordinal();
    
    private EnumGate(String name, String gateClass) {
        this.name = name;
        this.gateType = gateClass;
    }
    
    public boolean implemented() {
        return name != null;
    }
    
    public ItemStack getItemStack() {
        return new ItemStack(ProjectRedIntegration.itemPartGate, 1, meta);
    }
}
