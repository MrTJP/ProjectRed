package mrtjp.projectred.integration2;

import mrtjp.projectred.ProjectRedIntegration;
import net.minecraft.item.ItemStack;

public enum EnumGate
{
    OR("OR Gate", "pr_sgate"),
    NOR("NOR Gate", "pr_sgate"),
    NOT("NOT Gate", "pr_sgate"),
    AND("AND Gate", "pr_sgate"),
    NAND("NAND Gate", "pr_sgate"),
    XOR("XOR Gate", "pr_sgate"),
    XNOR("XNOR Gate", "pr_sgate"),
    Buffer("Buffer Gate", "pr_sgate"),
    Multiplexer(null, "pr_sgate"),
    Pulse("Pulse Former", "pr_sgate"),
    Repeater("Repeater", "pr_sgate"),
    Randomizer("Randomizer", "pr_sgate"),
    NullCell("Null Cell", "pr_sgate"),
    InvertCell(null, "pr_sgate"),
    NonInvertCell(null, "pr_sgate"),
    RSLatch(null, "pr_sgate"),
    ToggleLatch(null, "pr_sgate"),
    TransparentLatch("Transparent Latch", "pr_sgate")
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
        return new ItemStack(ProjectRedIntegration.itemPartGate2, 1, meta);
    }
}
