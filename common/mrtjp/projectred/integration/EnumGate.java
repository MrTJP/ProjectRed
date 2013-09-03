package mrtjp.projectred.integration;

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
    Multiplexer("Multiplexer", "pr_sgate"),
    Pulse("Pulse Former", "pr_sgate"),
    Repeater("Repeater", "pr_sgate"),
    Randomizer("Randomizer", "pr_sgate"),
    RSLatch("RS Latch", "pr_igate"),
    ToggleLatch("Toggle Latch", "pr_igate"),
    TransparentLatch("Transparent Latch", "pr_sgate"),
    LightSensor("Light Sensor", "pr_sgate"),
    RainSensor("Rain Sensor", "pr_sgate"),
    Timer("Timer", "pr_igate"),
    Sequencer("Sequencer", "pr_igate"),
    Counter("Counter", "pr_igate"),
    StateCell("State Cell", "pr_igate"),
    Synchronizer("Synchronizer", "pr_igate"),
    BusTransceiver("Bus Transceiver", "pr_bgate"),
    NullCell("Null Cell", "pr_agate"),
    InvertCell("Invert Cell", "pr_agate"),
    BufferCell("Buffer Cell", "pr_agate"),
    Comparator("Comparator", "pr_igate")
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
