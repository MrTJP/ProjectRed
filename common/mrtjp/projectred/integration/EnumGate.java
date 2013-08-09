package mrtjp.projectred.integration;

import mrtjp.projectred.ProjectRed;
import net.minecraft.item.ItemStack;
import net.minecraftforge.oredict.OreDictionary;

public enum EnumGate {
    AND("AND gate", GateLogic.AND.class, GateRenderBridge.AND.class),
    OR("OR gate", GateLogic.OR.class, GateRenderBridge.OR.class),
    NOT("NOT gate", GateLogic.NOT.class, GateRenderBridge.NOT.class),
    RSLATCH("RS-Latch", GateLogic.RSLatch.class, GateRenderBridge.RSLatch.class),
    TOGGLE("T-FlipFlop", GateLogic.ToggleLatch.class, GateRenderBridge.ToggleLatch.class),
    NOR("NOR gate", GateLogic.NOR.class, GateRenderBridge.NOR.class),
    NAND("NAND gate", GateLogic.NAND.class, GateRenderBridge.NAND.class),
    XOR("XOR gate", GateLogic.XOR.class, GateRenderBridge.XOR.class),
    XNOR("XNOR gate", GateLogic.XNOR.class, GateRenderBridge.XNOR.class),
    Buffer("Buffer gate", GateLogic.Buffer.class, GateRenderBridge.Buffer.class),
    Multiplexer("Multiplexer", GateLogic.Multiplexer.class, GateRenderBridge.Multiplexer.class),
    Repeater("Repeater", GateLogic.Repeater.class, GateRenderBridge.Repeater.class),
    Timer("Timer", GateLogic.Timer.class, GateRenderBridge.Timer.class),
    Counter("Counter", GateLogic.Counter.class, GateRenderBridge.Counter.class),
    Sequencer("Sequencer", GateLogic.Sequencer.class, GateRenderBridge.Sequencer.class),
    PulseFormer("Pulse Former", GateLogic.PulseFormer.class, GateRenderBridge.PulseFormer.class),
    Randomizer("Randomizer", GateLogic.Randomizer.class, GateRenderBridge.Randomizer.class),
    StateCell("State Cell", GateLogic.StateCell.class, GateRenderBridge.StateCell.class),
    Synchronizer("Synchronizer", GateLogic.Synchronizer.class, GateRenderBridge.Synchronizer.class),
    DLatch("D-Latch", GateLogic.DLatch.class, GateRenderBridge.DLatch.class),
    DFlop("D-FlipFlop", GateLogic.DFlop.class, GateRenderBridge.DFlop.class),
    BundledLatch("Bundled Latch", GateLogic.BundledLatch.class, GateRenderBridge.BundledLatch.class),
    BundledRelay("Bundled Relay", GateLogic.BundledRelay.class, GateRenderBridge.BundledRelay.class),
    BundledMultiplexer("Bundled Multiplexer", GateLogic.BundledMultiplexer.class, GateRenderBridge.BundledMultiplexer.class),
    LightSensor("Light Sensor", GateLogic.LightSensor.class, GateRenderBridge.LightSensor.class),
    RainSensor("Rain Sensor", GateLogic.RainSensor.class, GateRenderBridge.RainSensor.class),
    IOExpander("CC I/O Expander", GatePeripheralPart.class, GateLogic.CCIOExpander.class, GateRenderBridge.CCIOExpander.class)
    ;

    public static final EnumGate[] VALID_GATES = values();

    private final Class<? extends GateLogic> logicClass;
    private final Class<? extends GateRenderBridge> renderClass;
    private final Class<? extends GatePart> gateClass;

    public final String name;
    public int meta = this.ordinal();
    public static final String oreDictDefinition = "projredGate";

    private EnumGate(String name, Class<? extends GateLogic> logicClass, Class<? extends GateRenderBridge> renderClass) {
        this(name, GatePart.class, logicClass, renderClass);
    }
    
    private EnumGate(String name, Class<? extends GatePart> partClass, Class<? extends GateLogic> logicClass, Class<? extends GateRenderBridge> renderClass) {
        this.name = name;
        this.gateClass = partClass;
        this.logicClass = logicClass;
        this.renderClass = renderClass;
    }

    public GatePart createPart() {
        try {
            return (GatePart)gateClass.getConstructors()[0].newInstance(this);
        } catch (Throwable e) {
            throw new RuntimeException(e);
        }
    }
    
    public GateLogic createLogic() {
        try {
            return logicClass.getConstructor().newInstance();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public GateRenderBridge createRenderBridge() {
        try {
            return renderClass.getConstructor().newInstance();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
    
    public Class<? extends GateLogic> getLogicClass() {
        return logicClass;
    }


    public static EnumGate get(int ordinal) {
        if (ordinal > VALID_GATES.length - 1) {
            return null;
        }
        return VALID_GATES[ordinal];
    }

    public static EnumGate getByName(String name) {
        for (EnumGate g : VALID_GATES) {
            if (g.name.matches(name)) {
                return g;
            }
        }
        return null;
    }

    public ItemStack getItemStack() {
        return new ItemStack(ProjectRed.itemPartGate, 1, meta);
    }

    public static void initOreDictDefinitions() {
        for (EnumGate g : EnumGate.values()) {
            OreDictionary.registerOre(oreDictDefinition, g.getItemStack());
        }
    }
}
