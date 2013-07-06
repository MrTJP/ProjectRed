package mrtjp.projectred.multipart.wiring.gates;

import mrtjp.projectred.ProjectRed;
import net.minecraft.item.ItemStack;
import net.minecraftforge.oredict.OreDictionary;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public enum EnumGate {
	AND("AND gate", GateLogic.AND.class, GateRendering.AND.class),
	OR("OR gate", GateLogic.OR.class, GateRendering.OR.class),
	NOT("NOT gate", GateLogic.NOT.class, GateRendering.NOT.class),
	RSLATCH("RS-Latch", GateLogic.RSLatch.class, GateRendering.RSLatch.class),
	TOGGLE("T-FlipFlop", GateLogic.ToggleLatch.class, GateRendering.ToggleLatch.class),
	NOR("NOR gate", GateLogic.NOR.class, GateRendering.NOR.class),
	NAND("NAND gate", GateLogic.NAND.class, GateRendering.NAND.class),
	XOR("XOR gate", GateLogic.XOR.class, GateRendering.XOR.class),
	XNOR("XNOR gate", GateLogic.XNOR.class, GateRendering.XNOR.class),
	Buffer("Buffer gate", GateLogic.Buffer.class, GateRendering.Buffer.class),
	Multiplexer("Multiplexer", GateLogic.Multiplexer.class, GateRendering.Multiplexer.class),
	Repeater("Repeater", GateLogic.Repeater.class, GateRendering.Repeater.class),
	Timer("Timer", GateLogic.Timer.class, GateRendering.Timer.class),
	Counter("Counter", GateLogic.Counter.class, GateRendering.Counter.class),
	Sequencer("Sequencer", GateLogic.Sequencer.class, GateRendering.Sequencer.class),
	PulseFormer("Pulse Former", GateLogic.PulseFormer.class, GateRendering.PulseFormer.class),
	Randomizer("Randomizer", GateLogic.Randomizer.class, GateRendering.Randomizer.class),
	StateCell("State Cell", GateLogic.StateCell.class, GateRendering.StateCell.class),
	Synchronizer("Synchronizer", GateLogic.Synchronizer.class, GateRendering.Synchronizer.class),
	DLatch("D-Latch", GateLogic.DLatch.class, GateRendering.DLatch.class),
	DFlop("D-FlipFlop", GateLogic.DFlop.class, GateRendering.DFlop.class),
	BundledLatch("Bundled Latch", GateLogic.BundledLatch.class, GateRendering.BundledLatch.class),
	BundledRelay("Bundled Relay", GateLogic.BundledRelay.class, GateRendering.BundledRelay.class),
	BundledMultiplexer("Bundled Multiplexer", GateLogic.BundledMultiplexer.class, GateRendering.BundledMultiplexer.class),
	
	;
	
	private Class<? extends GateLogic> logicClass;
	private Class<? extends GateRendering> renderClass;
	private GateLogic logicInst;
	private GateRendering renderInst;
	public final String name;
	public int meta = this.ordinal();
	public static final String oreDictDefinition = "projredGate";

	
	private EnumGate(String name, Class<? extends GateLogic> logicClass, Class<? extends GateRendering> renderClass) {
		this.name = name;
		
		this.logicClass = logicClass;
		this.renderClass = renderClass;
		
		if(GateLogic.Stateless.class.isAssignableFrom(logicClass))
			logicInst = createLogic();
	}
	
	public GateLogic createLogic() {
		if(logicInst != null)
			return logicInst;
		try {
			return logicClass.getConstructor().newInstance();
		} catch(Exception e) {
			throw new RuntimeException(e);
		}
	}
	
	@SideOnly(Side.CLIENT)
	public GateRendering getRendering() {
		if(renderInst != null)
			return renderInst;
		try {
			renderInst = renderClass.getConstructor().newInstance(); 
		} catch(Exception e) {
			throw new RuntimeException(e);
		}
		return renderInst;
	}

	public Class<? extends GateLogic> getLogicClass() {
		return logicClass;
	}
	
	public static final EnumGate[] VALUES = values();
	public static EnumGate get(int ordinal) {
		if (ordinal > VALUES.length -1) {
			return null;
		}
		return VALUES[ordinal];
	}
	
	public ItemStack getItemStack() {
		return new ItemStack(ProjectRed.blockGate, 1, meta);
	}
	
	public static void initOreDictDefinitions() {
		for (EnumGate g : EnumGate.values()) {
			OreDictionary.registerOre(oreDictDefinition, g.getItemStack());
		}
	}
}
