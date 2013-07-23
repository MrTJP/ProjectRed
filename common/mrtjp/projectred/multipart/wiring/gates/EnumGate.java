package mrtjp.projectred.multipart.wiring.gates;

import java.util.HashMap;
import java.util.Map;

import mrtjp.projectred.ProjectRed;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import net.minecraftforge.oredict.OreDictionary;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public enum EnumGate {
	AND("AND gate", 4, GateLogic.AND.class, GateRenderBridge.AND.class),
	OR("OR gate", 2, GateLogic.OR.class, GateRenderBridge.OR.class),
	NOT("NOT gate", 1, GateLogic.NOT.class, GateRenderBridge.NOT.class),
	RSLATCH("RS-Latch", 2, GateLogic.RSLatch.class, GateRenderBridge.RSLatch.class),
	TOGGLE("T-FlipFlop", 2, GateLogic.ToggleLatch.class, GateRenderBridge.ToggleLatch.class),
	NOR("NOR gate", 1, GateLogic.NOR.class, GateRenderBridge.NOR.class),
	NAND("NAND gate", 3, GateLogic.NAND.class, GateRenderBridge.NAND.class),
	XOR("XOR gate", 3, GateLogic.XOR.class, GateRenderBridge.XOR.class),
	XNOR("XNOR gate", 4, GateLogic.XNOR.class, GateRenderBridge.XNOR.class),
	Buffer("Buffer gate", 2, GateLogic.Buffer.class, GateRenderBridge.Buffer.class),
	Multiplexer("Multiplexer", 4, GateLogic.Multiplexer.class, GateRenderBridge.Multiplexer.class),
	Repeater("Repeater", 2, GateLogic.Repeater.class, GateRenderBridge.Repeater.class),
	Timer("Timer", 2, GateLogic.Timer.class, GateRenderBridge.Timer.class),
	Counter("Counter", 3, GateLogic.Counter.class, GateRenderBridge.Counter.class),
	Sequencer("Sequencer", 5, GateLogic.Sequencer.class, GateRenderBridge.Sequencer.class),
	PulseFormer("Pulse Former", 3, GateLogic.PulseFormer.class, GateRenderBridge.PulseFormer.class),
	Randomizer("Randomizer", 0, GateLogic.Randomizer.class, GateRenderBridge.Randomizer.class),
	StateCell("State Cell", 2, GateLogic.StateCell.class, GateRenderBridge.StateCell.class),
	Synchronizer("Synchronizer", 1, GateLogic.Synchronizer.class, GateRenderBridge.Synchronizer.class),
	DLatch("D-Latch", 0, GateLogic.DLatch.class, GateRenderBridge.DLatch.class),
	DFlop("D-FlipFlop", 0, GateLogic.DFlop.class, GateRenderBridge.DFlop.class),
	BundledLatch("Bundled Latch", 0, GateLogic.BundledLatch.class, GateRenderBridge.BundledLatch.class),
	BundledRelay("Bundled Relay", 0, GateLogic.BundledRelay.class, GateRenderBridge.BundledRelay.class),
	BundledMultiplexer("Bundled Multiplexer", 0, GateLogic.BundledMultiplexer.class, GateRenderBridge.BundledMultiplexer.class),
	LightSensor("Light Sensor", 0, GateLogic.LightSensor.class, GateRenderBridge.LightSensor.class),
	RainSensor("Rain Sensor", 0, GateLogic.RainSensor.class, GateRenderBridge.RainSensor.class)
	;
	
	private Class<? extends GateLogic> logicClass;
	private Class<? extends GateRenderBridge> renderClass;
	private GateLogic logicInst;
	private GateRenderBridge renderInst;
	private int lightCount;
	public final String name;
	public int meta = this.ordinal();
	public static final String oreDictDefinition = "projredGate";
	
	private EnumGate(String name, int lights, Class<? extends GateLogic> logicClass, Class<? extends GateRenderBridge> renderClass) {
		this.name = name;
		this.lightCount = lights;
		
		this.logicClass = logicClass;
		this.renderClass = renderClass;
		
		if(GateLogic.Stateless.class.isAssignableFrom(logicClass))
			logicInst = createLogic();
	}
		
	public GateLogic createLogic() {
		if(logicInst != null) {
			return logicInst;
		}
		try {
			return logicClass.getConstructor().newInstance();
		} catch(Exception e) {
			throw new RuntimeException(e);
		}
	}
	
	public int getLightCount() {
		return lightCount;
	}
	
	@SideOnly(Side.CLIENT)
	public GateRenderBridge getRendering() {
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
