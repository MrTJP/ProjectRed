package mrtjp.projectred.renderstuffs;

import java.util.Map;

import mrtjp.projectred.multipart.wiring.wires.TileWire;
import mrtjp.projectred.utils.codechicken.core.render.CCModel;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.util.Icon;

public class WireRenderAssistant2 {
	public double x;
	public double y;
	public double z;
	public int side;
	
	public float xOffset = 0;
	public float yOffset = 0;
	public float zOffset = 0;
	
	public RenderBlocks renderBlocks;
	
	public Map<String, CCModel> wireMap;
	public Icon wireIcon;

	public boolean isCenterCrossed;
	public boolean isCenterNS;
	public boolean isCenterWE;

	public boolean connectsInsideConnectorN;
	public boolean connectsInsideConnectorS;
	public boolean connectsInsideConnectorW;
	public boolean connectsInsideConnectorE;
	
	public boolean connectsN;
	public boolean connectsS;
	public boolean connectsW;
	public boolean connectsE;

	public boolean connectsInsideN;
	public boolean connectsInsideS;
	public boolean connectsInsideW;
	public boolean connectsInsideE;

	public boolean connectsCornerN;
	public boolean connectsCornerS;
	public boolean connectsCornerW;
	public boolean connectsCornerE;

	public WireRenderAssistant2() {
	}
	
	public void setRenderForWire(TileWire tw) {
		
	}

}
