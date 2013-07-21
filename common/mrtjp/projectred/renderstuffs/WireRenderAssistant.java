package mrtjp.projectred.renderstuffs;

import java.util.Map;

import mrtjp.projectred.multipart.wiring.wires.TileWire;
import mrtjp.projectred.utils.codechicken.core.render.CCModel;
import mrtjp.projectred.utils.codechicken.core.render.IconTransformation;
import mrtjp.projectred.utils.codechicken.core.vec.Rotation;
import mrtjp.projectred.utils.codechicken.core.vec.TransformationList;
import mrtjp.projectred.utils.codechicken.core.vec.Translation;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.util.Icon;

public class WireRenderAssistant {

	public int x;
	public int y;
	public int z;
	public int side;
	
	public RenderBlocks renderBlocks;
	
	public Map<String, CCModel> wireMap;
	public Icon wireIcon;

	boolean isCenterCrossed;
	boolean isCenterNS;
	boolean isCenterWE;

	boolean connectsCornerN;
	boolean connectsCornerS;
	boolean connectsCornerW;
	boolean connectsCornerE;

	boolean connectsInsideN;
	boolean connectsInsideS;
	boolean connectsInsideW;
	boolean connectsInsideE;

	boolean connectsN;
	boolean connectsS;
	boolean connectsW;
	boolean connectsE;
	
	boolean connectsInsideConnectorN;
	boolean connectsInsideConnectorS;
	boolean connectsInsideConnectorW;
	boolean connectsInsideConnectorE;

	public WireRenderAssistant() {
	}

	// Used to check relative direction for the side the wire is on.
	public int[][] sideMap = { 
			{ -1, -1, 2, 3, 5, 4}, 
			{ -1, -1, 3, 2, 4, 5}, 
			{ -1, -1, 1, 0, 5, 4}, 
			{ -1, -1, 0, 1, 4, 5}, 
			{ -1, -1, 2, 3, 0, 1}, 
			{ -1, -1, 3, 2, 1, 0 }, };
	
	public int[] frontMap_NS = { 2, 3, 1, 0, 2, 3 };
	public int[] frontMap_WE = { 5, 4, -1, -1, 0, 1 };

	public void setWireRenderState(TileWire t) {
		byte sideMask = t.getSideMask();
		// Connections
		connectsN = t.connectsInDirection(side, sideMap[side][2]);
		connectsS = t.connectsInDirection(side, sideMap[side][3]);
		connectsW = t.connectsInDirection(side, sideMap[side][4]);
		connectsE = t.connectsInDirection(side, sideMap[side][5]);

		// Center textures
		isCenterCrossed = (connectsN && connectsW || connectsN && connectsE || connectsS && connectsW || connectsS && connectsE);
		isCenterNS = (!isCenterCrossed && (connectsN || connectsS)) || (!connectsN && !connectsS && !connectsW && !connectsE);
		isCenterWE = (!isCenterCrossed && (connectsW || connectsE));

		// Outside corners
		connectsCornerN = (side != 0 && side != 1) && t.connectsInDirectionAroundCorner(side, sideMap[side][2]);
		connectsCornerS = (side != 0 && side != 1 && side != 4 && side != 5) && t.connectsInDirectionAroundCorner(side, sideMap[side][3]);
		connectsCornerW = (side != 0 && side != 1) && t.connectsInDirectionAroundCorner(side, sideMap[side][4]);
		connectsCornerE = (side != 0 && side != 1 && side != 2 && side != 3) && t.connectsInDirectionAroundCorner(side, sideMap[side][5]);

		// InIn edges
		connectsInsideN = ((sideMask & (1 << sideMap[side][2])) != 0);
		connectsInsideS = ((sideMask & (1 << sideMap[side][3])) != 0);
		connectsInsideW = ((sideMask & (1 << sideMap[side][4])) != 0);
		connectsInsideE = ((sideMask & (1 << sideMap[side][5])) != 0);

		connectsInsideConnectorN = (isCenterCrossed && connectsN) || (isCenterNS);
		connectsInsideConnectorS = (isCenterCrossed && connectsS) || (isCenterNS);
		connectsInsideConnectorW = (isCenterCrossed && connectsW) || (isCenterWE);
		connectsInsideConnectorE = (isCenterCrossed && connectsE) || (isCenterWE);

	}
	
	public void pushRender() {
		// Center
		if (isCenterCrossed) {
			if (side == 4){
				renderModelNS(wireMap.get("center_X"));
			} else {
				renderModelWE(wireMap.get("center_X"));
			}
		} else if (isCenterNS) {
			renderModelNS(wireMap.get("center_PN"));
		} else if (isCenterWE) {
			renderModelWE(wireMap.get("center_PN"));
		}
		
		//Inner connectors
		if (connectsInsideConnectorN) {
			renderModelNS(wireMap.get("insideconnector_N"));
		}
		if (connectsInsideConnectorS) {
			renderModelNS(wireMap.get("insideconnector_P"));
		}
		if (connectsInsideConnectorW) {
			renderModelWE(wireMap.get("insideconnector_N"));
		}
		if (connectsInsideConnectorE) {
			renderModelWE(wireMap.get("insideconnector_P"));
		}
		
		//Inout or InIn
		if (connectsN) {
			if (connectsInsideN) {
				renderModelNS(wireMap.get("inin_N"));
			} else {
				renderModelNS(wireMap.get("inout_N"));
			}
		}
		if (connectsS) {
			if (connectsInsideS) {
				renderModelNS(wireMap.get("inin_P"));
			} else {
				renderModelNS(wireMap.get("inout_P"));
			}
		}
		if (connectsW) {
			if (connectsInsideW) {
				renderModelWE(wireMap.get("inin_N"));
			} else {
				renderModelWE(wireMap.get("inout_N"));
			}
		}
		if (connectsE) {
			if (connectsInsideE) {
				renderModelWE(wireMap.get("inin_P"));
			} else {
				renderModelWE(wireMap.get("inout_P"));
			}
		}
		
		// Outer connectors
		if (connectsCornerN) {
			renderModelNS(wireMap.get("outsideconnector_N"));
		}
		if (connectsCornerS) {
			renderModelNS(wireMap.get("outsideconnector_P"));
		}
		if (connectsCornerW) {
			renderModelWE(wireMap.get("outsideconnector_N"));
		}
		if (connectsCornerE) {
			renderModelWE(wireMap.get("outsideconnector_P"));
		}
	}
	

	public void renderJacketedWire(Icon wireIcon, Map<String, CCModel> wireMap) {

	}

	private void renderModelNS(CCModel cc) {
		TransformationList t = new TransformationList();
		t.with(new Translation(.5, 0, .5)).with(Rotation.getForSideFacing(side, frontMap_NS[side])).with(new Translation(x, y, z));
		cc.render(0, cc.verts.length, t, new IconTransformation(renderBlocks.overrideBlockTexture != null ? renderBlocks.overrideBlockTexture : wireIcon), null);
	}
	
	private void renderModelWE(CCModel cc) {
		TransformationList t = new TransformationList();
		t.with(new Translation(.5, 0, .5)).with(Rotation.getForSideFacing(side, frontMap_WE[side])).with(new Translation(x, y, z));
		cc.render(0, cc.verts.length, t, new IconTransformation(renderBlocks.overrideBlockTexture != null ? renderBlocks.overrideBlockTexture : wireIcon), null);
	}

}
