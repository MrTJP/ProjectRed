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

	public double x;
	public double y;
	public double z;
	public int side;
	public int facing = -1;

	public float xOffset = 0;
	public float yOffset = 0;
	public float zOffset = 0;

	public RenderBlocks renderBlocks;

	/** Start Wire Rendering **/
	
	public Map<String, CCModel> model;
	public Icon wireIcon;

	public boolean isCenterCrossed;
	public boolean isCenterNS;
	public boolean isCenterWE;

	public boolean connectsCornerN;
	public boolean connectsCornerS;
	public boolean connectsCornerW;
	public boolean connectsCornerE;

	public boolean connectsInsideN;
	public boolean connectsInsideS;
	public boolean connectsInsideW;
	public boolean connectsInsideE;

	public boolean connectsN;
	public boolean connectsS;
	public boolean connectsW;
	public boolean connectsE;

	public boolean connectsInsideConnectorN;
	public boolean connectsInsideConnectorS;
	public boolean connectsInsideConnectorW;
	public boolean connectsInsideConnectorE;

	// Used to check relative direction for the side the wire is on.
	public int[][] sideMap = { 
			{ -1, -1, 2, 3, 5, 4 }, 
			{ -1, -1, 3, 2, 4, 5 }, 
			{ -1, -1, 1, 0, 5, 4 }, 
			{ -1, -1, 0, 1, 4, 5 }, 
			{ -1, -1, 0, 1, 2, 3 }, 
			{ -1, -1, 1, 0, 3, 2 }, 
			};

	public int[] frontMap_NS = { 2, 3, 1, 0, 0, 1 };
	public int[] frontMap_WE = { 5, 4, 5, 4, 2, 3 };	

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
		connectsCornerS = (side != 0 && side != 1) && t.connectsInDirectionAroundCorner(side, sideMap[side][3]);
		connectsCornerW = (side != 0 && side != 1) && t.connectsInDirectionAroundCorner(side, sideMap[side][4]);
		connectsCornerE = (side != 0 && side != 1 && side != 2 && side != 3 && side != 4 && side != 5) && t.connectsInDirectionAroundCorner(side, sideMap[side][5]);

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
			if (side == 1 || side == 3 || side == 4) {
				renderModelWE(model.get("center_X"));
			} else {
				renderModelNS(model.get("center_X"));
			}
		} else if (isCenterNS) {
			renderModelNS(model.get("center_PN"));
		} else if (isCenterWE) {
			renderModelWE(model.get("center_PN"));
		}

		// Inner connectors
		if (connectsInsideConnectorN) {
			renderModelNS(model.get("insideconnector_N"));
		}
		if (connectsInsideConnectorS) {
			renderModelNS(model.get("insideconnector_P"));
		}
		if (connectsInsideConnectorW) {
			renderModelWE(model.get("insideconnector_N"));
		}
		if (connectsInsideConnectorE) {
			renderModelWE(model.get("insideconnector_P"));
		}

		// Inout or InIn
		if (connectsN) {
			if (connectsInsideN) {
				renderModelNS(model.get("inin_N"));
			} else {
				renderModelNS(model.get("inout_N"));
			}
		}
		if (connectsS) {
			if (connectsInsideS) {
				renderModelNS(model.get("inin_P"));
			} else {
				renderModelNS(model.get("inout_P"));
			}
		}
		if (connectsW) {
			if (connectsInsideW) {
				renderModelWE(model.get("inin_N"));
			} else {
				renderModelWE(model.get("inout_N"));
			}
		}
		if (connectsE) {
			if (connectsInsideE) {
				renderModelWE(model.get("inin_P"));
			} else {
				renderModelWE(model.get("inout_P"));
			}
		}

		// Outer corner connectors
		if (connectsCornerN) {
			renderModelNS(model.get("outsideconnector_N"));
		}
		if (connectsCornerS) {
			renderModelNS(model.get("outsideconnector_P"));
		}
		if (connectsCornerW) {
			renderModelWE(model.get("outsideconnector_N"));
		}
		if (connectsCornerE) {
			renderModelWE(model.get("outsideconnector_P"));
		}
	}

	private void renderModelNS(CCModel cc) {
		if (cc == null) {
			System.out.println("Wire model is currupt or missing.");
			return;
		}
		TransformationList t = new TransformationList();
		t.with(new Translation(.5 + xOffset, 0 + yOffset, .5 + zOffset)).with(Rotation.getForSideFacing(side, (facing > -1 ? facing : frontMap_NS[side]))).with(new Translation(x, y, z));
		cc.render(0, cc.verts.length, t, new IconTransformation(renderBlocks != null && renderBlocks.overrideBlockTexture != null ? renderBlocks.overrideBlockTexture : wireIcon), null);
	}

	private void renderModelWE(CCModel cc) {
		if (cc == null) {
			System.out.println("Wire model is currupt or missing.");
			return;
		}
		TransformationList t = new TransformationList();
		t.with(new Translation(.5 + xOffset, 0 + yOffset, .5 + zOffset)).with(Rotation.getForSideFacing(side, (facing > -1 ? facing : frontMap_WE[side]))).with(new Translation(x, y, z));
		cc.render(0, cc.verts.length, t, new IconTransformation(renderBlocks != null && renderBlocks.overrideBlockTexture != null ? renderBlocks.overrideBlockTexture : wireIcon), null);
	}
	/** End Wire Rendering **/
	
	
	/** Start Jacketed Rendering **/
	public boolean isJacketCenterCrossed;
	public boolean isJacketCenterNS;
	public boolean isJacketCenterWE;
	public boolean isJacketCenterUD;
	
	public boolean renderFrameEdges;
	
	public boolean jacketU;
	public boolean jacketD;
	public boolean jacketN;
	public boolean jacketS;
	public boolean jacketW;
	public boolean jacketE;
	
	public void setJacketRender(TileWire t) {
		// Swapped connection logic, because model is rendering flipped, Blender mistake probably.
		jacketD = t.wireConnectsInDirection(-1, 0);
		jacketU = t.wireConnectsInDirection(-1, 1);
		jacketN = t.wireConnectsInDirection(-1, 3);
		jacketS = t.wireConnectsInDirection(-1, 2);
		jacketW = t.wireConnectsInDirection(-1, 5);
		jacketE = t.wireConnectsInDirection(-1, 4);
		
		renderFrameEdges = true;
		
		// Utils
		int connections = 0;
		if (jacketD) {
			connections++;
		}
		if (jacketU) {
			connections++;
		}
		if (jacketN) {
			connections++;
		}
		if (jacketS) {
			connections++;
		}
		if (jacketW) {
			connections++;
		}
		if (jacketE) {
			connections++;
		}
		// Center
		isJacketCenterCrossed = (jacketD && (jacketN || jacketS || jacketW || jacketE)) ||(jacketU && (jacketN || jacketS || jacketW || jacketE)) ||(jacketN && (jacketU || jacketD || jacketW || jacketE)) ||(jacketS && (jacketU || jacketD || jacketW || jacketE)) ||(jacketW && (jacketN || jacketS || jacketU || jacketD)) ||(jacketE && (jacketN || jacketS || jacketU || jacketD)) || (connections < 2);
		isJacketCenterUD = !isJacketCenterCrossed && (jacketU && jacketD);
		isJacketCenterNS = !isJacketCenterCrossed && (jacketN && jacketS);
		isJacketCenterWE = !isJacketCenterCrossed && (jacketW && jacketE);
	}
	
	public void setInventoryJacketRender() {
		jacketU = jacketD = jacketN = jacketS = jacketW = jacketE = true;
		isJacketCenterCrossed = true;
		renderFrameEdges = false;
	}
	
	public void pushJacketFrameRender() {
		// Center
		if (isJacketCenterCrossed) {
			renderJacketModel(model.get("frame_center"));
		} else if (isJacketCenterUD) {
			renderJacketModel(model.get("frame_center_UD"));
		} else if (isJacketCenterNS) {
			renderJacketModel(model.get("frame_center_NS"));
		} else if (isJacketCenterWE) {
			renderJacketModel(model.get("frame_center_WE"));
		}

		if (!renderFrameEdges) {
			return;
		}
		
		// Up
		if (jacketU) {
			renderJacketModel(model.get("frame_U"));
		}
		// Down
		if (jacketD) {
			renderJacketModel(model.get("frame_D"));
		}
		// North
		if (jacketN) {
			renderJacketModel(model.get("frame_N"));
		}
		// South
		if (jacketS) {
			renderJacketModel(model.get("frame_S"));
		}
		// West
		if (jacketW) {
			renderJacketModel(model.get("frame_W"));
		}
		// East
		if (jacketE) {
			renderJacketModel(model.get("frame_E"));
		}
	}
	
	public void pushJacketWireRender() {
		// Center
		if (isJacketCenterCrossed) {
			renderJacketModel(model.get("wire_center"));
		} else if (isJacketCenterUD) {
			renderJacketModel(model.get("wire_center_UD"));
		} else if (isJacketCenterNS) {
			renderJacketModel(model.get("wire_center_NS"));
		} else if (isJacketCenterWE) {
			renderJacketModel(model.get("wire_center_WE"));
		}

		// Up
		if (jacketU) {
			renderJacketModel(model.get("wire_U"));
		}
		// Down
		if (jacketD) {
			renderJacketModel(model.get("wire_D"));
		}
		// North
		if (jacketN) {
			renderJacketModel(model.get("wire_N"));
		}
		// South
		if (jacketS) {
			renderJacketModel(model.get("wire_S"));
		}
		// West
		if (jacketW) {
			renderJacketModel(model.get("wire_W"));
		}
		// East
		if (jacketE) {
			renderJacketModel(model.get("wire_E"));
		}
	}
	
	public void renderJacketModel(CCModel cc) {
		if (cc == null) {
			System.out.println("Wire model is currupt or missing.");
			return;
		}
		TransformationList t = new TransformationList();
		t.with(new Translation(.5 + xOffset, 0 + yOffset, .5 + zOffset)).with(new Translation(x, y, z));
		cc.render(0, cc.verts.length, t, new IconTransformation(renderBlocks != null && renderBlocks.overrideBlockTexture != null ? renderBlocks.overrideBlockTexture : wireIcon), null);
	}
	/** End Jacketed Rendering **/
}
