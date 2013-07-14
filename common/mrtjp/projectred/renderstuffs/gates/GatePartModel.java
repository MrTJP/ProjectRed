package mrtjp.projectred.renderstuffs.gates;

import java.util.Map;

import mrtjp.projectred.utils.codechicken.core.render.CCModel;
import mrtjp.projectred.utils.codechicken.core.render.CCRenderState;
import mrtjp.projectred.utils.codechicken.core.vec.InvertX;
import mrtjp.projectred.utils.codechicken.core.vec.Rotation;
import mrtjp.projectred.utils.codechicken.core.vec.Translation;

public class GatePartModel {

	private Map<String, CCModel> models;
	private String objPath;
	private String texPath;

	/**
	 * The name of the object in folder /mods/projectred/textures/obj/. Name of
	 * png in folder /mods/projectred/textures/blocks/gates/.
	 * 
	 * You would pass in example.obj and example2.png;
	 * 
	 * @param objName
	 * @param texName
	 */
	public GatePartModel(String objName, String texName) {
		String baseObj = "/mods/projectred/textures/obj/gateparts/";
		String baseTex = "/mods/projectred/textures/blocks/gates/";
		objPath =  baseObj + objName;
		texPath = baseTex + texName;
		models = CCModel.parseObjModels(objPath, new InvertX());
	}

	/**
	 * This is bound to the render engine before rendering models.
	 * 
	 * @return
	 */
	public String getTexture() {
		return texPath;
	}

	public Map<String, CCModel> getCCModels() {
		return models;
	}

	/**
	 * Renders the part of the model, on the block x, y, z, sticking to the side
	 * facing the direction specified, offSetted within the block by xOffset,
	 * yOffset, and zOffset.
	 * 
	 * @param x
	 * @param y
	 * @param z
	 * @param side
	 * @param facing
	 * @param xOffset
	 * @param yOffset
	 * @param zOffset
	 */
	public void renderPart(String part, float x, float y, float z, int side, int facing, float xOffset, float yOffset, float zOffset) {
		CCModel cc = getCCModels().get(part);
		if (cc != null) {
			CCRenderState.reset();
			CCRenderState.changeTexture(getTexture());
			CCRenderState.useNormals(true);
			CCRenderState.startDrawing(4);
			cc.copy().apply(new Translation(xOffset, yOffset, zOffset)).apply(Rotation.getForSideFacing(side, facing)).apply(new Translation(x, y, z)).render();
			CCRenderState.draw();
		}
	}
}
