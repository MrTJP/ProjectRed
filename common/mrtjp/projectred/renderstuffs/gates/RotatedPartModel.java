package mrtjp.projectred.renderstuffs.gates;

import java.util.Map;

import mrtjp.projectred.utils.codechicken.core.render.CCModel;
import mrtjp.projectred.utils.codechicken.core.render.CCRenderState;
import mrtjp.projectred.utils.codechicken.core.render.IUVTransformation;
import mrtjp.projectred.utils.codechicken.core.vec.InvertX;
import mrtjp.projectred.utils.codechicken.core.vec.Rotation;
import mrtjp.projectred.utils.codechicken.core.vec.TransformationList;
import mrtjp.projectred.utils.codechicken.core.vec.Translation;
import net.minecraft.util.Icon;

/**
 * RotatedTessellator can easily render these models at x, y, z with side and
 * facing, and offsets.
 * 
 * @author MrTJP
 * 
 */
public class RotatedPartModel {
	private Map<String, CCModel> models;
	private String objPath;
	private Icon partIcon;

	public RotatedPartModel(String objName, Icon icon) {
		this(null, objName, icon);
	}

	/**
	 * The name of the object in folder /mods/projectred/textures/obj/. Icon
	 * used to render the actual part. If a model map is passed in, it will not
	 * be parsed.
	 *  
	 * @param objName
	 * @param texName
	 * @param Icon
	 */
	public RotatedPartModel(Map<String, CCModel> objModel, String objName, Icon icon) {
		String baseObj = "/assets/projectred/textures/obj/";
		objPath = baseObj + objName;
		if (objModel == null) {
			models = CCModel.parseObjModels(objPath, 7, new InvertX());
			for (CCModel m : models.values()) {
				m.shrinkUVs(0.0005);
			}
		} else {
			models = objModel;
		}
		partIcon = icon;
	}

	/**
	 * This is bound to the render engine before rendering models.
	 * 
	 * @return
	 */
	public Icon getIcon() {
		return partIcon;
	}

	public Map<String, CCModel> getCCModels() {
		return models;
	}

	/**
	 * Used to render the model on block x, y, z, sticking to side side and
	 * facing ForgeDirection facing. It is rotated, offsetted, rotated to sides
	 * facing, then translated to the block coords..
	 * 
	 * @param part
	 * @param x
	 * @param y
	 * @param z
	 * @param side
	 * @param facing
	 * @param xOffset
	 * @param yOffset
	 * @param zOffset
	 * @param degRotation
	 * @param color
	 * @param uv
	 */
	public void renderPart(String part, float x, float y, float z, int side, int facing, float xOffset, float yOffset, float zOffset, float degRotation, int color, IUVTransformation uv) {
		CCModel cc = getCCModels().get(part);
		if (cc != null) {
			TransformationList tl = new TransformationList();
			if (degRotation > -1) {
				tl.with(new Rotation(Math.toRadians(degRotation), 0, 1, 0).inverse());
			}
			tl.with(new Translation(xOffset, yOffset, zOffset)).with(Rotation.getForSideFacing(side, facing)).with(new Translation(x, y, z));
			if (color > -1) {
				CCRenderState.setColourOpaque(color);
			}
			cc.render(0, cc.verts.length, tl, uv, null);
		}
	}
}
