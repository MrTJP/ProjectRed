package mrtjp.projectred.illumination;

import net.minecraft.client.renderer.tileentity.TileEntitySpecialRenderer;
import net.minecraft.tileentity.TileEntity;

public class LampRenderer extends TileEntitySpecialRenderer {
	public static final LampRenderer instance = new LampRenderer();
	private LampModel model = new LampModel();

	@Override
	public void renderTileEntityAt(TileEntity tileentity, double x, double y, double z, float f) {
		if (tileentity instanceof TileLamp) {
			TileLamp lamp = (TileLamp) tileentity;
			if (lamp.getLightValue() == 15) {
				model.renderLampShade(x, y, z, lamp.getColor());
			}
		}
	}
}
