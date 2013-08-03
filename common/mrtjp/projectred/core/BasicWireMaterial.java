package mrtjp.projectred.core;

import net.minecraft.block.material.Material;
import net.minecraft.block.material.MaterialLogic;

public class BasicWireMaterial extends MaterialLogic {

	public BasicWireMaterial() {
		super(Material.circuits.materialMapColor);
	}

	/**
	 * Used to prevent water from washing away.
	 */
	@Override
	public boolean blocksMovement() {
		return true;
	}
}
