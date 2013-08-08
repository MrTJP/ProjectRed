package mrtjp.projectred.transmission;

import net.minecraft.world.World;
import net.minecraftforge.common.ForgeDirection;
import buildcraft.api.power.IPowerReceptor;
import buildcraft.api.power.PowerHandler;
import buildcraft.api.power.PowerHandler.PowerReceiver;
import buildcraft.api.power.PowerHandler.Type;

public class MJXJacketedPart extends MJXPart implements IPowerReceptor {

	private PowerHandler pwr;
	
	public MJXJacketedPart(EnumWire type, boolean isJacketedWire, int onside) {
		super(type, isJacketedWire, onside);
		pwr = new PowerHandler(this, Type.MACHINE);
		pwr.configurePowerPerdition(0, 0);
		pwr.configure(1, 100, 1, 128);
	}

	@Override
	public PowerReceiver getPowerReceiver(ForgeDirection side) {
		if (isJacketed) {
			if (maskConnectsJacketed(side.ordinal())) {
				return pwr.getPowerReceiver();
			}
		}
		return null;
	}
	
	@Override
	public void update() {
		super.update();
		pwr.update();
	}

	@Override
	public void doWork(PowerHandler workProvider) {
		float added = workProvider.useEnergy(0, this.addPower(workProvider.getEnergyStored()), true);
	}

	@Override
	public World getWorld() {
		return world();
	}

}
