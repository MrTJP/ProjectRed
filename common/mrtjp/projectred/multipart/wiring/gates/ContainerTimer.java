package mrtjp.projectred.multipart.wiring.gates;

import mrtjp.projectred.utils.gui.BaseContainer;
import net.minecraft.entity.player.EntityPlayer;

public class ContainerTimer extends BaseContainer {

	public int intervalTicks = 4; // game ticks

	private GateLogicTimed timer;

	public ContainerTimer(EntityPlayer player, TileGate tile) {
		super(player, tile);
		timer = (GateLogicTimed) tile.getLogic();
	}

	@Override
	public void detectAndSendChanges() {
		super.detectAndSendChanges();
		setProgressBar((short) 0, (short) timer.getInterval());
	}

	@Override
	public void updateProgressBar(int par1, int par2) {
		if (par1 == 0)
			intervalTicks = par2 & 0xFFFF;
	}

	@Override
	public void onButtonPressed(int id) {
		intervalTicks = timer.getInterval();

		switch (id) {
		case 0:
			intervalTicks -= 200;
			break;
		case 1:
			intervalTicks -= 20;
			break;
		case 2:
			intervalTicks -= 1;
			break;
		case 3:
			intervalTicks += 1;
			break;
		case 4:
			intervalTicks += 20;
			break;
		case 5:
			intervalTicks += 200;
			break;
		}
		if (intervalTicks < 4)
			intervalTicks = 4;
		if (intervalTicks > 65535)
			intervalTicks = 65535;

		timer.setInterval(intervalTicks);
	}
}
