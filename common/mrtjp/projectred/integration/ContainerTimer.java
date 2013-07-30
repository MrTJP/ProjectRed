package mrtjp.projectred.integration;

import mrtjp.projectred.integration.GateLogic.GateLogicTimed;
import net.minecraft.entity.player.EntityPlayer;
import codechicken.multipart.TMultiPart;

public class ContainerTimer extends ContainerMultipart {

	GatePart g = (GatePart) part;
	GateLogicTimed timer = (GateLogicTimed) g.getLogic();
	int timerInterval = 4;

	public ContainerTimer(EntityPlayer player, TMultiPart part) {
		super(player, part);
	}

	@Override
	public void setNewMessages() {
		setGraphicalUpdate((short) 0, (short) timer.getInterval());
	}

	@Override
	public void interceptMessage(int ID, int message) {
		if (ID == 0) {
			timerInterval = message;
		}
	}

	@Override
	public void handleActionFromClient(int buttonID) {
		timerInterval = timer.getInterval();
		switch (buttonID) {
		case 0:
			timerInterval -= 200;
			break;
		case 1:
			timerInterval -= 20;
			break;
		case 2:
			timerInterval -= 1;
			break;
		case 3:
			timerInterval += 1;
			break;
		case 4:
			timerInterval += 20;
			break;
		case 5:
			timerInterval += 200;
			break;
		}
		if (timerInterval < 4)
			timerInterval = 4;
		if (timerInterval > 65535)
			timerInterval = 65535;
		timer.setInterval(timerInterval);
	}

	@Override
	public boolean canInteractWith(EntityPlayer entityplayer) {
		return true;
	}

}
