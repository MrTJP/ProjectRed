package mrtjp.projectred.multipart.wiring.gates;
import mrtjp.projectred.integration.TileGate;
import mrtjp.projectred.utils.gui.BaseContainer;
import net.minecraft.entity.player.EntityPlayer;

public class ContainerCounter extends BaseContainer {
	
	public int max = 10, incr = 1, decr = 1, value = 0;
	
	private GateLogic.Counter counter;
	
	public ContainerCounter(EntityPlayer player, TileGate tile) {
		super(player, tile);
		
		counter = ((GateLogic.Counter)tile.getLogic());
	}
	
	@Override
	public void detectAndSendChanges() {
		super.detectAndSendChanges();
		
		setProgressBar((short)0, (short)counter.max);
		setProgressBar((short)1, (short)counter.incr);
		setProgressBar((short)2, (short)counter.decr);
		setProgressBar((short)3, (short)counter.value);
	}
	
	@Override
	public void updateProgressBar(int par1, int par2) {
		if(par1 == 0) max = par2;
		else if(par1 == 1) incr = par2;
		else if(par1 == 2) decr = par2;
		else if(par1 == 3) value = par2;
	}
	
	@Override
	public void onButtonPressed(int id) {
		int delta = 0;
		switch(id & 7) {
		case 0: delta -= 10; break;
		case 1: delta -= 5; break;
		case 2: delta -= 1; break;
		case 3: delta += 1; break;
		case 4: delta += 5; break;
		case 5: delta += 10; break;
		}
		
		switch(id >> 3) {
		case 0: counter.max = Math.min(32767, Math.max(1, counter.max + delta)); break;
		case 1: counter.incr = Math.min(counter.max, Math.max(1, counter.incr + delta)); break;
		case 2: counter.decr = Math.min(counter.max, Math.max(1, counter.decr + delta)); break;
		}
		
		counter.value = Math.min(counter.max, counter.value);
	}
}
