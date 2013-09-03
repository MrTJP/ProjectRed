package mrtjp.projectred.integration;

import net.minecraft.entity.player.EntityPlayer;
import net.minecraft.nbt.NBTTagCompound;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;

public class InstancedRsGatePart extends SimpleGatePart
{
    public InstancedRsGateLogic logic;
    
    @Override
    public String getType() {
        return "pr_igate";
    }
    
    @Override
    public InstancedRsGateLogic getLogic() {
        return logic;
    }
    
    @Override
    public void load(NBTTagCompound tag) {
        super.load(tag);
        logic = InstancedRsGateLogic.create(this, subID);
        logic.load(tag);
    }
    
    @Override
    public void save(NBTTagCompound tag) {
        super.save(tag);
        logic.save(tag);
    }
    
    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        logic.writeDesc(packet);
    }
    
    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        if(logic == null)
            logic = InstancedRsGateLogic.create(this, subID);
        logic.readDesc(packet);
    }
    
    @Override
    public void read(MCDataInput packet, int switch_key) {
        if(switch_key <= 10)
            super.read(packet, switch_key);
        else
            logic.read(packet, switch_key);
    }
    
    @Override
    public void onPlaced(EntityPlayer player, int side, int meta) {
        super.onPlaced(player, side, meta);
        logic = InstancedRsGateLogic.create(this, subID);
    }
}
