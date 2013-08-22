package mrtjp.projectred.integration2;

import net.minecraft.nbt.NBTTagCompound;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.multipart.TickScheduler;

public class SimpleGatePart extends RedstoneGatePart
{
    public byte state;
    
    public int state() {
        return state&0xFF;
    }
    
    public void setState(int s) {
        state = (byte) s;
    }
    
    @Override
    public void save(NBTTagCompound tag) {
        super.save(tag);
        tag.setByte("state", state);
    }
    
    @Override
    public void load(NBTTagCompound tag) {
        super.load(tag);
        state = tag.getByte("state");
    }
    
    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeByte(state);
    }
    
    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        state = packet.readByte();
    }
    
    @Override
    public void read(MCDataInput packet, int switch_key) {
        if(switch_key == 10) {
            state = packet.readByte();
            tile().markRender();
        }
        else
            super.read(packet, switch_key);
    }
    
    @Override
    public String getType() {
        return "pr_sgate";
    }
    
    public SimpleGateLogic getLogic() {
        return SimpleGateLogic.instances[subID&0xFF];
    }

    public void sendStateUpdate() {
        tile().getWriteStream(this).writeByte(10).writeByte(state);
    }
    
    public void onInputChange() {
        tile().markDirty();
        sendStateUpdate();
    }

    public void onOutputChange(int mask) {
        tile().markDirty();
        sendStateUpdate();
        tile().internalPartChange(this);
        notifyNeighbors(toAbsoluteMask(mask));
    }
}
