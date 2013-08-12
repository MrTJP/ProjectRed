package mrtjp.projectred.transmission;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.Icon;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class InsulatedRedAlloyPart extends RedwirePart {

    public byte colour;
    
    public InsulatedRedAlloyPart(int side) {
        super(side);
    }
    
    @Override
    public void save(NBTTagCompound tag) {
        super.save(tag);
        tag.setByte("colour", colour);
    }
    
    @Override
    public void load(NBTTagCompound tag) {
        super.load(tag);
        colour = tag.getByte("colour");
    }
    
    @Override
    public void writeDesc(MCDataOutput packet) {
        super.writeDesc(packet);
        packet.writeByte(colour);
    }
    
    @Override
    public void readDesc(MCDataInput packet) {
        super.readDesc(packet);
        colour = packet.readByte();
    }
    
    @Override
    public void onPlaced(int side, int meta) {
        super.onPlaced(side, meta);
        colour = (byte)(meta-EnumWire.INSULATED_0.ordinal());
    }
    
    @Override
    public EnumWire getWireType() {
        return EnumWire.INSULATED_WIRE[colour];
    }
    
    @Override
    public String getType() {
        return "pr_insulated";
    }
    
    @Override
    @SideOnly(Side.CLIENT)
    public Icon getIcon() {
        return getWireType().wireSprites[strength != 0 ? 1 : 0];
    }
    
    @Override
    public boolean canConnectToType(WirePart wire, int r) {
        if(wire instanceof InsulatedRedAlloyPart)
            return ((InsulatedRedAlloyPart) wire).colour == colour;
        
        return true;
    }
    
    @Override
    public int weakPowerLevel(int side)
    {
        if(this.side == side || this.side == (side^1))
            return 0;
        
        return super.weakPowerLevel(side);
    }
}
