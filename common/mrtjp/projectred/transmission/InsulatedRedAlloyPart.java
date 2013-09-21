package mrtjp.projectred.transmission;

import mrtjp.projectred.api.IConnectable;
import net.minecraft.nbt.NBTTagCompound;
import net.minecraft.util.Icon;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.vec.Rotation;
import codechicken.multipart.TMultiPart;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class InsulatedRedAlloyPart extends RedwirePart implements IInsulatedRedwirePart {

    public byte colour;
        
    @Override
    public String getType() {
        return "pr_insulated";
    }

    @Override
    public EnumWire getWireType() {
        return EnumWire.INSULATED_WIRE[colour];
    }

    @Override
    public void preparePlacement(int side, int meta) {
        super.preparePlacement(side, meta);
        colour = (byte)(meta-EnumWire.INSULATED_0.ordinal());
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
    public int getPartSignal(TMultiPart part, int r) {
        if(part instanceof IBundledCablePart)
            return (((IBundledCablePart) part).getBundledSignal()[colour]&0xFF)-1;
        
        return super.getPartSignal(part, r);
    }

    @Override
    public int weakPowerLevel(int side) {
        if(this.side == side || this.side == (side^1) ||
                !maskConnects(Rotation.rotationTo(this.side, side)))
            return 0;
        
        return super.weakPowerLevel(side);
    }

    @Override
    public boolean canConnectToType(IConnectable wire) {
        if(wire instanceof IInsulatedRedwirePart)
            return ((IInsulatedRedwirePart) wire).getInsulatedColour() == colour;
        
        return true;
    }
        
    @Override
    @SideOnly(Side.CLIENT)
    public Icon getIcon() {
        return getWireType().wireSprites[signal != 0 ? 1 : 0];
    }
    
    @Override
    public int getInsulatedColour() {
        return colour;
    }
}
