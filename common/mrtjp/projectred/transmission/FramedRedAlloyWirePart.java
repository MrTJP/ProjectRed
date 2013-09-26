package mrtjp.projectred.transmission;

public class FramedRedAlloyWirePart extends FramedRedwirePart
{
    @Override
    public String getType() {
        return "pr_sredwire";
    }

    @Override
    public EnumWire getWireType() {
        return EnumWire.RED_ALLOY;
    }
    
    @Override
    public int getColour() {
        return ((signal&0xFF)/2 + 60) << 24 | 0xFF;
    }
}
