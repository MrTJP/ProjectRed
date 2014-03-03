package mrtjp.projectred.transmission;

public class FramedRedAlloyWirePart extends FramedRedwirePart
{
    @Override
    public WireDef getWireType()
    {
        return WireDef.RED_ALLOY();
    }

    @Override
    public int getColour()
    {
        return (signal&0xFF)/2+60<<24|0xFF;
    }
}
