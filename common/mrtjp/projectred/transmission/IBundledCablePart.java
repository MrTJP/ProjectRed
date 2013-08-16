package mrtjp.projectred.transmission;

public interface IBundledCablePart extends IWirePart
{
    public byte[] getBundledSignal();

    public byte[] calculateSignal();

    public void setSignal(byte[] newSignal);
}
