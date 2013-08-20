package mrtjp.projectred.transmission;

public interface IBundledCablePart extends IWirePart, IBundledEmitter
{
    public byte[] getBundledSignal();

    public byte[] calculateSignal();

    public void setSignal(byte[] newSignal);
}
