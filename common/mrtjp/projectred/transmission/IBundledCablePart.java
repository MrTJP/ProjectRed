package mrtjp.projectred.transmission;

import codechicken.multipart.TMultiPart;
import mrtjp.projectred.api.IBundledEmitter;

public interface IBundledCablePart extends IWirePart, IBundledEmitter
{
    public byte[] getBundledSignal();

    public byte[] calculateSignal();

    public void setSignal(byte[] newSignal);

    // Commons callbacks
    public void propogate(TMultiPart prev, int mode);
    public boolean propogateTo(TMultiPart part, int mode);
}
