package mrtjp.projectred.transmission;

import static mrtjp.projectred.transmission.IWirePart.DROPPING;
import static mrtjp.projectred.transmission.IWirePart.FORCE;
import static mrtjp.projectred.transmission.IWirePart.FORCED;
import static mrtjp.projectred.transmission.IWirePart.RISING;
import codechicken.multipart.TMultiPart;

public class BundledCableCommons
{
    public static byte[] tmpSignal = new byte[16];
    private static int propogatingMask = 0xFFFF;

    public static boolean signalsEqual(byte[] signal1, byte[] signal2) {
        for (int i = 0; i < 16; i++)
            if (signal1[i] != signal2[i])
                return false;

        return true;
    }

    public static boolean isSignalZero(byte[] signal, int mask) {
        if (signal == null)
            return true;

        for (int i = 0; i < 16; i++)
            if ((mask & 1<<i) != 0 && signal[i] != 0)
                return false;

        return true;
    }

    public static boolean dropSignalsLessThan(byte[] signal1, byte[] signal2) {
        boolean dropped = false;

        for (int i = 0; i < 16; i++)
            if ((signal2[i] & 0xFF) < (signal1[i] & 0xFF)) {
                signal1[i] = 0;
                dropped = true;
            }

        return dropped;
    }
    
    public static void applyChangeMask(byte[] signal, byte[] newSignal, int mask) {
        for(int i = 0; i < 16; i++)
            if((mask & 1<<i) == 0)
                newSignal[i] = signal[i];
    }

    public static void updateAndPropogate(IBundledCablePart part, TMultiPart prev, int mode) {
        int mask = getUpdateMask(part, prev, mode);
        if (mode == DROPPING && isSignalZero(part.getBundledSignal(), mask))
            return;
        
        byte[] newSignal = part.calculateSignal();
        applyChangeMask(part.getBundledSignal(), newSignal, mask);
        
        propogatingMask = mask;
        
        if (dropSignalsLessThan(part.getBundledSignal(), newSignal)) {
            part.propogate(prev, DROPPING);
        } else if (!signalsEqual(part.getBundledSignal(), newSignal)) {
            part.setSignal(newSignal);
            if (mode == DROPPING)
                part.propogate(null, RISING);
            else
                part.propogate(prev, RISING);
        } else {
            if (mode == DROPPING)
                part.propogateTo(prev, RISING);
            else if (mode == FORCE)
                part.propogate(prev, FORCED);
        }
        
        propogatingMask = 0xFFFF;
    }

    public static int getUpdateMask(IBundledCablePart part, TMultiPart prev, int mode) {
        if(prev instanceof IInsulatedRedwirePart)
            return 1<<((IInsulatedRedwirePart)prev).getInsulatedColour();
        
        if(prev instanceof IBundledCablePart) {
            byte[] osignal = ((IBundledCablePart)prev).getBundledSignal();
            if(mode == DROPPING) {
                int m = 0;
                for(int i = 0; i < 16; i++)
                    if(osignal[i] == 0)
                        m|=1<<i;
                return m;
            }
            else if(mode == RISING) {
                int m = 0;
                for(int i = 0; i < 16; i++)
                    if((osignal[i] & 0xFF) > (part.getBundledSignal()[i] & 0xFF))
                        m|=1<<i;
                return m;
            }
        }
        
        return 0xFFFF;
    }
    
    public static boolean shouldPropogate(IBundledCablePart bundled, TMultiPart part, int mode) {
        // no point propogating to an ins wire if we didn't change their colour
        if (part instanceof IInsulatedRedwirePart)
            if ((propogatingMask & 1<<((IInsulatedRedwirePart) part).getInsulatedColour()) == 0)
                return false;
        
        return true;
    }
    
    public static void calculatePartSignal(TMultiPart part, int r) {
        if (part instanceof IBundledCablePart) {
            byte[] osignal = ((IBundledCablePart) part).getBundledSignal();
            for (int i = 0; i < 16; i++)
                if ((osignal[i] & 0xFF) - 1 > (tmpSignal[i] & 0xFF))
                    tmpSignal[i] = (byte) (osignal[i] - 1);
        } else if (part instanceof IInsulatedRedwirePart) {
            IInsulatedRedwirePart insPart = (IInsulatedRedwirePart) part;
            int s = insPart.getRedwireSignal(r) - 1;
            if (s > (tmpSignal[insPart.getInsulatedColour()] & 0xFF))
                tmpSignal[insPart.getInsulatedColour()] = (byte) s;
        } else if (part instanceof IBundledEmitter) {
            byte[] osignal = ((IBundledEmitter) part).getBundledSignal(r);
            if (osignal != null) {
                for (int i = 0; i < 16; i++)
                    if ((osignal[i] & 0xFF) > (tmpSignal[i] & 0xFF))
                        tmpSignal[i] = osignal[i];
            }
        }
    }
}
