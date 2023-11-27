package mrtjp.projectred.fabrication.lithography;

import codechicken.lib.data.MCDataOutput;

public interface ILithographerNetwork {

    MCDataOutput getBufferedStream(int frameKey);

    boolean isClientSide();

    void markSave();
}
