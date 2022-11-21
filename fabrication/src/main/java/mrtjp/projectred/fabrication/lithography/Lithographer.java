package mrtjp.projectred.fabrication.lithography;

import codechicken.lib.data.MCDataInput;

public class Lithographer {

    // Size of wafer in nanometers
    private int waferSize;
    // Size of each tile in nanometers
    private int tileSize;
    // Chance of defect per nanometer
    private double defectChance;

    private final ILithographerNetwork network;

    public Lithographer(ILithographerNetwork network) {
        this.network = network;
    }

    public void clear() {

    }

    public void setup() {

    }

    public void readBufferedStream(MCDataInput in, int frameKey) {

    }
}
