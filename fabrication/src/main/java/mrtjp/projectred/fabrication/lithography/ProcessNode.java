package mrtjp.projectred.fabrication.lithography;

public enum ProcessNode {
    PROCESS_64NM(64),
    PROCESS_32NM(32),
    PROCESS_16NM(16),
    PROCESS_8NM(8),
    ;

    private final int tileLen;
    private final int tileArea;

    ProcessNode(int tileLen) {
        this.tileLen = tileLen;
        this.tileArea = tileLen * tileLen;
    }

    public int getTileWidth() {
        return tileLen;
    }

    public int getTileHeight() {
        return tileLen;
    }

    public int getTileArea() {
        return tileArea;
    }

    public String getDisplayName() {
        return "" + tileLen + "nm";
    }
}
