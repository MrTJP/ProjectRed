package mrtjp.projectred.fabrication.lithography;

public enum WaferType {

    ROUGH_WAFER     (4096, 0.0005),
    PURIFIED_WAFER  (4096, 0.0001),
    POLISHED_WAFER  (4096, 0.00001);

    private final int waferLen;
    private final int waferArea;
    private final double defectChancePerLen;

    WaferType(int waferLen, double defectChancePerLen) {
        this.waferLen = waferLen;
        this.waferArea = waferLen * waferLen;
        this.defectChancePerLen = defectChancePerLen;
    }

    public int getWaferArea() {
        return waferArea;
    }

    public int getWaferLen() {
        return waferLen;
    }

    public double getDefectChancePerLen() {
        return defectChancePerLen;
    }
}
