package mrtjp.projectred.fabrication.lithography;

public enum WaferType {

    //@formatter:off
    ROUGH_WAFER     (4096, 0.5,  "roughWafer"),
    PURIFIED_WAFER  (4096, 0.1,  "purifiedWafer"),
    POLISHED_WAFER  (4096, 0.01, "polishedWafer");
    //@formatter:on

    private final String unlocalizedName;
    private final int waferLen;
    private final int waferArea;
    private final double defectRatePerUnitArea;

    // Note: standard die defect rate is rate of defect of 16x16 design in 64nm process
    WaferType(int waferLen, double standardDieDefectRate, String unlocalizedName) {
        this.waferLen = waferLen;
        this.waferArea = waferLen * waferLen; // square wafers only atm

        // back-calculate standard die defect rate to defect per unit area
        this.defectRatePerUnitArea = standardDieDefectRate / (Math.pow(64 * 16, 2));

        this.unlocalizedName = unlocalizedName;
    }

    public int getWaferArea() {
        return waferArea;
    }

    public int getWaferWidth() {
        return waferLen;
    }

    public int getWaferHeight() {
        return waferLen;
    }

    public double getDefectRatePerUnitArea() {
        return defectRatePerUnitArea;
    }

    public String getUnlocalizedName() {
        return unlocalizedName;
    }
}
