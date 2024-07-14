package mrtjp.projectred.fabrication.lithography;

import static mrtjp.projectred.fabrication.init.FabricationUnlocal.*;

public enum WaferType {

    //@formatter:off
    ROUGH_WAFER     (4096, 0.5, UL_WAFER_TYPE_ROUGH),
    PURIFIED_WAFER  (4096, 0.1, UL_WAFER_TYPE_PURIFIED),
    POLISHED_WAFER  (4096, 0.01, UL_WAFER_TYPE_POLISHED);
    //@formatter:on

    private final String localizedNameKey;
    private final int    waferLen;
    private final int    waferArea;
    private final double defectRatePerUnitArea;

    // Note: standard die defect rate is rate of defect of 16x16 design in 64nm process
    WaferType(int waferLen, double standardDieDefectRate, String localizedNameKey) {
        this.waferLen = waferLen;
        this.waferArea = waferLen * waferLen; // square wafers only atm

        // back-calculate standard die defect rate to defect per unit area
        this.defectRatePerUnitArea = standardDieDefectRate / (Math.pow(64 * 16, 2));

        this.localizedNameKey = localizedNameKey;
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

    public String getLocalizedNameKey() {
        return this.localizedNameKey;
    }
}
