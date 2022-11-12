package mrtjp.projectred.fabrication.lithography;

import mrtjp.projectred.lib.Size;

public class YieldCalculator {

    private int tileMapWidth;
    private int tileMapHeight;
    private int tileMapLayers;

    private LithographyPipeline pipeline = LithographyPipeline.BASIC;
    private ProcessNode processNode = ProcessNode.PROCESS_64NM;
    private WaferType waferType = WaferType.ROUGH_WAFER;

    public LithographyPipeline getPipeline() {
        return pipeline;
    }

    public void setPipeline(LithographyPipeline pipeline) {
        this.pipeline = pipeline;
    }

    public ProcessNode getProcessNode() {
        return processNode;
    }

    public void setProcessNode(ProcessNode processNode) {
        this.processNode = processNode;
    }

    public WaferType getWaferType() {
        return waferType;
    }

    public void setWaferType(WaferType waferType) {
        this.waferType = waferType;
    }

    public void setTileMapSize(int width, int height, int layers) {
        this.tileMapWidth = width;
        this.tileMapHeight = height;
        this.tileMapLayers = layers;
    }

    public Size getTileMapSize() {
        return new Size(tileMapWidth, tileMapHeight);
    }

    public Size getDieSize() {
        return new Size(tileMapWidth * processNode.getTileWidth(), tileMapHeight * processNode.getTileHeight());
    }

    public Size getWaferSize() {
        return new Size(waferType.getWaferWidth(), waferType.getWaferHeight());
    }

    public Size getDieCount() {
        return getWaferSize().divide(getDieSize());
    }

    public double getSingleLayerYield() {
        Size dieSize = getDieSize();
        return 1.0 - waferType.getDefectRatePerUnitArea() * dieSize.width * dieSize.height;
    }

    public double getMultiLayerYield() {
        return Math.pow(getSingleLayerYield(), tileMapLayers);
    }

    public String getDieSizeString() {
        Size dieSize = getDieSize();
        return String.format("%d nm x %d nm", dieSize.width, dieSize.height);
    }

    public String getWaferSizeString() {
        return String.format("%d nm x %d nm", waferType.getWaferWidth(), waferType.getWaferHeight());
    }

    public String getDiesPerWaferString() {
        Size dieCount = getDieCount();
        return String.format("%d dies x %d dies (%d total)", dieCount.width, dieCount.height, dieCount.width * dieCount.height);
    }

    public String getSingleLayerYieldString() {
        return String.format("%.2f%%", getSingleLayerYield() * 100);
    }

    public String getYieldString() {
        return String.format("%.2f%%", getMultiLayerYield() * 100);
    }
}
