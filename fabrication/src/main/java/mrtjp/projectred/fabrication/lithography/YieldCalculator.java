package mrtjp.projectred.fabrication.lithography;

import mrtjp.projectred.lib.Size;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;

import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_DIMENSIONS_DIES_TOTAL;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_DIMENSIONS_NM;

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

    public Component getDieDimensionsText() {
        Size dieSize = getDieSize();
        return new TranslatableComponent(UL_DIMENSIONS_NM, dieSize.width, dieSize.height);
    }

    public Component getWaferDimensionsText() {
        return new TranslatableComponent(UL_DIMENSIONS_NM, waferType.getWaferWidth(), waferType.getWaferHeight());
    }

    public Component getDieCountDimensionsText() {
        Size dieCount = getDieCount();
        return new TranslatableComponent(UL_DIMENSIONS_DIES_TOTAL, dieCount.width, dieCount.height, dieCount.width * dieCount.height);
    }

    public Component getSingleLayerYieldText() {
        return new TextComponent(String.format("%.2f%%", getSingleLayerYield() * 100));
    }

    public Component getYieldText() {
        return new TextComponent(String.format("%.2f%%", getMultiLayerYield() * 100));
    }
}
