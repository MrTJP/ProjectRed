package mrtjp.projectred.fabrication.editor;

import mrtjp.fengine.TileCoord;

public enum WorkbenchDimension {
    SIXTEEN(new TileCoord(-8, 0, -8), new TileCoord(7, 0, 7), "16x16"),
    TWENTY_FOUR(new TileCoord(-12, 0, -12), new TileCoord(11, 0, 11), "24x24"),
    THIRTY_TWO(new TileCoord(-16, 0, -16), new TileCoord(15, 0, 15), "32x32"),
    FORTY_EIGHT(new TileCoord(-24, 0, -24), new TileCoord(23, 0, 23), "48x48");

    private final TileCoord minBounds;
    private final TileCoord maxBounds;
    private final String unlocalizedName;

    WorkbenchDimension(TileCoord minBounds, TileCoord maxBounds, String unlocalizedName) {
        this.minBounds = minBounds;
        this.maxBounds = maxBounds;
        this.unlocalizedName = unlocalizedName;
    }

    public TileCoord getMinBounds() {
        return minBounds;
    }

    public TileCoord getMaxBounds() {
        return maxBounds;
    }

    public String getUnlocalizedName() {
        return unlocalizedName;
    }
}
