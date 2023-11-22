package mrtjp.projectred.fabrication.engine;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.TileCoord;
import mrtjp.fengine.tiles.FETile;
import mrtjp.fengine.tiles.FETileMap;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;

import java.util.*;
import java.util.stream.Collectors;

import static mrtjp.projectred.fabrication.editor.EditorDataUtils.loadTileCoord;
import static mrtjp.projectred.fabrication.editor.EditorDataUtils.saveTileCoord;

public class BaseTileMap implements FETileMap {

    private static final TileCoord defaultMinBounds = new TileCoord(-8, 0, -8);
    private static final TileCoord defaultMaxBounds = new TileCoord(7, 0, 7);

    private final Map<TileCoord, BaseTile> tileMap = new HashMap<>();
    private final ICWorkbenchEditor editor;

    private final Set<IIOConnectionTile> ioTiles = new HashSet<>();

    private TileCoord minBounds = defaultMinBounds;
    private TileCoord maxBounds = defaultMaxBounds;

    public BaseTileMap(ICWorkbenchEditor editor) {
        this.editor = editor;
    }

    public ICWorkbenchEditor getEditor() {
        return editor;
    }

    public TileCoord getMinBounds() {
        return minBounds;
    }

    public TileCoord getMaxBounds() {
        return maxBounds;
    }

    public TileCoord getDimensions() {
        return maxBounds.subtract(minBounds).add(1, 1, 1);
    }

    public void setBounds(TileCoord minBounds, TileCoord maxBounds) {
        this.minBounds = minBounds;
        this.maxBounds = maxBounds;
    }

    public boolean isInBounds(TileCoord coord) {
        return coord.x >= minBounds.x && coord.x <= maxBounds.x &&
                coord.y >= minBounds.y && coord.y <= maxBounds.y &&
                coord.z >= minBounds.z && coord.z <= maxBounds.z;
    }

    public boolean addTile(TileCoord coord, BaseTile tile) {

        if (!tileMap.containsKey(coord)) {
            tileMap.put(coord, tile);
            tile.bindMap(this, coord);
            cacheType(tile);
            return true;
        }
        return false;
    }

    public Optional<BaseTile> removeTile(TileCoord coord) {

        BaseTile removed = tileMap.remove(coord);
        if (removed != null) {
            removed.unbindMap();
            uncacheType(removed);
        }
        return Optional.ofNullable(removed);
    }

    public Optional<BaseTile> getBaseTile(TileCoord coord) {
        return Optional.ofNullable(tileMap.get(coord));
    }

    public int getTileCount() {
        return tileMap.size();
    }

    public Collection<BaseTileEntry> getBaseTileEntries() {
        return tileMap.entrySet().stream()
                .map(e -> new BaseTileEntry(e.getKey(), e.getValue()))
                .collect(Collectors.toList());
    }

    // TODO keep track of tiles by layer
    public Collection<BaseTileEntry> getTilesOnLayer(int y) {
        return getBaseTileEntries().stream()
                .filter(t -> t.getCoord().y == y)
                .collect(Collectors.toList());
    }

    public Collection<IIOConnectionTile> getIOTiles() {
        return ioTiles;
    }

    public void removeAll() {
        ioTiles.clear();

        for (BaseTile t : tileMap.values()) t.unbindMap();
        tileMap.clear();

        minBounds = defaultMinBounds;
        maxBounds = defaultMaxBounds;
    }

    private void cacheType(BaseTile tile) {
        if (tile instanceof IIOConnectionTile) ioTiles.add((IIOConnectionTile) tile);
    }

    private void uncacheType(BaseTile tile) {
        if (tile instanceof IIOConnectionTile) ioTiles.remove(tile);
    }

    public InterfaceSpec getInterfaceSpec() {
        InterfaceSpec spec = new InterfaceSpec();
        spec.setFromIOTiles(ioTiles);
        return spec;
    }

    public void save(CompoundTag tag) {

        ListTag tileList = new ListTag();
        for (Map.Entry<TileCoord, BaseTile> entry : tileMap.entrySet()) {
            CompoundTag tileTag = new CompoundTag();
            tileTag.putByte("_id", (byte) entry.getValue().getTileType().getID());
            tileTag.putByte("_x", (byte) entry.getKey().x);
            tileTag.putByte("_y", (byte) entry.getKey().y);
            tileTag.putByte("_z", (byte) entry.getKey().z);
            entry.getValue().save(tileTag);
            tileList.add(tileTag);
        }
        tag.put("tiles", tileList);

        saveTileCoord(tag, "maxBounds", maxBounds);
        saveTileCoord(tag, "minBounds", minBounds);
    }

    public void load(CompoundTag tag) {
        removeAll();

        ListTag tileList = tag.getList("tiles", 10);
        for (int i = 0; i < tileList.size(); i++) {
            CompoundTag tileTag = tileList.getCompound(i);
            int id = tileTag.getByte("_id") & 0xFF;
            int x = tileTag.getByte("_x");
            int y = tileTag.getByte("_y");
            int z = tileTag.getByte("_z");
            BaseTile tile = ICTileType.createFromId(id);
            addTile(new TileCoord(x, y, z), tile);
            tile.load(tileTag);
        }

        maxBounds = loadTileCoord(tag, "maxBounds");
        minBounds = loadTileCoord(tag, "minBounds");
    }

    public void writeDesc(MCDataOutput out) {

        out.writeByte(minBounds.x).writeByte(minBounds.y).writeByte(minBounds.z);
        out.writeByte(maxBounds.x).writeByte(maxBounds.y).writeByte(maxBounds.z);

        for (Map.Entry<TileCoord, BaseTile> entry : tileMap.entrySet()) {
            out.writeByte(entry.getValue().getTileType().getID());
            out.writeByte(entry.getKey().x);
            out.writeByte(entry.getKey().y);
            out.writeByte(entry.getKey().z);
            entry.getValue().writeDesc(out);
        }
        out.writeByte(255);
    }

    public void readDesc(MCDataInput in) {
        removeAll();

        minBounds = new TileCoord(in.readByte(), in.readByte(), in.readByte());
        maxBounds = new TileCoord(in.readByte(), in.readByte(), in.readByte());

        int id = in.readUByte();
        while (id != 255) {
            BaseTile tile = ICTileType.createFromId(id);
            TileCoord coord = new TileCoord(in.readByte(), in.readByte(), in.readByte());
            addTile(coord, tile);
            tile.readDesc(in);

            id = in.readUByte();
        }
    }

    public static class BaseTileEntry {
        private final TileCoord coord;
        private final BaseTile tile;

        public BaseTileEntry(TileCoord coord, BaseTile tile) {
            this.coord = coord;
            this.tile = tile;
        }

        public BaseTile getTile() {
            return tile;
        }

        public TileCoord getCoord() {
            return coord;
        }
    }

    @Override
    public Optional<FETile> getTile(TileCoord coord) {
        return Optional.ofNullable(tileMap.get(coord));
    }

    @Override
    public Collection<TileMapEntry> getEntries() {

        return tileMap.entrySet().stream().map(e -> new TileMapEntry() {
            //@formatter:off
            @Override public TileCoord getCoord() { return e.getKey(); }
            @Override public FETile getTile() { return e.getValue(); }
            //@formatter:on
        }).collect(Collectors.toList());
    }
}