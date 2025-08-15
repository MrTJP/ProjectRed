package mrtjp.projectred.fabrication.engine;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.TileCoord;
import mrtjp.fengine.tiles.FETile;
import mrtjp.fengine.tiles.FETileMap;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.editor.WorkbenchDimension;
import net.covers1624.quack.collection.FastStream;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.ListTag;

import java.util.*;

import static mrtjp.projectred.fabrication.editor.EditorDataUtils.loadTileCoord;
import static mrtjp.projectred.fabrication.editor.EditorDataUtils.saveTileCoord;

public class BaseTileMap implements FETileMap {
    private final Map<TileCoord, BaseTile> tileMap = new HashMap<>();
    private final ICWorkbenchEditor editor;

    private final Set<IIOConnectionTile> ioTiles = new HashSet<>();
    private final Map<Integer, Map<TileCoord, BaseTile>> layerToTileMap = new HashMap<>();

    private TileCoord minBounds = WorkbenchDimension.SIXTEEN.getMinBounds();
    private TileCoord maxBounds = WorkbenchDimension.SIXTEEN.getMaxBounds();

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

    public String getDimensionsString() {
        return String.format("%d x %d x %d", getDimensions().x, getDimensions().y, getDimensions().z);
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
            cacheType(coord, tile);
            return true;
        }
        return false;
    }

    public Optional<BaseTile> removeTile(TileCoord coord) {

        BaseTile removed = tileMap.remove(coord);
        if (removed != null) {
            removed.unbindMap();
            uncacheType(coord, removed);
        }
        return Optional.ofNullable(removed);
    }

    public Optional<BaseTile> getBaseTile(TileCoord coord) {
        return Optional.ofNullable(tileMap.get(coord));
    }

    public int getTileCount() {
        return tileMap.size();
    }

    public Collection<Map.Entry<TileCoord, BaseTile>> getBaseTileEntries() {
        return tileMap.entrySet();
    }

    public Collection<Map.Entry<TileCoord, BaseTile>> getTilesOnLayer(int y) {
        return layerToTileMap.computeIfAbsent(y, k -> new HashMap<>()).entrySet();
    }

    public Collection<IIOConnectionTile> getIOTiles() {
        return ioTiles;
    }

    public void removeAll() {
        ioTiles.clear();

        for (BaseTile t : tileMap.values()) t.unbindMap();
        tileMap.clear();
        layerToTileMap.clear();

        minBounds = WorkbenchDimension.SIXTEEN.getMinBounds();
        maxBounds = WorkbenchDimension.SIXTEEN.getMaxBounds();
    }

    private void cacheType(TileCoord coord, BaseTile tile) {
        // Type caching
        if (tile instanceof IIOConnectionTile) ioTiles.add((IIOConnectionTile) tile);

        // Layer caching
        layerToTileMap.computeIfAbsent(coord.y, k -> new HashMap<>()).put(coord, tile);
    }

    private void uncacheType(TileCoord coord, BaseTile tile) {
        // Type caching
        if (tile instanceof IIOConnectionTile) ioTiles.remove(tile);

        // Layer caching
        layerToTileMap.get(coord.y).remove(coord);
    }

    public InterfaceSpec getInterfaceSpec() {
        InterfaceSpec spec = new InterfaceSpec();
        spec.setFromIOTiles(ioTiles);
        return spec;
    }

    public void save(CompoundTag tag) {
        tag.putByte("format", (byte) ICTileType.ID_MAP_FORMAT);
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
        int format = tag.getByte("format") & 0xFF;
        ListTag tileList = tag.getList("tiles", 10);
        for (int i = 0; i < tileList.size(); i++) {
            CompoundTag tileTag = tileList.getCompound(i);
            int id = tileTag.getByte("_id") & 0xFF;
            int x = tileTag.getByte("_x");
            int y = tileTag.getByte("_y");
            int z = tileTag.getByte("_z");
            BaseTile tile = Objects.requireNonNull(ICTileType.createFromIdAndFormat(id, format));
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
            BaseTile tile = Objects.requireNonNull(ICTileType.createFromId(id));
            TileCoord coord = new TileCoord(in.readByte(), in.readByte(), in.readByte());
            addTile(coord,tile);
            tile.readDesc(in);

            id = in.readUByte();
        }
    }

    @Override
    public Optional<FETile> getTile(TileCoord coord) {
        return Optional.ofNullable(tileMap.get(coord));
    }

    @Override
    public Collection<TileMapEntry> getEntries() {

        return FastStream.of(tileMap.entrySet()).map(e -> (TileMapEntry) new TileMapEntry() {
            //@formatter:off
            @Override public TileCoord getCoord() { return e.getKey(); }
            @Override public FETile getTile() { return e.getValue(); }
            //@formatter:on
        }).toImmutableList();
    }
}
