package mrtjp.projectred.fabrication.editor;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.tools.IICEditorTool;
import mrtjp.projectred.fabrication.engine.BaseTileMap;
import mrtjp.projectred.fabrication.engine.ICSimulationContainer;
import mrtjp.projectred.fabrication.engine.ICTileType;
import mrtjp.projectred.fabrication.engine.BaseTile;
import net.minecraft.nbt.CompoundNBT;

import javax.annotation.Nullable;
import java.util.*;

import static mrtjp.projectred.ProjectRedFabrication.LOGGER;

public class ICWorkbenchEditor implements ICEditorStateMachine.StateMachineCallback {

    private static final int STREAM_ID_GENERAL = 0;
    private static final int STREAM_ID_TILE_UPDATES = 1;
    private static final int STREAM_ID_COMPILER = 2;

    private static final int ADD_TILE = 1;
    private static final int REMOVE_TILE = 2;
    private static final int KEY_TOOL = 10;

    private final IICWorkbenchEditorNetwork network;
    private final BaseTileMap tileMap = new BaseTileMap(this);
    private final ICEditorStateMachine stateMachine = new ICEditorStateMachine(this, this);

    private final ArrayList<IICEditorTool> toolList = ICEditorToolType.createToolList();
    private final List<TileCoord> neighborChangeList = new LinkedList<>();

    private boolean isActive = false;
    private String icName = "untitled";

    public ICWorkbenchEditor(IICWorkbenchEditorNetwork network) {
        this.network = network;
        for (IICEditorTool t : toolList) t.bindEditor(this);
    }

    public ArrayList<IICEditorTool> getToolList() {
        return toolList;
    }

    public BaseTileMap getTileMap() {
        return tileMap;
    }

    public ICEditorStateMachine getStateMachine() {
        return stateMachine;
    }

    public boolean isActive() {
        return isActive;
    }

    //region ICWorkbenchTile utilities
    public void save(CompoundNBT tag) {
        LOGGER.info("ICWorkbenchEditor: saving to NBT");
        tag.putBoolean("active", isActive);
        tag.putString("ic_name", icName);
        tileMap.save(tag);
        stateMachine.save(tag);
    }

    public void load(CompoundNBT tag) {
        LOGGER.info("ICWorkbenchEditor: reading form NBT");
        isActive = tag.getBoolean("active");
        icName = tag.getString("ic_name");
        tileMap.load(tag);
        stateMachine.load(tag);
    }

    public void writeDesc(MCDataOutput out) {
        LOGGER.info("ICWorkbenchEditor: writing description");
        out.writeBoolean(isActive);
        out.writeString(icName);
        tileMap.writeDesc(out);
        stateMachine.writeDesc(out);
    }

    public void readDesc(MCDataInput in) {
        LOGGER.info("ICWorkbenchEditor: Reading description");
        isActive = in.readBoolean();
        icName = in.readString();
        tileMap.readDesc(in);
        stateMachine.readDesc(in);
    }

    private void clear() {
        LOGGER.info("ICWorkbenchEditor: Preparing load of initial data (Should be server only)");
        tileMap.removeAll();
//        compiler.clear();
    }

    public void readBlueprintTagAndActivate(@Nullable CompoundNBT tag) {

        clear();
        isActive = true;
        if (tag == null) {
            LOGGER.info("ICWorkbenchEditor: No blueprint tag found. Activating fresh session");
            return;
        }

        LOGGER.info("ICWorkbenchEditor: Reading blueprint tag");
        icName = tag.getString("ic_name");
        tileMap.load(tag);
        stateMachine.load(tag);
    }

    public void writeBlueprintTagAndDeactivate(CompoundNBT tag) {
        LOGGER.info("ICWorkbenchEditor: Writing blueprint tag");
        isActive = false;
        tag.putString("ic_name", icName);
        tileMap.save(tag);
        stateMachine.save(tag);
        tag.putInt("tilecount", tileMap.getTileCount()); //TODO better tooltips
        clear();
    }
    //endregion

    public void readBufferedStream(MCDataInput in, int streamKey, int frameKey) {
        switch (streamKey) {
            case STREAM_ID_GENERAL:
                readGeneralStream(in, frameKey);
                break;
            case STREAM_ID_TILE_UPDATES:
                readTileStream(in, frameKey);
                break;
            case STREAM_ID_COMPILER:
                stateMachine.readCompilerStream(in, frameKey);
                break;
            default:
                LOGGER.error("Unknown stream key " + streamKey);
        }
    }

    private void readTileStream(MCDataInput in, int frameKey) {

        TileCoord positionToUpdate = new TileCoord(in.readByte(), in.readByte(), in.readByte());
        int key = in.readUByte();
        Optional<BaseTile> tileToUpdate = tileMap.getBaseTile(positionToUpdate);
        if (!tileToUpdate.isPresent() || tileToUpdate.get().getTileType().getID() != frameKey) {
            LOGGER.error("Tile Update error: No tile with id " + frameKey + " at position " + positionToUpdate + ". Reading into temporary tile");
            BaseTile tmp = ICTileType.createFromId(frameKey);
            if (tmp == null) {
                LOGGER.error("Unknown tile id " + frameKey + " in tile update stream");
            } else {
                tmp.read(in, key);
            }
            return;
        }
        tileToUpdate.get().read(in, key);
    }

    private void readGeneralStream(MCDataInput in, int frameKey) {
        switch (frameKey) {
            case ADD_TILE:
                BaseTile tile = ICTileType.createFromId(in.readUByte()); //TODO check if not null?
                tileMap.addTile(new TileCoord(in.readByte(), in.readByte(), in.readByte()), tile);
                tile.readDesc(in);
                break;
            case REMOVE_TILE:
                tileMap.removeTile(new TileCoord(in.readByte(), in.readByte(), in.readByte())); //TODO check if removed?
                break;
            case KEY_TOOL:
                toolList.get(in.readUByte()).readPacket(in);
                break;
            default:
                LOGGER.error("Unknown key " + frameKey + " in general stream");
        }
    }

    public void tick() {

        // Alert neighbors of changes if necessary
        if (!neighborChangeList.isEmpty()) {
            Queue<TileCoord> changesRemaining = new LinkedList<>(neighborChangeList);
            neighborChangeList.clear();

            Set<TileCoord> tilesNotified = new HashSet<>();
            TileCoord next = changesRemaining.poll();
            while (next != null) {
                if (!tilesNotified.contains(next)) {
                    tilesNotified.add(next);
                    Optional<BaseTile> tile = tileMap.getBaseTile(next);
                    tile.ifPresent(BaseTile::onNeighborChanged);
                }
                next = changesRemaining.poll();
            }
        }

        // Run compiler or simulator
        if (!network.isClientSide()) stateMachine.onTick(network.getGameTime());
    }

    public MCDataOutput getToolStream(IICEditorTool tool) {
        MCDataOutput out = network.getBufferedStream(STREAM_ID_GENERAL, KEY_TOOL);
        out.writeByte(tool.getToolType().ordinal());
        return out;
    }

    public MCDataOutput getTileStream(BaseTile tile, int key) {
        // Use tile ID as frame key
        MCDataOutput out = network.getBufferedStream(STREAM_ID_TILE_UPDATES, tile.getTileType().getID());
        out.writeByte(tile.getPos().x).writeByte(tile.getPos().y).writeByte(tile.getPos().z);
        out.writeByte(key);
        return out;
    }

    public MCDataOutput getCompilerStream(int key) {
        return network.getBufferedStream(STREAM_ID_COMPILER, key);
    }

    public void addTile(BaseTile tile, TileCoord pos) {
        if (network.isClientSide()) throw new RuntimeException("Tiles can only be added server-side");

        if (!tileMap.addTile(pos, tile)) {
            LOGGER.error("Failed to add tile to pos " + pos);
            return;
        }
        tile.onAdded();

        // Send the new tile to clients
        MCDataOutput out = network.getBufferedStream(STREAM_ID_GENERAL, ADD_TILE);
        out.writeByte(tile.getTileType().getID());
        out.writeByte(pos.x).writeByte(pos.y).writeByte(pos.z);
        tile.writeDesc(out);

        markTileChange();
    }

    public void removeTile(TileCoord pos) {
        if (network.isClientSide()) throw new RuntimeException("Tiles can only be removed server-side");

        Optional<BaseTile> tileToRemove = tileMap.getBaseTile(pos);
        if (!tileToRemove.isPresent()) {
            LOGGER.error("No tile present to remove at pos " + pos);
            return;
        }
        tileToRemove.get().onRemoved();
        tileMap.removeTile(pos);

        // Send the tile removed action to clients
        MCDataOutput out = network.getBufferedStream(STREAM_ID_GENERAL, REMOVE_TILE);
        out.writeByte(pos.x).writeByte(pos.y).writeByte(pos.z);

        markTileChange();
    }

    public void queueNeighborChange(TileCoord pos) {
        neighborChangeList.add(pos);
    }

    public void markTileChange() {
        network.markSave();
        stateMachine.onTileMapChanged();
    }

    @Override
    public void onCompileStart() {
        LOGGER.info("Compiling...");
    }

    @Override
    public void onCompileComplete() {
        LOGGER.info("Compilation complete");
    }

    @Override
    public void onSimulationComplete(int changeMask, ICSimulationContainer container) {
//        LOGGER.info("Simulation complete. Change mask: " + changeMask);
        for (BaseTileMap.BaseTileEntry entry : tileMap.getBaseTileEntries()) {
            entry.getTile().onSimRegistersChanged(changeMask, container);
        }
    }
}
