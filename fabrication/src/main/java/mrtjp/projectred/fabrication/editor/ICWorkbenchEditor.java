package mrtjp.projectred.fabrication.editor;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.tools.IICEditorTool;
import mrtjp.projectred.fabrication.engine.BaseTile;
import mrtjp.projectred.fabrication.engine.BaseTileMap;
import mrtjp.projectred.fabrication.engine.ICSimulationContainer;
import mrtjp.projectred.fabrication.engine.ICTileType;
import mrtjp.projectred.fabrication.item.component.BlueprintDataComponent;
import mrtjp.projectred.fabrication.item.component.ICDataComponent;
import net.minecraft.ChatFormatting;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Style;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;

import java.util.*;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;
import static mrtjp.projectred.fabrication.editor.EditorDataUtils.*;

public class ICWorkbenchEditor implements ICEditorStateMachine.StateMachineCallback {

    public static final Style UNIFORM = Style.EMPTY.withFont(ResourceLocation.fromNamespaceAndPath("minecraft", "uniform"));
    public static final Style UNIFORM_DARK_GRAY = UNIFORM.withColor(ChatFormatting.DARK_GRAY);
    public static final Style UNIFORM_GRAY = UNIFORM.withColor(ChatFormatting.GRAY);
    public static final Style UNIFORM_RED = UNIFORM.withColor(ChatFormatting.RED);
    public static final Style UNIFORM_YELLOW = UNIFORM.withColor(ChatFormatting.YELLOW);

    public static final int EDITOR_FORMAT = 1;

    private static final int STREAM_ID_GENERAL = 0;
    private static final int STREAM_ID_TILE_UPDATES = 1;
    private static final int STREAM_ID_FSM = 2;

    // Server to client keys
    private static final int KEY_ADD_TILE = 1;
    private static final int KEY_REMOVE_TILE = 2;
    private static final int KEY_SET_IC_NAME = 3;

    // Client to server keys
    private static final int KEY_TOOL = 10;
    private static final int KEY_CLIENT_SEND_IC_NAME = 11;

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

    public String getIcName() {
        return icName;
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

    public boolean isClientSide() {
        return network.isClientSide();
    }

    //region ICWorkbenchTile utilities
    public void save(CompoundTag tag) {
        LOGGER.debug("ICWorkbenchEditor: saving to NBT");
        tag.putInt(KEY_FORMAT, EDITOR_FORMAT);
        tag.putBoolean(KEY_ACTIVE, isActive);
        tag.putString(KEY_IC_NAME, icName);

        CompoundTag tileMapTag = new CompoundTag();
        tileMap.save(tileMapTag);
        tag.put(KEY_TILE_MAP, tileMapTag);

        stateMachine.save(tag);
    }

    public void load(CompoundTag tag) {
        isActive = tag.getBoolean(KEY_ACTIVE);
        icName = tag.getString(KEY_IC_NAME);
        tileMap.load(tag.getCompound(KEY_TILE_MAP));
        stateMachine.load(tag);
    }

    public void writeDesc(MCDataOutput out) {
        out.writeBoolean(isActive);
        out.writeString(icName);
        tileMap.writeDesc(out);
        stateMachine.writeDesc(out);
    }

    public void readDesc(MCDataInput in) {
        isActive = in.readBoolean();
        icName = in.readString();
        tileMap.readDesc(in);
        stateMachine.readDesc(in);
    }

    public void onChunkLoad() {
        stateMachine.onChunkLoad();
    }

    private void clear() {
        tileMap.removeAll();
        stateMachine.reset();
        icName = "untitled";
    }

    public void readBlueprintTagAndActivate(ItemStack stack) {
        // Clear the session
        clear();

        var editorData = BlueprintDataComponent.getComponent(stack);

        // Load editor data
        if (editorData != null) {
            load(editorData.getEditorTag());
        }

        // Activate editor
        isActive = true;
    }

    public void writeBlueprintTagAndDeactivate(ItemStack stack) {
        // Save all editor contents
        var tag = new CompoundTag();
        save(tag);

        // Create design component
        var designData = ICDataComponent.builder()
                .setName(icName)
                .setTileCount(tileMap.getTileCount())
                .setWarningCount(stateMachine.getCompilerLog().getWarningCount())
                .setErrorCount(stateMachine.getCompilerLog().getErrorCount())
                .setBuilt(stateMachine.isSimulating())
                .setCompileFormat(stateMachine.getLastCompiledFormat())
                .setInterfaceSpec(tileMap.getInterfaceSpec())
                .setFlatMap(stateMachine.getLastCompiledFlatMap())
                .build();

        // Save blueprint data to item stack
        BlueprintDataComponent.setComponent(stack,
                new BlueprintDataComponent(tag, designData));

        // Deactivate editor
        isActive = false;
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
            case STREAM_ID_FSM:
                stateMachine.readStateMachineStream(in, frameKey);
                break;
            default:
                LOGGER.error("Unknown stream key " + streamKey);
        }
    }

    private void readTileStream(MCDataInput in, int frameKey) {

        TileCoord positionToUpdate = new TileCoord(in.readByte(), in.readByte(), in.readByte());
        int key = in.readUByte();
        Optional<BaseTile> tileToUpdate = tileMap.getBaseTile(positionToUpdate);
        if (tileToUpdate.isEmpty() || tileToUpdate.get().getTileType().getID() != frameKey) {
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
            case KEY_ADD_TILE:
                BaseTile tile = Objects.requireNonNull(ICTileType.createFromId(in.readUByte()));
                tileMap.addTile(new TileCoord(in.readByte(), in.readByte(), in.readByte()), tile);
                tile.readDesc(in);
                break;
            case KEY_REMOVE_TILE:
                tileMap.removeTile(new TileCoord(in.readByte(), in.readByte(), in.readByte())); //TODO check if removed?
                break;
            case KEY_SET_IC_NAME:
                icName = in.readString();
                break;
            case KEY_TOOL:
                toolList.get(in.readUByte()).readPacket(in);
                break;
            case KEY_CLIENT_SEND_IC_NAME:
                //TODO validate name
                icName = in.readString();
                network.getBufferedStream(STREAM_ID_GENERAL, KEY_SET_IC_NAME).writeString(icName);
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

    public MCDataOutput getStateMachineStream(int key) {
        return network.getBufferedStream(STREAM_ID_FSM, key);
    }

    public long getGameTime() {
        return network.getGameTime();
    }

    //region Server Utils
    public void addTile(BaseTile tile, TileCoord pos) {
        if (network.isClientSide()) throw new RuntimeException("Tiles can only be added server-side");

        if (!tileMap.addTile(pos, tile)) {
            LOGGER.error("Failed to add tile to pos " + pos);
            return;
        }
        tile.onAdded();

        // Send the new tile to clients
        MCDataOutput out = network.getBufferedStream(STREAM_ID_GENERAL, KEY_ADD_TILE);
        out.writeByte(tile.getTileType().getID());
        out.writeByte(pos.x).writeByte(pos.y).writeByte(pos.z);
        tile.writeDesc(out);

        markTileChange();
    }

    public void removeTile(TileCoord pos) {
        if (network.isClientSide()) throw new RuntimeException("Tiles can only be removed server-side");

        Optional<BaseTile> tileToRemove = tileMap.getBaseTile(pos);
        if (tileToRemove.isEmpty()) {
            LOGGER.error("No tile present to remove at pos " + pos);
            return;
        }
        tileToRemove.get().onRemoved();
        tileMap.removeTile(pos);

        // Send the tile removed action to clients
        MCDataOutput out = network.getBufferedStream(STREAM_ID_GENERAL, KEY_REMOVE_TILE);
        out.writeByte(pos.x).writeByte(pos.y).writeByte(pos.z);

        markTileChange();
    }

    public void queueNeighborChange(TileCoord pos) {
        neighborChangeList.add(pos);
    }

    public void markTileChange() {
        network.markSave();
        if (!network.isClientSide()) {
            stateMachine.onTileMapChanged();
        }
    }

    public void markDirty() {
        network.markSave();
    }
    //endregion

    //region Client Utils
    public void sendNewICName(String name) {
        // Notifies server to set a new IC name
        network.getBufferedStream(STREAM_ID_GENERAL, KEY_CLIENT_SEND_IC_NAME).writeString(name);
    }
    //endregion

    //region State Machine callbacks
    @Override
    public void onCompileStart() {
        LOGGER.debug("Compiling...");
    }

    @Override
    public void onCompileComplete() {
        LOGGER.debug("Compilation complete");
    }

    @Override
    public void onCompileFailed() {
        LOGGER.debug("Compilation failed");
    }

    @Override
    public void onSimulationComplete(int changeMask, ICSimulationContainer container) {
        for (var entry : tileMap.getBaseTileEntries()) {
            entry.getValue().onSimRegistersChanged(changeMask, container);
        }
    }
    //endregion
}
