package mrtjp.projectred.fabrication.engine;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Transformation;
import mrtjp.fengine.TileCoord;
import mrtjp.fengine.tiles.FETile;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.editor.tools.InteractionZone;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.neoforged.api.distmarker.Dist;
import net.neoforged.api.distmarker.OnlyIn;

import javax.annotation.Nullable;
import java.util.LinkedList;
import java.util.List;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.LOGGER;

public abstract class BaseTile implements FETile {

    private final ICTileType tileType;

    private @Nullable BaseTileMap map;
    private @Nullable TileCoord pos;

    private @Nullable InteractionZone[] interactionZones;

    public BaseTile(ICTileType tileType) {
        this.tileType = tileType;
    }

    public void bindMap(BaseTileMap map, TileCoord pos) {
        this.map = map;
        this.pos = pos;
    }

    public void unbindMap() {
        map = null;
        pos = null;
    }

    public BaseTileMap getMap() {
        assert map != null;
        return map;
    }

    public ICWorkbenchEditor getEditor() {
        assert map != null;
        return map.getEditor();
    }

    public TileCoord getPos() {
        assert pos != null;
        return pos;
    }

    public ICTileType getTileType() {
        return tileType;
    }

    public abstract void save(CompoundTag tag);

    public abstract void load(CompoundTag tag);

    public abstract void writeDesc(MCDataOutput out);

    public abstract void readDesc(MCDataInput in);

    public MCDataOutput getWriteStream(int key) {
        return getEditor().getTileStream(this, key);
    }

    public void read(MCDataInput in, int key) {
        switch (key) {
            case 0 -> readDesc(in);
            default -> LOGGER.error("Invalid key: " + key);
        }
    }

    public void sendDescUpdate() {
        writeDesc(getWriteStream(0));
    }

    public void update() { }

    public void onAdded() { }

    public void onRemoved() { }

    public void onNeighborChanged() { }

    public void onSimRegistersChanged(int rMask, ICSimulationContainer container) { }

    public final InteractionZone[] getInteractionZones() {
        if (interactionZones == null) {
            LinkedList<InteractionZone> zoneList = new LinkedList<>();
            buildInteractionZoneList(zoneList);
            interactionZones = zoneList.toArray(new InteractionZone[0]);
        }
        return interactionZones;
    }

    public void buildInteractionZoneList(List<InteractionZone> zones) {
    }

    public void buildToolTip(List<Component> toolTip) {
        toolTip.add(Component.translatable(tileType.getUnlocalizedName()));
    }

    @OnlyIn(Dist.CLIENT)
    public void renderTile(CCRenderState ccrs, Transformation t, float partialFrame) { }
}
