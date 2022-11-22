package mrtjp.projectred.fabrication.engine;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Transformation;
import mrtjp.fengine.TileCoord;
import mrtjp.fengine.tiles.FETile;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import java.util.Collections;
import java.util.List;

public abstract class BaseTile implements FETile {

    private final ICTileType tileType;

    private BaseTileMap map;
    private TileCoord pos;

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
        return map;
    }

    public ICWorkbenchEditor getEditor() {
        return map.getEditor();
    }

    public TileCoord getPos() {
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
            case 0:
                readDesc(in);
            default:
                // Unknown key
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

    public void onInteractionZoneClicked(int i) { }

    public void onInteractionZoneActivated(int i) { }

    @OnlyIn(Dist.CLIENT)
    public void buildToolTip(List<Component> toolTip) {
        toolTip.add(new TranslatableComponent(tileType.getUnlocalizedName()));
    }

    @OnlyIn(Dist.CLIENT)
    public void renderTile(CCRenderState ccrs, Transformation t, float partialFrame) { }

    @OnlyIn(Dist.CLIENT)
    public List<Cuboid6> getInteractionZones() {
        return Collections.emptyList();
    }

    @OnlyIn(Dist.CLIENT)
    public void buildInteractionToolTip(List<Component> toolTip, int i) { }
}
