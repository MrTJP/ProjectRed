package mrtjp.projectred.fabrication.engine.log;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.EditorDataUtils;
import mrtjp.projectred.fabrication.gui.ICRenderTypes;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;

import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

import static mrtjp.projectred.fabrication.engine.log.CompileProblemSeverity.ERROR;

public abstract class MultiPositionProblem extends CompileProblem {

    public final List<TileCoord> coordList = new LinkedList<>();

    public MultiPositionProblem(CompileProblemType type, CompileProblemSeverity severity) {
        super(type, severity);
    }

    public MultiPositionProblem(CompileProblemType type, CompileProblemSeverity severity, Collection<TileCoord> coordList) {
        this(type, severity);
        this.coordList.addAll(coordList);
    }

    @Override
    public void save(CompoundTag tag) {
        EditorDataUtils.saveTileCoordList(tag, "coordList", coordList);
    }

    @Override
    public void load(CompoundTag tag) {
        coordList.clear();
        EditorDataUtils.loadTileCoordList(tag, "coordList", coordList);
    }

    @Override
    public void writeDesc(MCDataOutput out) {
        out.writeShort(coordList.size());
        for (TileCoord coord : coordList) {
            out.writeByte(coord.x).writeByte(coord.y).writeByte(coord.z);
        }
    }

    @Override
    public void readDesc(MCDataInput in) {
        coordList.clear();
        int size = in.readUShort();
        for (int i = 0; i < size; i++) {
            coordList.add(new TileCoord(in.readByte(), in.readByte(), in.readByte()));
        }
    }

    @Override
    public void buildToolTip(List<Component> tooltip, TileCoord hoverPosition) {
        if (coordList.contains(hoverPosition)) {
            buildToolTip(tooltip);
        }
    }

    @Override
    public void renderOverlay(Vector3 mousePosition, CCRenderState ccrs, MultiBufferSource getter, PoseStack matrixStack) {
        ccrs.baseColour = severity == ERROR ? EnumColour.RED.rgba(200) : EnumColour.YELLOW.rgba(200);
        Vector3 vec = new Vector3();
        for (TileCoord coord : coordList) {
            vec.set(coord.x, coord.y, coord.z).add(0.5);
            ICRenderTypes.renderSelection(ccrs, vec, vec, 3 / 16D, 2 / 16D);
        }
    }
}
