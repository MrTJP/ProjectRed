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

import java.util.List;

import static mrtjp.projectred.fabrication.engine.log.CompileProblemSeverity.ERROR;

public abstract class SimpleLocatableProblem extends CompileProblem {

    public TileCoord coord = TileCoord.origin;

    public SimpleLocatableProblem(CompileProblemType type, CompileProblemSeverity severity) {
        super(type, severity);
    }

    public SimpleLocatableProblem(CompileProblemType type, CompileProblemSeverity severity, TileCoord coord) {
        this(type, severity);
        this.coord = coord;
    }

    @Override
    public void save(CompoundTag tag) {
        tag.put("coord", EditorDataUtils.tileCoordToNBT(coord));
    }

    @Override
    public void load(CompoundTag tag) {
        coord = EditorDataUtils.tileCoordFromNBT(tag.getCompound("coord"));
    }

    @Override
    public void writeDesc(MCDataOutput out) {
        out.writeByte(coord.x).writeByte(coord.y).writeByte(coord.z);
    }

    @Override
    public void readDesc(MCDataInput in) {
        coord = new TileCoord(in.readByte(), in.readByte(), in.readByte());
    }

    @Override
    public void buildToolTip(List<Component> tooltip, TileCoord hoverPosition) {
        if (coord.equals(hoverPosition)) {
            buildToolTip(tooltip);
        }
    }

    @Override
    public void renderOverlay(Vector3 mousePosition, CCRenderState ccrs, MultiBufferSource getter, PoseStack matrixStack) {
        ccrs.baseColour = severity == ERROR ? EnumColour.RED.rgba(200) : EnumColour.YELLOW.rgba(200);
        Vector3 vec = new Vector3(coord.x, coord.y, coord.z).add(0.5);
        ICRenderTypes.renderSelection(ccrs, vec, vec, 3 / 16D, 2 / 16D);
    }
}
