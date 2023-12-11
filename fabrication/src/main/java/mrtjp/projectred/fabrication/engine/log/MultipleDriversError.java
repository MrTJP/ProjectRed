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

import java.util.ArrayList;
import java.util.List;

import static mrtjp.projectred.fabrication.editor.ICWorkbenchEditor.UNIFORM_GRAY;
import static mrtjp.projectred.fabrication.engine.log.CompileProblemSeverity.ERROR;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_MULTIPLE_DRIVERS_DESC;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_MULTIPLE_DRIVERS_TITLE;

public class MultipleDriversError extends CompileProblem {

    public TileCoord coord = TileCoord.origin;
    public final List<Integer> registerList = new ArrayList<>();

    public MultipleDriversError() {
        super(CompileProblemType.MULTIPLE_DRIVERS, CompileProblemSeverity.ERROR);
    }

    public MultipleDriversError(TileCoord coord, List<Integer> registerList) {
        super(CompileProblemType.MULTIPLE_DRIVERS, CompileProblemSeverity.ERROR);
        this.coord = coord;
        this.registerList.addAll(registerList);
    }

    @Override
    public void save(CompoundTag tag) {
        tag.put("coord", EditorDataUtils.tileCoordToNBT(coord));
        tag.putIntArray("registers", registerList.stream().mapToInt(i -> i).toArray());
    }

    @Override
    public void load(CompoundTag tag) {
        coord = EditorDataUtils.tileCoordFromNBT(tag.getCompound("coord"));
        registerList.clear();
        for (int i : tag.getIntArray("registers")) {
            registerList.add(i);
        }
    }

    @Override
    public void writeDesc(MCDataOutput out) {
        out.writeByte(coord.x).writeByte(coord.y).writeByte(coord.z);
        out.writeShort(registerList.size());
        for (int i : registerList) {
            out.writeVarInt(i);
        }
    }

    @Override
    public void readDesc(MCDataInput in) {
        coord = new TileCoord(in.readByte(), in.readByte(), in.readByte());
        registerList.clear();
        int size = in.readUShort();
        for (int i = 0; i < size; i++) {
            registerList.add(in.readVarInt());
        }
    }

    @Override
    public Component getName() {
        return Component.translatable(UL_MULTIPLE_DRIVERS_TITLE);
    }

    @Override
    public void buildToolTip(List<Component> tooltip, TileCoord hoverPosition) {
        if (coord.equals(hoverPosition)) {
            buildToolTip(tooltip);
        }
    }

    @Override
    public void buildToolTip(List<Component> tooltip) {
        tooltip.add(Component.translatable(UL_MULTIPLE_DRIVERS_DESC).withStyle(UNIFORM_GRAY));

        StringBuilder s = new StringBuilder();
        for (int r : registerList) {
            s.append("R").append(r).append(", ");
        }
        s.delete(s.length() - 2, s.length()); // remove trailing comma
        tooltip.add(Component.literal("   ").withStyle(UNIFORM_GRAY).append(
                Component.literal(s.toString()).withStyle(UNIFORM_GRAY)));
    }

    @Override
    public void renderOverlay(Vector3 mousePosition, CCRenderState ccrs, MultiBufferSource getter, PoseStack matrixStack) {
        ccrs.baseColour = severity == ERROR ? EnumColour.RED.rgba(200) : EnumColour.YELLOW.rgba(200);
        Vector3 vec = new Vector3(coord.x, coord.y, coord.z).add(0.5);
        ICRenderTypes.renderSelection(ccrs, vec, vec, 3 / 16D, 2 / 16D);
    }
}
