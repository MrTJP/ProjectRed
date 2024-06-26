package mrtjp.projectred.fabrication.engine.log;

import codechicken.lib.data.MCDataInput;
import codechicken.lib.data.MCDataOutput;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.fengine.TileCoord;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;

import java.util.List;

import static mrtjp.projectred.fabrication.editor.ICWorkbenchEditor.UNIFORM_GRAY;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_NO_INPUTS_DESC;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_NO_INPUTS_TITLE;

public class NoInputsError extends CompileProblem {

    public NoInputsError() {
        super(CompileProblemType.NO_INPUTS, CompileProblemSeverity.ERROR);
    }

    @Override
    public void save(CompoundTag tag) {
    }

    @Override
    public void load(CompoundTag tag) {
    }

    @Override
    public void writeDesc(MCDataOutput out) {
    }

    @Override
    public void readDesc(MCDataInput in) {
    }

    @Override
    public Component getName() {
        return Component.translatable(UL_NO_INPUTS_TITLE);
    }

    @Override
    public void buildToolTip(List<Component> tooltip, TileCoord hoverPosition) {

    }

    @Override
    public void buildToolTip(List<Component> tooltip) {
        tooltip.add(Component.translatable(UL_NO_INPUTS_DESC).withStyle(UNIFORM_GRAY));
    }

    @Override
    public void renderOverlay(Vector3 mousePosition, CCRenderState ccrs, MultiBufferSource getter, PoseStack matrixStack) {

    }
}
