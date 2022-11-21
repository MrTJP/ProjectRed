package mrtjp.projectred.fabrication.gui;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.redui.AbstractGuiNode;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.network.chat.Component;

import java.util.List;

public class CompileTreeTab extends AbstractGuiNode implements ICompileOverlayRenderer {

    private final ICWorkbenchEditor editor;

    public CompileTreeTab(ICWorkbenchEditor editor) {
        this.editor = editor;
        setSize(91, 134);
        initSubNodes();
    }

    private void initSubNodes() {
    }

    //region ICompileTabOverlayRenderer
    @Override
    public void renderOverlay(ICRenderNode renderNode, Vector3 mousePosition, boolean isFirstHit, CCRenderState ccrs, MultiBufferSource getter, PoseStack matrixStack) {

    }

    @Override
    public void buildTooltip(ICRenderNode renderNode, Vector3 mousePosition, List<Component> tooltip) {

    }
    //endregion
}
