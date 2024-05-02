package mrtjp.projectred.fabrication.gui;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.engine.log.ICCompilerLog;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchCompileTab;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.lib.Rect;
import mrtjp.projectred.redui.AbstractGuiNode;
import mrtjp.projectred.redui.ScrollBarNode;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.network.chat.Component;

import java.util.List;

public class CompileStackTab extends AbstractGuiNode implements ICompileOverlayRenderer {

    private final ICWorkbenchEditor editor;

    private final CTNListNode ctnListNode = new CTNListNode();

    public CompileStackTab(ICWorkbenchEditor editor) {
        this.editor = editor;

        setSize(91, 134);
        initSubNodes();
    }

    private void initSubNodes() {

        // Stack
        ctnListNode.setPosition(6, 31);
        ctnListNode.setSize(79, 95);
        addChild(ctnListNode);

        // Scrollbar //TODO
        ScrollBar scrollBar = new ScrollBar();
        scrollBar.setPosition(77, 31);
        scrollBar.setZPosition(0.2);
        scrollBar.setSize(8, 95);
        scrollBar.setSliderSize(8, 16);
        addChild(scrollBar);
    }

    @Override
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {
        graphics.blit(ICWorkbenchCompileTab.TAB_BACKGROUND, getFrame().x(), getFrame().y(), 0, 223, getFrame().width(), getFrame().height(), 512, 512);
    }

    @Override
    public void onAddedToParent() {
        editor.getStateMachine().getCompilerLog().addTreeChangedListener(this::refreshList);
        refreshList();
    }

    private void refreshList() {
        List<ICCompilerLog.CompileTreeNode> execStack = editor.getStateMachine().getCompilerLog().getCurrentStack();
        ctnListNode.setNodeList(execStack);
    }

    private void renderCompileTreeNode(ICCompilerLog.CompileTreeNode node, CCRenderState ccrs, MultiBufferSource getter, PoseStack matrixStack) {
        ccrs.reset();
        ccrs.bind(ICRenderTypes.selectionRenderType, Minecraft.getInstance().renderBuffers().bufferSource(), matrixStack);
        ccrs.baseColour = EnumColour.LIGHT_BLUE.rgba(200);

        for (TileCoord pos : node.tileCoords) {
            Vector3 p = new Vector3(pos.x, pos.y, pos.z);
            ICRenderTypes.renderSelection(ccrs, p, p.copy().add(0.01), 3 / 16D, 2 / 16D);
        }
    }

    //region ICompileTabOverlayRenderer
    @Override
    public void renderOverlay(ICRenderNode renderNode, Vector3 mousePosition, boolean isFirstHit, CCRenderState ccrs, MultiBufferSource getter, PoseStack matrixStack) {
        for (ICCompilerLog.CompileTreeNode node : editor.getStateMachine().getCompilerLog().getCurrentStack()) {
            renderCompileTreeNode(node, ccrs, getter, matrixStack);
        }
    }

    @Override
    public void buildTooltip(ICRenderNode renderNode, Vector3 mousePosition, List<Component> tooltip) {

    }
    //endregion

    private class ScrollBar extends ScrollBarNode {

        public ScrollBar() {
            super(ScrollAxis.VERTICAL);
        }

        @Override
        protected void drawSlider(GuiGraphics graphics, Rect sliderFrame) {
            graphics.blit(ICWorkbenchCompileTab.TAB_BACKGROUND, sliderFrame.x(), sliderFrame.y(), 305, 58, sliderFrame.width(), sliderFrame.height(), 512, 512);
        }

        @Override
        protected void adjustContent(double scrollPercentage) {
            //TODO adjust scroll
        }
    }
}
