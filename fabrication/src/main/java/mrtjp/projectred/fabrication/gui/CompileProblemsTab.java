package mrtjp.projectred.fabrication.gui;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.editor.tools.IICEditorTool;
import mrtjp.projectred.fabrication.engine.log.CompileProblem;
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

public class CompileProblemsTab extends AbstractGuiNode implements ICompileOverlayRenderer {

    private final ICWorkbenchEditor editor;
    private final ProblemListNode issueListNode = new ProblemListNode();

    public CompileProblemsTab(ICWorkbenchEditor editor) {
        this.editor = editor;
        setSize(91, 134);
        initSubNodes();
    }

    private void initSubNodes() {

        // Stack
        issueListNode.setPosition(6, 31);
        issueListNode.setSize(79, 95);
        addChild(issueListNode);

        // Scrollbar
        ScrollBar scrollBar = new ScrollBar();
        scrollBar.setPosition(77, 31);
        scrollBar.setZPosition(0.2);
        scrollBar.setSize(8, 95);
        scrollBar.setSliderSize(8, 16);
        addChild(scrollBar);
    }

    @Override
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {
        graphics.blit(ICWorkbenchCompileTab.TAB_BACKGROUND, getFrame().x(), getFrame().y(), 184, 223, getFrame().width(), getFrame().height(), 512, 512);
    }

    @Override
    public void onAddedToParent() {
        editor.getStateMachine().getCompilerLog().addTreeChangedListener(this::refreshList);
        refreshList();
    }

    private void refreshList() {
        issueListNode.setProblemList(editor.getStateMachine().getCompilerLog().getProblems());
    }

    //region ICompileTabOverlayRenderer
    public void renderOverlay(ICRenderNode renderNode, Vector3 mousePosition, boolean isFirstHit, CCRenderState ccrs, MultiBufferSource getter, PoseStack matrixStack) {
        ccrs.reset();
        ccrs.bind(ICRenderTypes.selectionRenderType, Minecraft.getInstance().renderBuffers().bufferSource(), matrixStack);

        for (CompileProblem problem : editor.getStateMachine().getCompilerLog().getProblems()) {
            problem.renderOverlay(mousePosition, ccrs, getter, matrixStack);
        }
    }

    @Override
    public void buildTooltip(ICRenderNode renderNode, Vector3 mousePosition, List<Component> tooltip) {

        TileCoord pos = IICEditorTool.toNearestPosition(mousePosition);
        for (CompileProblem issue : editor.getStateMachine().getCompilerLog().getProblems()) {
            issue.buildToolTip(tooltip, pos);
        }
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
            issueListNode.setScrollPercentage(scrollPercentage);
        }
    }
}
