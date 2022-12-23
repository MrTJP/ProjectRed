package mrtjp.projectred.fabrication.gui;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.fabrication.engine.log.CompileProblem;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchCompileTab;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.lib.Rect;
import mrtjp.projectred.redui.AbstractGuiNode;
import net.minecraft.client.gui.GuiComponent;
import net.minecraft.network.chat.Component;

import java.util.LinkedList;
import java.util.List;

public class ProblemListNode extends AbstractGuiNode {

    private final List<CompileProblem> problemList = new LinkedList<>();

    private final AbstractGuiNode listParent = new AbstractGuiNode() {
    };

    private double scroll = 0;

    public ProblemListNode() {
        initSubNodes();
    }

    private void initSubNodes() {
        addChild(listParent);
    }

    public void setProblemList(List<CompileProblem> list) {
        this.problemList.clear();
        this.problemList.addAll(list);
        refreshListItems();
    }

    private void refreshListItems() {

        listParent.removeAllChildren();
        listParent.setPosition(0, 0);

        int y = 0;
        for (CompileProblem issue : problemList) {
            IssueListItemNode item = new IssueListItemNode(issue);
            item.setPosition(0, y);
            listParent.addChild(item);
            y += item.calculateAccumulatedFrame().height();
        }

        moveListToScroll();
    }

    public void setScrollPercentage(double scrollPercentage) {
        this.scroll = scrollPercentage;
        moveListToScroll();
    }

    private void moveListToScroll() {
        Rect subFrame = calculateChildrenFrame();
        if (subFrame.height() <= getFrame().height()) return;

        int totalScroll = subFrame.height() - getFrame().height(); // How much scroll is possible
        int dist = (int) (totalScroll * scroll); // How much we want to scroll

        listParent.setPosition(0, -dist);
    }

    @Override
    public void onSubTreePreDrawBack() {
        // This node's frame converted to GL11 window coordinates
        Rect gl11Rect = calculateGL11Frame();

        // Enable scissor using the calculated rect
        RenderSystem.enableScissor(gl11Rect.x(), gl11Rect.y(), gl11Rect.width(), gl11Rect.height());
    }

    @Override
    public void onSubTreePostDrawBack() {
        // Disable scissor
        RenderSystem.disableScissor();
    }

    private class IssueListItemNode extends AbstractGuiNode {

        private final CompileProblem issue;

        public IssueListItemNode(CompileProblem issue) {
            this.issue = issue;
            setSize(67, 12);
        }

        @Override
        public void drawBack(PoseStack stack, Point mouse, float partialFrame) {
            RenderSystem.setShaderTexture(0, ICWorkbenchCompileTab.TAB_BACKGROUND);

            GuiComponent.blit(stack, getFrame().x(), getFrame().y(), 1, 375, getFrame().width(), getFrame().height(), 512, 512);

            Component s = issue.getName();
            getRoot().getFontRenderer().draw(stack, s, getFrame().x() + 2, getFrame().y() + 2, 0xFFFFFF);
        }

        @Override
        public void drawFront(PoseStack stack, Point mouse, float partialFrame) {
            if (!isFirstHit(mouse)) return;

            List<Component> toolTip = new LinkedList<>();
            issue.buildToolTip(toolTip);
            renderTooltip(stack, mouse, toolTip);
        }

        @Override
        public boolean checkHit(Point absPoint) {
            return super.checkHit(absPoint) && ProblemListNode.this.convertParentRectToScreen(ProblemListNode.this.getFrame()).contains(absPoint);
        }
    }
}
