package mrtjp.projectred.fabrication.gui;

import com.mojang.blaze3d.systems.RenderSystem;
import mrtjp.projectred.fabrication.engine.log.ICCompilerLog.CompileTreeNode;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchCompileTab;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.lib.Rect;
import mrtjp.projectred.redui.AbstractGuiNode;
import mrtjp.projectred.redui.RedUISprite;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;

import javax.annotation.Nullable;
import java.util.LinkedList;
import java.util.List;

import static mrtjp.projectred.fabrication.editor.ICWorkbenchEditor.UNIFORM;

public class CTNListNode extends AbstractGuiNode {

    private final List<CompileTreeNode> nodeList = new LinkedList<>();
    private final AbstractGuiNode listParent = new AbstractGuiNode() {
    };

    private double scroll = 0;

    public CTNListNode() {
        initSubNodes();
    }

    private void initSubNodes() {
        addChild(listParent);
    }

    public void setNodeList(List<CompileTreeNode> nodeList) {
        if (!this.nodeList.equals(nodeList)) {
            this.nodeList.clear();
            this.nodeList.addAll(nodeList);
            refreshListItems();
        }
    }

    private void refreshListItems() {
        listParent.removeAllChildren();
        listParent.setPosition(0, 0);

        int y = 0;
        for (CompileTreeNode node : nodeList) {
            CompileTreeNodeListItem item = new CompileTreeNodeListItem(node);
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

    public @Nullable CompileTreeNode getSelectedNode() {
        for (var child : listParent.getOurChildren()) {
            if (child instanceof CompileTreeNodeListItem item && item.selected) {
                return item.node;
            }
        }
        return null;
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

    private void selectNodeInList(CompileTreeNodeListItem node) {
        for (var child : listParent.getOurChildren()) {
            if (child instanceof CompileTreeNodeListItem item) {
                item.selected = item == node;
            }
        }
    }

    private class CompileTreeNodeListItem extends AbstractGuiNode {

        private static final RedUISprite BACKGROUND_UNSELECTED = new RedUISprite(ICWorkbenchCompileTab.TAB_BACKGROUND, 1, 375, 79, 12, 512, 512);
        private static final RedUISprite BACKGROUND_SELECTED = new RedUISprite(ICWorkbenchCompileTab.TAB_BACKGROUND, 81, 375, 79, 12, 512, 512);

        private final CompileTreeNode node;
        private boolean selected = false;

        public CompileTreeNodeListItem(CompileTreeNode node) {
            this.node = node;
            setSize(68, 12);
        }

        @Override
        public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {
            blitSprite(graphics, selected ? BACKGROUND_SELECTED : BACKGROUND_UNSELECTED);

            var fr = getRoot().getFontRenderer();
            Component c = CompileTreeTab.getTitleForCTNNode(node).copy().withStyle(UNIFORM);
            graphics.drawString(fr, c, getFrame().x() + 2, getFrame().y() + getFrame().height() / 2 - fr.lineHeight / 2, 0xFFFFFF, false);
        }

        @Override
        public void drawFront(GuiGraphics graphics, Point mouse, float partialFrame) {

            if (!isFirstHit(mouse)) return;

            List<Component> toolTip = new LinkedList<>();
            CompileTreeTab.buildTooltipForCTNNode(node, toolTip);
            renderTooltip(graphics, mouse, toolTip);
        }

        @Override
        public boolean checkHit(Point absPoint) {
            return super.checkHit(absPoint) && CTNListNode.this.convertParentRectToScreen(CTNListNode.this.getFrame()).contains(absPoint);
        }

        @Override
        public boolean mouseClicked(Point p, int glfwMouseButton, boolean consumed) {
            if (!consumed && isFirstHit(p)) {
                selectNodeInList(this);
                return true;
            }
            return false;
        }
    }
}
