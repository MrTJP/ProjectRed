package mrtjp.projectred.fabrication.gui;

import codechicken.lib.texture.TextureUtils;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.core.vec.Point;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchScreen;
import mrtjp.projectred.redui.AbstractGuiNode;
import net.minecraft.client.audio.SimpleSound;
import net.minecraft.util.SoundEvents;
import net.minecraft.util.text.ITextProperties;

import java.util.LinkedList;
import java.util.List;

import static net.minecraft.client.gui.AbstractGui.blit;

public abstract class TabButtonNode extends AbstractGuiNode {

    private TabState renderState;
    private final TabSide side;

    protected final TabControllerNode.IToolbarTab tab;

    public TabButtonNode(TabControllerNode.IToolbarTab tab, TabSide side) {
        this(tab, side, TabState.ALL_CLOSED);
    }

    public TabButtonNode(TabControllerNode.IToolbarTab tab, TabSide side, TabState renderState) {
        this.tab = tab;
        this.side = side;
        this.renderState = renderState;

        setSize(side.w, side.h);
    }


    public TabState getRenderState() {
        return renderState;
    }

    public void setTabState(TabState renderState) {

        if (renderState != this.renderState) {
            TabState prevState = this.renderState;
            this.renderState = renderState;

            onTabStateChanged(prevState, renderState);
        }
    }

    @Override
    public void drawBack(MatrixStack stack, Point mouse, float partialFrame) {
        TextureUtils.changeTexture(ICWorkbenchScreen.BACKGROUND);

        int x = getPosition().x();
        int y = getPosition().y();
        int u = side.u;
        int v = side.v;
        int w = side.w;
        int h = side.h;

        int ex = x + side.extOffsetX;
        int ey = y + side.extOffsetY;
        int eu = side.extU;
        int ev = side.extV;
        int ew = side.extW;
        int eh = side.extH;

        // If inactive, render greyed out version of the sprite
        if (renderState == TabState.ALL_CLOSED || renderState == TabState.CLOSED) {
            u += side.inactiveOffsetU;
            v += side.inactiveOffsetV;
            eu += side.inactiveOffsetU;
            ev += side.inactiveOffsetV;
        }

        blit(stack, x, y, u, v, w, h, 512, 512);

        // Render extension if needed
        if (renderState == TabState.CLOSED || renderState == TabState.OPEN) {
            blit(stack, ex, ey, eu, ev, ew, eh, 512, 512);
        }

        renderIcon(stack, mouse, partialFrame);
    }

    @Override
    public void drawFront(MatrixStack stack, Point mouse, float partialFrame) {

        if (!isFirstHit(mouse))
            return;

        List<ITextProperties> tooltip = new LinkedList<>();
        buildTooltip(tooltip);

        renderTooltip(stack, mouse, tooltip);
    }

    @Override
    public boolean mouseClicked(Point p, int glfwMouseButton, boolean consumed) {
        if (!consumed && isFirstHit(p)) {
            getRoot().getMinecraft().getSoundManager().play(SimpleSound.forUI(SoundEvents.UI_BUTTON_CLICK, 1));
            onClicked();
            return true;
        }
        return false;
    }

    public void onClicked() {
        TabControllerNode tabControllerNode = (TabControllerNode) getParent();

        tabControllerNode.openTab(tab);
        if (!hasBody()) tabControllerNode.minimizeTab(tab); // If no body, minimize tab
    }

    public void onTabStateChanged(TabState prevState, TabState newState) {
        switch (newState) {
            case OPEN:
                tab.onTabOpened();
                break;
            case CLOSED:
            case ALL_CLOSED:
                tab.onTabClosed();
                break;
            case MINIMIZED:
                tab.onTabMinimized();
        }
    }

    public boolean hasBody() {
        return true;
    }

    public abstract void renderIcon(MatrixStack stack, Point mouse, float partialFrame);

    public abstract void buildTooltip(List<ITextProperties> tooltip);

    enum TabState {
        ALL_CLOSED, // Unselected with no extension
        CLOSED, // Unselected with extension
        MINIMIZED, // Selected with no extension
        OPEN // Selected with extension
    }

    public enum TabSide {
        LEFT(482, 179, 20, 20, 503, 179, 6, 20, 16, 0, 0, 21);

        public final int u;
        public final int v;
        public final int w;
        public final int h;
        public final int extU;
        public final int extV;
        public final int extW;
        public final int extH;
        public final int extOffsetX;
        public final int extOffsetY;
        public final int inactiveOffsetU;
        public final int inactiveOffsetV;

        TabSide(int u, int v, int w, int h, int extU, int extV, int extW, int extH, int extOffsetX, int extOffsetY, int inactiveOffsetU, int inactiveOffsetV) {
            this.u = u;
            this.v = v;
            this.w = w;
            this.h = h;
            this.extU = extU;
            this.extV = extV;
            this.extW = extW;
            this.extH = extH;
            this.extOffsetX = extOffsetX;
            this.extOffsetY = extOffsetY;
            this.inactiveOffsetU = inactiveOffsetU;
            this.inactiveOffsetV = inactiveOffsetV;
        }
    }
}
