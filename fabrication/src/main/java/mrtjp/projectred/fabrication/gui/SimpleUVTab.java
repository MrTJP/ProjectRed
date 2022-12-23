package mrtjp.projectred.fabrication.gui;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchScreen;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.AbstractGuiNode;
import net.minecraft.client.gui.GuiComponent;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceLocation;

import java.util.List;

public class SimpleUVTab implements TabControllerNode.IToolbarTab {

    private final AbstractGuiNode tabBodyNode;
    private final Component tabName;
    private final TabButtonNode.TabSide tabSide;

    private final int u;
    private final int v;
    private final ResourceLocation texture;

    public SimpleUVTab(AbstractGuiNode tabBodyNode, String unlocalTabName, TabButtonNode.TabSide side, int u, int v, ResourceLocation texture) { //TODO Icon width/height?
        this.tabBodyNode = tabBodyNode;
        this.tabName = new TranslatableComponent(unlocalTabName);
        this.tabSide = side;
        this.u = u;
        this.v = v;
        this.texture = texture;
    }

    public SimpleUVTab(AbstractGuiNode tabBodyNode, String tabName, TabButtonNode.TabSide side, int u, int v) {
        this(tabBodyNode, tabName, side, u, v, ICWorkbenchScreen.BACKGROUND);
    }

    public AbstractGuiNode getTabBodyNode() {
        return tabBodyNode;
    }

    //region Customization
    protected StatusDot getStatusDot() {
        return StatusDot.NONE;
    }

    protected void buildTooltip(List<Component> tooltip) {
        tooltip.add(tabName);
    }
    //endregion

    //region TabControllerNode.IToolbarTab
    @Override
    public void onTabClosed() {
        tabBodyNode.setHidden(true);
    }

    @Override
    public void onTabOpened() {
        tabBodyNode.setHidden(false);
    }

    @Override
    public void onTabMinimized() {
        tabBodyNode.setHidden(true);
    }

    @Override
    public TabButtonNode createButtonNode() {
        return new TabButtonNode(this, tabSide) {
            @Override
            public void renderIcon(PoseStack stack, Point mouse, float partialFrame) {
                RenderSystem.setShaderTexture(0, texture);
                GuiComponent.blit(stack, getFrame().x() + 3, getFrame().y() + 3, u, v, 14, 14, 512, 512);

                StatusDot statusDot = SimpleUVTab.this.getStatusDot();
                if (statusDot != StatusDot.NONE) {
                    RenderSystem.setShaderTexture(0, ICWorkbenchScreen.BACKGROUND);
                    GuiComponent.blit(stack, getFrame().x() + 3 + 7, getFrame().y() + 3, 7, 7, statusDot.u, statusDot.v, 14, 14, 512, 512);
                }
            }

            @Override
            public void buildTooltip(List<Component> tooltip) {
                SimpleUVTab.this.buildTooltip(tooltip);
            }
        };
    }
    //endregion

    public enum StatusDot {
        NONE(0, 0),
        GREEN(435, 1),
        YELLOW(450, 1),
        RED(465, 1)
        ;

        public final int u;
        public final int v;

        StatusDot(int u, int v) {
            this.u = u;
            this.v = v;
        }
    }
}
