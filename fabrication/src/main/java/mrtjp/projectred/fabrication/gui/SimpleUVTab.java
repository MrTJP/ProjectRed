package mrtjp.projectred.fabrication.gui;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchScreen;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.AbstractGuiNode;
import net.minecraft.client.gui.GuiComponent;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.resources.ResourceLocation;

import java.util.List;

public class SimpleUVTab implements TabControllerNode.IToolbarTab {

    private final AbstractGuiNode tabBodyNode;
    private final String tabName;
    private final TabButtonNode.TabSide tabSide;

    private final int u;
    private final int v;
    private final ResourceLocation texture;

    public SimpleUVTab(AbstractGuiNode tabBodyNode, String tabName, TabButtonNode.TabSide side, int u, int v, ResourceLocation texture) { //TODO Icon width/height?
        this.tabBodyNode = tabBodyNode;
        this.tabName = tabName;
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
            }

            @Override
            public void buildTooltip(List<Component> tooltip) {
                tooltip.add(new TextComponent(tabName));
            }
        };
    }
}
