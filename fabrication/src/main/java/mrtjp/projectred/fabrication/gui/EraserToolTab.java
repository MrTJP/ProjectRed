package mrtjp.projectred.fabrication.gui;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.fabrication.editor.tools.EraseTool;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchScreen;
import mrtjp.projectred.lib.Point;
import net.minecraft.client.gui.GuiComponent;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;

import java.util.List;

public class EraserToolTab extends ICEditorToolTab {

    private final EraseTool tool;

    public EraserToolTab(ICEditorToolManager manager, EraseTool tool) {
        super(manager, tool);
        this.tool = tool;
        construct();
    }

    private void construct() {

    }

    @Override
    public TabButtonNode createButtonNode() {
        return new TabButtonNode(this, TabButtonNode.TabSide.LEFT) {
            @Override
            public void renderIcon(PoseStack stack, Point mouse, float partialFrame) {
                RenderSystem.setShaderTexture(0, ICWorkbenchScreen.BACKGROUND);
                GuiComponent.blit(stack, getFrame().x() + 3, getFrame().y() + 3, 390, 16, 14, 14, 512, 512);
            }

            @Override
            public void buildTooltip(List<Component> tooltip) {
                tooltip.add(new TextComponent("Eraser")); //TODO Localize
            }

            @Override
            public boolean hasBody() {
                return false;
            }
        };
    }
}
