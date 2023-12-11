package mrtjp.projectred.fabrication.gui;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.fabrication.editor.tools.InteractTool;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchScreen;
import mrtjp.projectred.lib.Point;
import net.minecraft.client.gui.GuiComponent;
import net.minecraft.network.chat.Component;

import java.util.List;

import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_INTERACT_TOOL;

public class InteractToolTab extends ICEditorToolTab {

    private final InteractTool tool;

    public InteractToolTab(ICEditorToolManager manager, InteractTool tool) {
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
                GuiComponent.blit(stack, getFrame().x() + 3, getFrame().y() + 3, 390, 1, 14, 14, 512, 512);
            }

            @Override
            public void buildTooltip(List<Component> tooltip) {
                tooltip.add(Component.translatable(UL_INTERACT_TOOL));
            }
        };
    }

    @Override
    public boolean hasBody() {
        return false;
    }
}
