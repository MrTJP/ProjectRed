package mrtjp.projectred.fabrication.gui;

import mrtjp.projectred.fabrication.editor.tools.EraseTool;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchScreen;
import mrtjp.projectred.lib.Point;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;

import java.util.List;

import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_ERASER_TOOL;

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
            public void renderIcon(GuiGraphics graphics, Point mouse, float partialFrame) {
                graphics.blit(ICWorkbenchScreen.BACKGROUND, getFrame().x() + 3, getFrame().y() + 3, 390, 16, 14, 14, 512, 512);
            }

            @Override
            public void buildTooltip(List<Component> tooltip) {
                tooltip.add(Component.translatable(UL_ERASER_TOOL));
            }
        };
    }

    @Override
    public boolean hasBody() {
        return false;
    }
}
