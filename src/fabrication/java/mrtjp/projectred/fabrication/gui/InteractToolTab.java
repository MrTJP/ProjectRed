package mrtjp.projectred.fabrication.gui;

import codechicken.lib.texture.TextureUtils;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.core.vec.Point;
import mrtjp.projectred.fabrication.editor.tools.InteractTool;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchScreen;
import net.minecraft.client.gui.AbstractGui;
import net.minecraft.util.text.ITextProperties;
import net.minecraft.util.text.StringTextComponent;

import java.util.List;

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
            public void renderIcon(MatrixStack stack, Point mouse, float partialFrame) {
                TextureUtils.changeTexture(ICWorkbenchScreen.BACKGROUND);
                AbstractGui.blit(stack, getFrame().x() + 3, getFrame().y() + 3, 390, 1, 14, 14, 512, 512);
            }

            @Override
            public void buildTooltip(List<ITextProperties> tooltip) {
                tooltip.add(new StringTextComponent("Interact")); //TODO Localize
            }

            @Override
            public boolean hasBody() {
                return false;
            }
        };
    }
}
