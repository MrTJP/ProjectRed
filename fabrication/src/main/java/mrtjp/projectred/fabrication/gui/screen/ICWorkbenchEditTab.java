package mrtjp.projectred.fabrication.gui.screen;

import codechicken.lib.colour.EnumColour;
import mrtjp.projectred.fabrication.editor.ICEditorToolType;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.editor.tools.*;
import mrtjp.projectred.fabrication.gui.*;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.lib.Rect;
import mrtjp.projectred.redui.AbstractGuiNode;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;

public class ICWorkbenchEditTab extends AbstractGuiNode {

    private final ICWorkbenchEditor editor;
    private final ICEditorToolManager toolManager;

    private final TabControllerNode toolbarNode = new TabControllerNode();

    public ICWorkbenchEditTab(ICWorkbenchEditor editor) {
        this.editor = editor;
        this.toolManager = new ICEditorToolManager(editor.getToolList());

        toolManager.addToolSwappedListener(this::onToolSwapped);

        setSize(304, 222);
        initSubNodes();
    }

    private void initSubNodes() {

        toolbarNode.setPosition(286, 24);
        toolbarNode.setZPosition(0.1);
        addChild(toolbarNode);

        for (IICEditorTool tool : editor.getToolList()) {

            ICEditorToolTab tab = getTabForTool(tool);
            tab.setPosition(305, 0);
            tab.setHidden(true);

            toolbarNode.addButtonForTab(tab);
            addChild(tab);
        }
        toolbarNode.selectInitialTab(0);
        toolbarNode.spreadButtonsVertically(1);

        ICRenderNode icRenderNode = new ICRenderNode(editor, toolManager);
        icRenderNode.setPosition(7, 18);
        icRenderNode.setSize(290, 197);
        addChild(icRenderNode);
    }

    private ICEditorToolTab getTabForTool(IICEditorTool tool) {

        if (tool instanceof InteractTool)
            return new InteractToolTab(toolManager, (InteractTool) tool);

        if (tool instanceof EraseTool)
            return new EraserToolTab(toolManager, (EraseTool) tool);

        if (tool instanceof GatePlacerTool)
            return new GatePlacerToolTab(toolManager, (GatePlacerTool) tool);

        if (tool instanceof WirePlacerTool)
            return new WirePlacerToolTab(toolManager, (WirePlacerTool) tool);

        return new ICEditorToolTab(toolManager, tool);
    }

    private void onToolSwapped(ICEditorToolType newToolType) {
        toolbarNode.openTab(tab -> ((ICEditorToolTab) tab).getTool().getToolType() == newToolType);
    }

    @Override
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {
        Rect frame = getFrame();
        Font fontRenderer = getRoot().getFontRenderer();

        graphics.blit(ICWorkbenchScreen.BACKGROUND, frame.x(), frame.y(), 0, 0, frame.width(), frame.height(), 512, 512);

        // Blueprint name in top left corner
        graphics.drawString(fontRenderer, editor.getIcName(), frame.x() + 8, frame.y() + 6, EnumColour.GRAY.argb(), false);
    }

    @Override
    public boolean onKeyPressed(int glfwKeyCode, int glfwScanCode, int glfwFlags, boolean consumed) {
        if (!consumed && !isHidden()) {
            return toolManager.keyPressed(glfwKeyCode, glfwFlags);
        }
        return false;
    }

    @Override
    public boolean onKeyReleased(int glfwKeyCode, int glfwScanCode, int glfwFlags, boolean consumed) {
        return toolManager.keyReleased(glfwKeyCode, glfwFlags);
    }
}
