package mrtjp.projectred.fabrication.gui.screen;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.editor.tools.*;
import mrtjp.projectred.fabrication.gui.*;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.AbstractGuiNode;
import net.minecraft.client.gui.GuiComponent;

public class ICWorkbenchEditTab extends AbstractGuiNode {

    private final ICWorkbenchEditor editor;
    private final ICEditorToolManager toolManager;

    public ICWorkbenchEditTab(ICWorkbenchEditor editor) {
        this.editor = editor;
        this.toolManager = new ICEditorToolManager(editor.getToolList());

        setSize(304, 222);
        initSubNodes();
    }

    private void initSubNodes() {

        TabControllerNode toolbarNode = new TabControllerNode();
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

    @Override
    public void drawBack(PoseStack stack, Point mouse, float partialFrame) {
        RenderSystem.setShaderTexture(0, ICWorkbenchScreen.BACKGROUND);
        GuiComponent.blit(stack, getFrame().x(), getFrame().y(), 0, 0, getFrame().width(), getFrame().height(), 512, 512);
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
