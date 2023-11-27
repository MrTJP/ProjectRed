package mrtjp.projectred.fabrication.gui.screen;

import codechicken.lib.colour.EnumColour;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.fabrication.ProjectRedFabrication;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.engine.log.ICCompilerLog;
import mrtjp.projectred.fabrication.gui.SimpleUVTab;
import mrtjp.projectred.fabrication.gui.TabButtonNode;
import mrtjp.projectred.fabrication.gui.TabControllerNode;
import mrtjp.projectred.fabrication.init.FabricationReferences;
import mrtjp.projectred.fabrication.tile.ICWorkbenchTile;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.AbstractGuiNode;
import mrtjp.projectred.redui.ItemStackNode;
import mrtjp.projectred.redui.RedUIScreen;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import org.lwjgl.glfw.GLFW;

import java.util.List;

import static mrtjp.projectred.fabrication.editor.ICWorkbenchEditor.*;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.*;

public class ICWorkbenchScreen extends RedUIScreen {

    public static final ResourceLocation BACKGROUND = new ResourceLocation(ProjectRedFabrication.MOD_ID, "textures/gui/ic_workbench.png");

    private final ICWorkbenchTile tile;
    private final ICWorkbenchEditor editor;

    private AbstractGuiNode contentNode;
    private InactiveOverlayNode overlayNode;

    public ICWorkbenchScreen(ICWorkbenchTile tile) {
        super(304, 222, new TextComponent(tile.getType().getRegistryName().toString()));

        this.tile = tile;
        this.editor = tile.getEditor();

        initSubNodes();
    }

    public static void openGuiOnClient(ICWorkbenchTile tile) {
        Minecraft.getInstance().setScreen(new ICWorkbenchScreen(tile));
    }

    private void initSubNodes() {

        //organizational node to make hiding everything easy
        contentNode = new AbstractGuiNode() { };
        addChild(contentNode);

        ICWorkbenchInfoTab infoTab = new ICWorkbenchInfoTab(editor);
        contentNode.addChild(infoTab);

        ICWorkbenchEditTab editTab = new ICWorkbenchEditTab(editor);
        contentNode.addChild(editTab);

        ICWorkbenchCompileTab compileTab = new ICWorkbenchCompileTab(editor);
        contentNode.addChild(compileTab);

        TabControllerNode tabControllerNode = new TabControllerNode();
        tabControllerNode.setPosition(-19, 4);
        tabControllerNode.setZPosition(0.1);
        contentNode.addChild(tabControllerNode);

        tabControllerNode.addButtonForTab(new SimpleUVTab(infoTab, UL_TAB_INFO, TabButtonNode.TabSide.LEFT, 420, 1));
        tabControllerNode.addButtonForTab(new EditTab(editTab, UL_TAB_EDIT, TabButtonNode.TabSide.LEFT, 420, 16));
        tabControllerNode.addButtonForTab(new CompileTab(compileTab, UL_TAB_COMPILE, TabButtonNode.TabSide.LEFT, 420, 31));

        tabControllerNode.selectInitialTab(1);
        tabControllerNode.spreadButtonsVertically(1);

        // Overlay for no blueprint
        overlayNode = new InactiveOverlayNode();
        overlayNode.setPosition(
                getFrame().midX() - overlayNode.getFrame().width() / 2,
                getFrame().midY() - overlayNode.getFrame().height() / 2);
        addChild(overlayNode);

        refreshOverlay();
    }

    @Override
    public void drawBack(PoseStack stack, Point mouse, float partialFrame) {
        super.drawBack(stack, mouse, partialFrame);
    }

    @Override
    public void removed() {
        super.removed();
        tile.closeGuiFromClient();
        ProjectRedFabrication.LOGGER.info("ICWorkbenchScreen REMOVED");
    }

    @Override
    public void onClose() {
        super.onClose();
        ProjectRedFabrication.LOGGER.info("ICWorkbenchScreen ONCLOSE");
    }

    @Override
    public void update() {
        refreshOverlay();
    }

    @Override
    public boolean shouldCloseOnEsc() {
        // Suppress Vanilla logic to always close screen on ESC press
        return false;
    }

    @Override
    public boolean keyPressed(int glfwKeyCode, int glfwScanCode, int glfwFlags) {
        // This gives our UI elements a chance to consume ESC before we close the screen
        if (super.keyPressed(glfwKeyCode, glfwScanCode, glfwFlags)) return true;

        // Since we didn't consume ESC, we'll close the screen if it was pressed
        if (glfwKeyCode == GLFW.GLFW_KEY_ESCAPE) {
            onClose();
            return true;
        }

        return false;
    }

    private void refreshOverlay() {
        // If active, hide overlay and show every thing else. Otherwise, do opposite
        boolean isActive = editor.isActive();
        overlayNode.setHidden(isActive);
        contentNode.setHidden(!isActive);
    }

    private static class InactiveOverlayNode extends AbstractGuiNode {

        public InactiveOverlayNode() {
            setSize(132, 92);
            initSubNodes();
        }

        private void initSubNodes() {
            ItemStackNode blueprintNode = new ItemStackNode(new ItemStack(FabricationReferences.IC_BLUEPRINT_ITEM));
            blueprintNode.setPosition(58, 24);
            addChild(blueprintNode);

            ItemStackNode workbenchNode = new ItemStackNode(new ItemStack(FabricationReferences.IC_WORKBENCH_BLOCK));
            workbenchNode.setPosition(58, 64);
            addChild(workbenchNode);
        }

        @Override
        public void drawBack(PoseStack stack, Point mouse, float partialFrame) {
            RenderSystem.setShaderTexture(0, BACKGROUND);
            blit(stack, getFrame().x(), getFrame().y(), 0, 222, getFrame().width(), getFrame().height(), 512, 512);

            Font fontRenderer = getRoot().getFontRenderer();
            Component text = new TranslatableComponent(UL_PLACE_BLUEPRINT);

            fontRenderer.draw(stack, text,
                    getFrame().midX() - fontRenderer.width(text) / 2f,
                    getFrame().y() + 6, EnumColour.GRAY.argb());
        }
    }

    private class EditTab extends SimpleUVTab {

        public EditTab(AbstractGuiNode tabBodyNode, String unlocalTabName, TabButtonNode.TabSide side, int u, int v) {
            super(tabBodyNode, unlocalTabName, side, u, v);
        }

        @Override
        protected StatusDot getStatusDot() {
            if (editor.getStateMachine().isSimulating()) {
                return StatusDot.GREEN;
            }
            return StatusDot.NONE;
        }

        @Override
        protected void buildTooltip(List<Component> tooltip) {
            super.buildTooltip(tooltip);
            if (editor.getStateMachine().isSimulating()) {
                tooltip.add(new TranslatableComponent(UL_SIM_RUNNING).withStyle(UNIFORM_GRAY));
            }
        }
    }

    private class CompileTab extends SimpleUVTab {

        public CompileTab(AbstractGuiNode tabBodyNode, String unlocalTabName, TabButtonNode.TabSide side, int u, int v) {
            super(tabBodyNode, unlocalTabName, side, u, v);
        }

        @Override
        protected StatusDot getStatusDot() {
            if (editor.getStateMachine().isCompiling()) {
                return StatusDot.GREEN;
            }
            if (editor.getStateMachine().didLastCompileFailed()) {
                return StatusDot.RED;
            }
            if (editor.getStateMachine().canTriggerCompile()) {
                return StatusDot.YELLOW;
            }
            return StatusDot.NONE;
        }

        @Override
        protected void buildTooltip(List<Component> tooltip) {
            super.buildTooltip(tooltip);

            ICCompilerLog log = editor.getStateMachine().getCompilerLog();
            if (editor.getStateMachine().canTriggerCompile()) {
                tooltip.add(new TranslatableComponent(UL_COMPILE_READY).withStyle(UNIFORM_GRAY));
            }

            if (editor.getStateMachine().isCompiling()) {
                tooltip.add(new TranslatableComponent(UL_COMPILE_PROGRESS, log.getCompletedSteps(), log.getTotalSteps()).withStyle(UNIFORM_GRAY));
            }

            if (editor.getStateMachine().didLastCompileFailed()) {
                tooltip.add(new TranslatableComponent(UL_COMPILE_FAILED).withStyle(UNIFORM_RED));
            }

            ICWorkbenchCompileTab.appendProblemsInfo(log, tooltip);
        }
    }
}
