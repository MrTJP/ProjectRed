package mrtjp.projectred.fabrication.gui.screen;

import codechicken.lib.colour.EnumColour;
import mrtjp.projectred.fabrication.ProjectRedFabrication;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.engine.log.ICCompilerLog;
import mrtjp.projectred.fabrication.gui.SimpleUVTab;
import mrtjp.projectred.fabrication.gui.TabButtonNode;
import mrtjp.projectred.fabrication.gui.TabControllerNode;
import mrtjp.projectred.fabrication.tile.ICWorkbenchBlockEntity;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.AbstractGuiNode;
import mrtjp.projectred.redui.ItemStackNode;
import mrtjp.projectred.redui.RedUIScreen;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;
import net.minecraftforge.registries.ForgeRegistries;
import org.lwjgl.glfw.GLFW;

import javax.annotation.Nullable;
import java.util.List;
import java.util.Objects;

import static mrtjp.projectred.fabrication.editor.ICWorkbenchEditor.UNIFORM_GRAY;
import static mrtjp.projectred.fabrication.editor.ICWorkbenchEditor.UNIFORM_RED;
import static mrtjp.projectred.fabrication.init.FabricationBlocks.IC_WORKBENCH_BLOCK;
import static mrtjp.projectred.fabrication.init.FabricationItems.IC_BLUEPRINT_ITEM;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.*;

public class ICWorkbenchScreen extends RedUIScreen {

    public static final ResourceLocation BACKGROUND = new ResourceLocation(ProjectRedFabrication.MOD_ID, "textures/gui/ic_workbench.png");

    private final ICWorkbenchBlockEntity tile;
    private final ICWorkbenchEditor editor;

    private @Nullable AbstractGuiNode contentNode;
    private @Nullable InactiveOverlayNode overlayNode;

    public ICWorkbenchScreen(ICWorkbenchBlockEntity tile) {
        super(304, 222, Component.literal(Objects.requireNonNull(ForgeRegistries.BLOCK_ENTITY_TYPES.getKey(tile.getType())).toString()));

        this.tile = tile;
        this.editor = tile.getEditor();

        initSubNodes();
    }

    public static void openGuiOnClient(ICWorkbenchBlockEntity tile) {
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
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {
        super.drawBack(graphics, mouse, partialFrame);
    }

    @Override
    public void removed() {
        super.removed();
        tile.closeGuiFromClient();
    }

    @Override
    public void onClose() {
        super.onClose();
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
        Objects.requireNonNull(overlayNode).setHidden(isActive);
        Objects.requireNonNull(contentNode).setHidden(!isActive);
    }

    private static class InactiveOverlayNode extends AbstractGuiNode {

        public InactiveOverlayNode() {
            setSize(132, 92);
            initSubNodes();
        }

        private void initSubNodes() {
            ItemStackNode blueprintNode = new ItemStackNode(new ItemStack(IC_BLUEPRINT_ITEM.get()));
            blueprintNode.setPosition(58, 24);
            addChild(blueprintNode);

            ItemStackNode workbenchNode = new ItemStackNode(new ItemStack(IC_WORKBENCH_BLOCK.get()));
            workbenchNode.setPosition(58, 64);
            addChild(workbenchNode);
        }

        @Override
        public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {
            graphics.blit(BACKGROUND, getFrame().x(), getFrame().y(), 0, 222, getFrame().width(), getFrame().height(), 512, 512);

            Font fontRenderer = getRoot().getFontRenderer();
            Component text = Component.translatable(UL_PLACE_BLUEPRINT);

            graphics.drawString(fontRenderer, text,
                    getFrame().midX() - fontRenderer.width(text) / 2,
                    getFrame().y() + 6, EnumColour.GRAY.argb(), false);
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
                tooltip.add(Component.translatable(UL_SIM_RUNNING).withStyle(UNIFORM_GRAY));
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
                tooltip.add(Component.translatable(UL_COMPILE_READY).withStyle(UNIFORM_GRAY));
            }

            if (editor.getStateMachine().isCompiling()) {
                tooltip.add(Component.translatable(UL_COMPILE_PROGRESS, log.getCompletedSteps(), log.getTotalSteps()).withStyle(UNIFORM_GRAY));
            }

            if (editor.getStateMachine().didLastCompileFailed()) {
                tooltip.add(Component.translatable(UL_COMPILE_FAILED).withStyle(UNIFORM_RED));
            }

            ICWorkbenchCompileTab.appendProblemsInfo(log, tooltip);
        }
    }
}
