package mrtjp.projectred.fabrication.gui.screen;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.ProjectRedFabrication;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.editor.tools.IICEditorTool;
import mrtjp.projectred.fabrication.engine.log.ICCompilerLog;
import mrtjp.projectred.fabrication.gui.*;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.AbstractButtonNode;
import mrtjp.projectred.redui.AbstractGuiNode;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiComponent;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceLocation;
import org.lwjgl.glfw.GLFW;

import java.util.LinkedList;
import java.util.List;

import static mrtjp.projectred.fabrication.editor.ICWorkbenchEditor.*;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.*;

public class ICWorkbenchCompileTab extends AbstractGuiNode implements ICRenderNode.IICRenderNodeEventReceiver {

    public static final ResourceLocation TAB_BACKGROUND = new ResourceLocation(ProjectRedFabrication.MOD_ID, "textures/gui/compile_tab.png");

    private final ICWorkbenchEditor editor;

    private List<ICompileOverlayRenderer> overlays = new LinkedList<>();

    private boolean upPressed = false;
    private boolean rightPressed = false;
    private boolean downPressed = false;
    private boolean leftPressed = false;
    private boolean layerUpPressed = false;
    private boolean layerDownPressed = false;

    public ICWorkbenchCompileTab(ICWorkbenchEditor editor) {
        this.editor = editor;

        setSize(304, 222);
        initSubNodes();
    }

    private void initSubNodes() {

        CompileButton compileButton = new CompileButton();
        compileButton.setPosition(208, 16);
        compileButton.setSize(18, 18);
        addChild(compileButton);

        ICRenderNode icRenderNode = new ICRenderNode(editor, this);
        icRenderNode.setPosition(7, 18);
        icRenderNode.setSize(197, 197);
        addChild(icRenderNode);

        // Tab nodes
        CompileStackTab compileStackTab = new CompileStackTab(editor);
        compileStackTab.setPosition(208, 77);
        addChild(compileStackTab);

        CompileTreeTab compileTreeTab = new CompileTreeTab(editor);
        compileTreeTab.setPosition(208, 77);
        addChild(compileTreeTab);

        CompileProblemsTab compileProblemsTab = new CompileProblemsTab(editor);
        compileProblemsTab.setPosition(208, 77);
        addChild(compileProblemsTab);

        // Keep track of overlays
        overlays.add(compileStackTab);
        overlays.add(compileTreeTab);
        overlays.add(compileProblemsTab);

        // Bottom tabs
        TabControllerNode tabControllerNode = new TabControllerNode();
        tabControllerNode.setPosition(212, 210);
        tabControllerNode.setZPosition(0.1);
        addChild(tabControllerNode);

        tabControllerNode.addButtonForTab(new SimpleUVTab(compileStackTab, UL_TAB_STACK, TabButtonNode.TabSide.BOTTOM, 350, 11, TAB_BACKGROUND));
        tabControllerNode.addButtonForTab(new SimpleUVTab(compileTreeTab, UL_TAB_TREE, TabButtonNode.TabSide.BOTTOM, 365, 11, TAB_BACKGROUND));
        tabControllerNode.addButtonForTab(new ProblemsTab(compileProblemsTab, UL_TAB_PROBLEMS, TabButtonNode.TabSide.BOTTOM, 380, 11, TAB_BACKGROUND));

        tabControllerNode.selectInitialTab(0);
        tabControllerNode.spreadButtonsHorizontally(1);
    }

    @Override
    public void drawBack(PoseStack stack, Point mouse, float partialFrame) {
        RenderSystem.setShaderTexture(0, TAB_BACKGROUND);
        GuiComponent.blit(stack, getFrame().x(), getFrame().y(), 0, 0, getFrame().width(), getFrame().height(), 512, 512);

        // Blueprint name in top left corner
        getRoot().getFontRenderer().draw(stack, editor.getIcName(), getFrame().x() + 8, getFrame().y() + 6, EnumColour.GRAY.argb());

        // Progress bar
        RenderSystem.setShaderTexture(0, TAB_BACKGROUND);
        int barWidth = 91;
        int progress = editor.getStateMachine().getCompilerLog().getProgressScaled(barWidth);
        GuiComponent.blit(stack, getFrame().x() + 208, getFrame().y() + 36, 304, 0, barWidth, 5, 512, 512);
        GuiComponent.blit(stack, getFrame().x() + 208, getFrame().y() + 36, 304, 5, progress, 5, 512, 512);

        // Progress text
        ICCompilerLog log = editor.getStateMachine().getCompilerLog();
        getRoot().getFontRenderer().draw(stack,
                new TranslatableComponent(editor.getStateMachine().isCompiling() ? UL_COMPILE_PROGRESS : UL_COMPILE_DONE, log.getCompletedSteps(), log.getTotalSteps())
                        .withStyle(UNIFORM_DARK_GRAY),
                getFrame().x() + 208, getFrame().y() + 42, EnumColour.GRAY.argb());

        // Error Count
        Component errorText = log.getErrorCount() == 0 ?
                new TextComponent("<!> ")
                        .append(new TranslatableComponent(UL_NO_ERRORS))
                        .withStyle(UNIFORM_DARK_GRAY) :
                new TextComponent("<!> ")
                        .withStyle(ICWorkbenchEditor.UNIFORM_RED)
                        .append(new TranslatableComponent(UL_UNIT_ERRORS, log.getErrorCount())
                                .withStyle(UNIFORM_DARK_GRAY));

        Component warningText = log.getWarningCount() == 0 ?
                new TextComponent("<!> ")
                        .append(new TranslatableComponent(UL_NO_WARNINGS))
                        .withStyle(UNIFORM_DARK_GRAY) :
                new TextComponent("<!> ")
                        .withStyle(ICWorkbenchEditor.UNIFORM_YELLOW)
                        .append(new TranslatableComponent(UL_UNIT_WARNINGS, log.getWarningCount())
                                .withStyle(UNIFORM_DARK_GRAY));

        getRoot().getFontRenderer().draw(stack, errorText, getFrame().x() + 208, getFrame().y() + 52, EnumColour.GRAY.argb());
        getRoot().getFontRenderer().draw(stack, warningText, getFrame().x() + 208, getFrame().y() + 60, EnumColour.GRAY.argb());
    }

    //TODO Reduce this reused code (ICEditorToolManager)
    @Override
    public boolean onKeyPressed(int glfwKeyCode, int glfwScanCode, int glfwFlags, boolean consumed) {
        if (consumed || isHidden())
            return false;

        switch (glfwKeyCode) {
            case GLFW.GLFW_KEY_W:
                upPressed = true;
                break;
            case GLFW.GLFW_KEY_A:
                leftPressed = true;
                break;
            case GLFW.GLFW_KEY_S:
                downPressed = true;
                break;
            case GLFW.GLFW_KEY_D:
                rightPressed = true;
                break;
            case GLFW.GLFW_KEY_UP:
                layerUpPressed = true;
                break;
            case GLFW.GLFW_KEY_DOWN:
                layerDownPressed = true;
                break;
            default:
                return false;
        }
        return true;
    }

    @Override
    public boolean onKeyReleased(int glfwKeyCode, int glfwScanCode, int glfwFlags, boolean consumed) {
        switch (glfwKeyCode) {
            case GLFW.GLFW_KEY_W:
                upPressed = false;
                break;
            case GLFW.GLFW_KEY_A:
                leftPressed = false;
                break;
            case GLFW.GLFW_KEY_S:
                downPressed = false;
                break;
            case GLFW.GLFW_KEY_D:
                rightPressed = false;
                break;
            case GLFW.GLFW_KEY_UP:
                layerUpPressed = false;
                break;
            case GLFW.GLFW_KEY_DOWN:
                layerDownPressed = false;
                break;
            default:
                return false;
        }
        return true;
    }

    //region ICRenderNode.IICRenderNodeEventReceiver

    @Override
    public void update(ICRenderNode renderNode) {
        // Pan camera
        if (upPressed || downPressed || leftPressed || rightPressed) {
            Vector3 panDelta = new Vector3();
            double deltaPerTick = 0.05D;
            panDelta.z = (upPressed ? -deltaPerTick : 0) + (downPressed ? deltaPerTick : 0);
            panDelta.x = (leftPressed ? -deltaPerTick : 0) + (rightPressed ? deltaPerTick : 0);
            renderNode.applyPanningDelta(panDelta);
        }

        // Shift Layers
        if (layerUpPressed) {
            renderNode.setLayer(renderNode.getLayer() + 1);
        } else if (layerDownPressed) {
            renderNode.setLayer(renderNode.getLayer() - 1);
        }
        layerUpPressed = false;
        layerDownPressed = false;
    }

    @Override
    public void mouseScrolled(ICRenderNode renderNode, Vector3 mousePosition, double scroll) {
        renderNode.moveZoomAt(mousePosition, scroll * 0.3D);
    }

    @Override
    public void buildTooltip(ICRenderNode renderNode, Vector3 mousePosition, boolean isFirstHit, List<Component> tooltip) {
        if (!isFirstHit) return;

        TileCoord pos = IICEditorTool.toNearestPosition(mousePosition);
        editor.getTileMap().getBaseTile(pos).ifPresent(tile -> tile.buildToolTip(tooltip));

        for (ICompileOverlayRenderer overlay : overlays) {
            overlay.buildTooltip(renderNode, mousePosition, tooltip);
        }
    }

    @Override
    public void onRenderOverlay(ICRenderNode renderNode, Vector3 mousePosition, boolean isFirstHit, CCRenderState ccrs, MultiBufferSource getter, PoseStack matrixStack) {
        for (ICompileOverlayRenderer overlay : overlays) {
            overlay.renderOverlay(renderNode, mousePosition, isFirstHit, ccrs, getter, matrixStack);
        }
    }

    //endregion

    //region Utilities
    public static void appendProblemsInfo(ICCompilerLog log, List<Component> tooltip) {
        Component errorText = log.getErrorCount() == 0 ?
                new TextComponent("<!> ")
                        .append(new TranslatableComponent(UL_NO_ERRORS))
                        .withStyle(UNIFORM_GRAY) :
                new TextComponent("<!> ")
                        .withStyle(UNIFORM_RED)
                        .append(new TranslatableComponent(UL_UNIT_ERRORS, log.getErrorCount())
                                .withStyle(UNIFORM_GRAY));

        Component warningText = log.getWarningCount() == 0 ?
                new TextComponent("<!> ")
                        .append(new TranslatableComponent(UL_NO_WARNINGS))
                        .withStyle(UNIFORM_GRAY) :
                new TextComponent("<!> ")
                        .withStyle(ICWorkbenchEditor.UNIFORM_YELLOW)
                        .append(new TranslatableComponent(UL_UNIT_WARNINGS, log.getWarningCount())
                                .withStyle(UNIFORM_GRAY));

        tooltip.add(errorText);
        tooltip.add(warningText);
    }
    //endregion

    public class CompileButton extends AbstractButtonNode {

        @Override
        protected void onButtonClicked() {
            editor.getStateMachine().sendCompileButtonClicked();
        }
        @Override
        protected boolean isButtonDisabled() {
            return !editor.getStateMachine().canTriggerCompile();
        }

        @Override
        protected void drawButtonBody(PoseStack stack, boolean mouseover) {
            RenderSystem.setShaderTexture(0, TAB_BACKGROUND);

            if (editor.getStateMachine().isCompiling()) {
                // Spinner
                long time = Minecraft.getInstance().level.getGameTime();
                int progress = (int) (time / 2) % 8;
                int u = 305 + (15 * progress);
                int v = 26;
                blitCentered(stack, u, v, 14, 14);
            } else {
                // Hammer icon
                blitCentered(stack, 305, 11, 14, 14);
            }
        }

        private void blitCentered(PoseStack stack, int u, int v, int width, int height) {
            GuiComponent.blit(stack,
                    getFrame().x() + (getFrame().width() - width) / 2,
                    getFrame().y() + (getFrame().height() - height) / 2, u, v,
                    width, height, 512, 512);
        }

        @Override
        protected void buildTooltip(List<Component> tooltip) {
            tooltip.add(new TranslatableComponent(UL_COMPILE));
            if (editor.getStateMachine().canTriggerCompile()) {
                tooltip.add(new TranslatableComponent(UL_COMPILE_READY).withStyle(UNIFORM_GRAY));
            }
        }
    }

    private class ProblemsTab extends SimpleUVTab {

        public ProblemsTab(AbstractGuiNode tabBodyNode, String unlocalTabName, TabButtonNode.TabSide side, int u, int v, ResourceLocation texture) {
            super(tabBodyNode, unlocalTabName, side, u, v, texture);
        }

        @Override
        protected StatusDot getStatusDot() {
            if (editor.getStateMachine().getCompilerLog().getErrorCount() > 0) {
                return StatusDot.RED;
            } else if (editor.getStateMachine().getCompilerLog().getWarningCount() > 0) {
                return StatusDot.YELLOW;
            } else {
                return StatusDot.NONE;
            }
        }

        @Override
        protected void buildTooltip(List<Component> tooltip) {
            super.buildTooltip(tooltip);
            ICCompilerLog log = editor.getStateMachine().getCompilerLog();
            appendProblemsInfo(log, tooltip);
        }
    }
}
