package mrtjp.projectred.fabrication.gui;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.fabrication.editor.tools.IICEditorTool;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchScreen;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.AbstractGuiNode;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.GuiComponent;
import net.minecraft.client.resources.sounds.SimpleSoundInstance;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.sounds.SoundEvents;

import java.util.LinkedList;
import java.util.List;

public class ICEditorToolTab extends AbstractGuiNode implements TabControllerNode.IToolbarTab {

    private static final int GROUP_U = 390;
    private static final int GROUP_V = 61;
    private static final int GROUP_WIDTH = 64;
    private static final int GROUP_HEIGHT = 16;

    private static final int FULL_BUTTON_U = 390;
    private static final int FULL_BUTTON_V = 77;
    private static final int FULL_BUTTON_WIDTH = 64;
    private static final int FULL_BUTTON_HEIGHT = 16;

    private static final int SINGLE_BUTTON_U = FULL_BUTTON_U + 64;
    private static final int SINGLE_BUTTON_V = FULL_BUTTON_V;
    private static final int SINGLE_BUTTON_WIDTH = 16;
    private static final int SINGLE_BUTTON_HEIGHT = 16;

    private static final int BUTTON_SELECTED_SHIFT_U = 0;
    private static final int BUTTON_SELECTED_SHIFT_V = 16;
    private static final int BUTTON_MOUSEOVER_SHIFT_U = 0;
    private static final int BUTTON_MOUSEOVER_SHIFT_V = 32;

    protected final ICEditorToolManager manager;
    protected final IICEditorTool tool;

    private int rowIndex = 0;
    private int columnIndex = 0;

    public ICEditorToolTab(ICEditorToolManager manager, IICEditorTool tool) {
        this.manager = manager;
        this.tool = tool;

        this.setSize(84, 222);
    }

    private void setAndIncrGridPos(AbstractGuiNode node, int cellWidth) {
        if (columnIndex + cellWidth > 4) {
            columnIndex = 0;
            rowIndex++;
        }

        node.setPosition(7 + columnIndex * SINGLE_BUTTON_WIDTH, 18 + rowIndex * SINGLE_BUTTON_HEIGHT);

        columnIndex += cellWidth;
        if (columnIndex > 4) {
            columnIndex = 0;
            rowIndex++;
        }
    }

    protected void addGroup(String unlocal) {
        addGroup(new TranslatableComponent(unlocal));
    }

    protected void addGroup(Component groupName) {

        GroupHeaderNode header = new GroupHeaderNode(groupName);
        setAndIncrGridPos(header, 4);
        addChild(header);
    }

    protected void addFullRowButton(ButtonController controller) {

        FullRowButtonNode button = new FullRowButtonNode(controller);
        setAndIncrGridPos(button, 4);
        addChild(button);
    }

    protected void addSingleButton(ButtonController controller) {

        SingleColumnButtonNode button = new SingleColumnButtonNode(controller);
        setAndIncrGridPos(button, 1);
        addChild(button);
    }

    public IICEditorTool getTool() {
        return tool;
    }

    @Override
    public void drawBack(PoseStack stack, Point mouse, float partialFrame) {
        RenderSystem.setShaderTexture(0, ICWorkbenchScreen.BACKGROUND);

        GuiComponent.blit(stack, getPosition().x, getPosition().y, 305, 0, 84, 222, 512, 512);

        //TODO: Render tool name on header
    }

    @Override
    public boolean hasBody() {
        return true;
    }

    @Override
    public void onTabClosed() {
        setHidden(true);
    }

    @Override
    public void onTabOpened() {
        setHidden(false);
        manager.swapTools(tool.getToolType());
    }

    @Override
    public void onTabMinimized() {
        setHidden(true);
    }

    @Override
    public TabButtonNode createButtonNode() {
        return new TabButtonNode(this, TabButtonNode.TabSide.LEFT) {

            @Override
            public void renderIcon(PoseStack stack, Point mouse, float partialFrame) {
            }

            @Override
            public void buildTooltip(List<Component> tooltip) {
                tooltip.add(new TextComponent("//TODO implement tab"));
            }
        };
    }

    protected interface ButtonController {
        void getTooltip(List<Component> tooltip);
        void onClick();
        boolean isSelected();
        void renderIcon(PoseStack stack, Point absPos, float partialFrame);
    }

    private static class GroupHeaderNode extends AbstractGuiNode {

        private final Component title;

        public GroupHeaderNode(Component title) {
            this.title = title;
            this.setSize(GROUP_WIDTH, GROUP_HEIGHT);
        }

        @Override
        public void drawBack(PoseStack stack, Point mouse, float partialFrame) {
            RenderSystem.setShaderTexture(0, ICWorkbenchScreen.BACKGROUND);

            GuiComponent.blit(stack, getPosition().x, getPosition().y, GROUP_U, GROUP_V, getFrame().width(), getFrame().height(), 512, 512);

            Font fontRenderer = getRoot().getFontRenderer();
            fontRenderer.draw(stack, title, getPosition().x + 2, getPosition().y + GROUP_HEIGHT / 2f - fontRenderer.lineHeight / 2f, 0xFFFFFF);
        }
    }

    private static class AbstractButtonNode extends AbstractGuiNode {

        private final ButtonController controller;
        private final Point uvBg;
        private final Point uvBgSelectedShift;
        private final Point uvMouseOverShift;

        public AbstractButtonNode(ButtonController controller, int width, int height, Point uvBg, Point uvBgSelectedShift, Point uvMouseOverShift) {
            this.controller = controller;
            this.setSize(width, height);
            this.uvBg = uvBg;
            this.uvBgSelectedShift = uvBgSelectedShift;
            this.uvMouseOverShift = uvMouseOverShift;
        }

        @Override
        public void drawBack(PoseStack stack, Point mouse, float partialFrame) {
            RenderSystem.enableBlend();
            RenderSystem.setShaderTexture(0, ICWorkbenchScreen.BACKGROUND);

            boolean mouseover = getFrame().contains(mouse) && isFirstHit(mouse);
            boolean selected = controller.isSelected();

            int x = getPosition().x;
            int y = getPosition().y;
            int w = getFrame().width();
            int h = getFrame().height();

            // Background
            int uBackground = uvBg.x + (selected ? uvBgSelectedShift.x : 0);
            int vBackground = uvBg.y + (selected ? uvBgSelectedShift.y : 0);
            GuiComponent.blit(stack, x, y, uBackground, vBackground, w, h, 512, 512);

            // Mouseover layer
            if (selected || mouseover) {
                int uMouseOver = uvBg.x + uvMouseOverShift.x;
                int vMouseOver = uvBg.y + uvMouseOverShift.y;
                GuiComponent.blit(stack, x, y, uMouseOver, vMouseOver, w, h, 512, 512);
            }

            RenderSystem.disableBlend();

            // Icon
            controller.renderIcon(stack, getPosition(), partialFrame);
        }

        @Override
        public void drawFront(PoseStack stack, Point mouse, float partialFrame) {

            if (!isFirstHit(mouse)) return;

            List<Component> tooltip = new LinkedList<>();
            controller.getTooltip(tooltip);

            renderTooltip(stack, mouse, tooltip);
        }

        @Override
        public boolean mouseClicked(Point p, int glfwMouseButton, boolean consumed) {
            if (!consumed && isFirstHit(p)) {
                getRoot().getMinecraft().getSoundManager().play(SimpleSoundInstance.forUI(SoundEvents.UI_BUTTON_CLICK, 1));
                controller.onClick();
                return true;
            }
            return false;
        }
    }

    private static class SingleColumnButtonNode extends AbstractButtonNode {
        public SingleColumnButtonNode(ButtonController controller) {
            super(controller, SINGLE_BUTTON_WIDTH, SINGLE_BUTTON_HEIGHT,
                    new Point(SINGLE_BUTTON_U, SINGLE_BUTTON_V),
                    new Point(BUTTON_SELECTED_SHIFT_U, BUTTON_SELECTED_SHIFT_V),
                    new Point(BUTTON_MOUSEOVER_SHIFT_U, BUTTON_MOUSEOVER_SHIFT_V));
        }
    }

    private static class FullRowButtonNode extends AbstractButtonNode {
        public FullRowButtonNode(ButtonController controller) {
            super(controller, FULL_BUTTON_WIDTH, FULL_BUTTON_HEIGHT,
                    new Point(FULL_BUTTON_U, FULL_BUTTON_V),
                    new Point(BUTTON_SELECTED_SHIFT_U, BUTTON_SELECTED_SHIFT_V),
                    new Point(BUTTON_MOUSEOVER_SHIFT_U, BUTTON_MOUSEOVER_SHIFT_V));
        }
    }
}
