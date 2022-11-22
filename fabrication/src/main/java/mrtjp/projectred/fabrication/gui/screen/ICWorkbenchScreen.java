package mrtjp.projectred.fabrication.gui.screen;

import codechicken.lib.colour.EnumColour;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.fabrication.ProjectRedFabrication;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.gui.SimpleUVTab;
import mrtjp.projectred.fabrication.gui.TabButtonNode;
import mrtjp.projectred.fabrication.gui.TabControllerNode;
import mrtjp.projectred.fabrication.init.FabricationReferences;
import mrtjp.projectred.fabrication.tile.ICWorkbenchTile;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.AbstractGuiNode;
import mrtjp.projectred.redui.ItemStackNode;
import mrtjp.projectred.redui.RedUIScreen;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.ItemStack;

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
        tabControllerNode.addButtonForTab(new SimpleUVTab(editTab, UL_TAB_EDIT, TabButtonNode.TabSide.LEFT, 420, 16));
        tabControllerNode.addButtonForTab(new SimpleUVTab(compileTab, UL_TAB_COMPILE, TabButtonNode.TabSide.LEFT, 420, 31));

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
}
