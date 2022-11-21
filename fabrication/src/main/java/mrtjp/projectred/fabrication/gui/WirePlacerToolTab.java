package mrtjp.projectred.fabrication.gui;

import codechicken.lib.math.MathHelper;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Scale;
import codechicken.lib.vec.TransformationList;
import codechicken.lib.vec.Translation;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.fabrication.editor.tools.WirePlacerTool;
import mrtjp.projectred.fabrication.engine.wires.ICWireTileType;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchScreen;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.transmission.client.WireModelRenderer;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.GuiComponent;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.texture.OverlayTexture;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;

import java.util.List;

public class WirePlacerToolTab extends ICEditorToolTab {

    private final WirePlacerTool tool;

    public WirePlacerToolTab(ICEditorToolManager manager, WirePlacerTool tool) {
        super(manager, tool);
        this.tool = tool;
        construct();
    }

    private void addWireButton(ICWireTileType type, boolean fullRow) {
        ButtonController buttonController = new ButtonController() {
            @Override public void getTooltip(List<Component> tooltip) { tooltip.add(new TextComponent(type.toString())); }
            @Override public void onClick() { tool.setWireType(type); }
            @Override public boolean isSelected() { return tool.getWireType() == type; }

            @Override
            public void renderIcon(PoseStack stack, Point absPos, float partialFrame) {
                MultiBufferSource.BufferSource getter = Minecraft.getInstance().renderBuffers().bufferSource();
                CCRenderState ccrs = CCRenderState.instance();
                ccrs.reset();
                ccrs.bind(RenderType.cutout(), getter, stack);
                ccrs.overlay = OverlayTexture.NO_OVERLAY;
                ccrs.brightness = 0xF000F0;

                double scale = 10/16D;
                TransformationList t = new TransformationList(
                        new Rotation(90.0F * MathHelper.torad, 1.0F, 0.0F, 0.0F),
                        new Scale(16.0F * scale, -16.0F * scale, 16.0F * scale),
                        new Translation(absPos.x + 8 - scale*8, absPos.y + 8 - scale*8, 0.0F)
                );

                WireModelRenderer.renderInventory(ccrs, type.multipartType.getThickness(), type.multipartType.getItemColour() << 8 | 0xFF, type.multipartType.getTextures().get(0), t);

                getter.endBatch();
            }
        };

        if (fullRow) {
            addFullRowButton(buttonController);
        } else {
            addSingleButton(buttonController);
        }
    }

    private void construct() {

        addGroup("Redwire");

        // Alloy wire
        addWireButton(ICWireTileType.RED_ALLOY, true);

        // Insulated wires
        for (ICWireTileType type : ICWireTileType.INSULATED)
            addWireButton(type, false);

        addGroup("Bundled");

        // Bundled neutral wire
        addWireButton(ICWireTileType.BUNDLED_NEUTRAL, true);

        // Bundled coloured wires
        for (ICWireTileType type : ICWireTileType.BUNDLED_COLOURED)
            addWireButton(type, false);

    }

    @Override
    public TabButtonNode createButtonNode() {
        return new TabButtonNode(this, TabButtonNode.TabSide.LEFT) {
            @Override
            public void renderIcon(PoseStack stack, Point mouse, float partialFrame) {

                RenderSystem.setShaderTexture(0, ICWorkbenchScreen.BACKGROUND);
                GuiComponent.blit(stack, getFrame().x() + 3, getFrame().y() + 3, 390, 46, 14, 14, 512, 512);
            }

            @Override
            public void buildTooltip(List<Component> tooltip) {
                tooltip.add(new TextComponent("Wires")); //TODO Localize
            }
        };
    }
}
