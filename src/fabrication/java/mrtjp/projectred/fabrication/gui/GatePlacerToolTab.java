package mrtjp.projectred.fabrication.gui;

import codechicken.lib.math.MathHelper;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.texture.TextureUtils;
import codechicken.lib.vec.Rotation;
import codechicken.lib.vec.Scale;
import codechicken.lib.vec.TransformationList;
import codechicken.lib.vec.Translation;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.core.vec.Point;
import mrtjp.projectred.fabrication.editor.tools.GatePlacerTool;
import mrtjp.projectred.fabrication.engine.gates.ICGateTileType;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchScreen;
import mrtjp.projectred.integration.RenderGate;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.AbstractGui;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.util.text.ITextProperties;
import net.minecraft.util.text.StringTextComponent;

import java.util.List;

public class GatePlacerToolTab extends ICEditorToolTab {

    private final GatePlacerTool tool;

    public GatePlacerToolTab(ICEditorToolManager manager, GatePlacerTool tool) {
        super(manager, tool);
        this.tool = tool;
        construct();
    }

    private void addGateButton(ICGateTileType type) {
        ButtonController buttonController = new ButtonController() {
            @Override public void getTooltip(List<ITextProperties> tooltip) { tooltip.add(new StringTextComponent(type.toString())); }
            @Override public void onClick() { tool.setGateType(type); }
            @Override public boolean isSelected() { return tool.getGateType() == type; }

            @Override
            public void renderIcon(MatrixStack stack, Point absPos, float partialFrame) {
                IRenderTypeBuffer.Impl getter = Minecraft.getInstance().renderBuffers().bufferSource();
                CCRenderState ccrs = CCRenderState.instance();
                ccrs.reset();
                ccrs.bind(RenderType.cutout(), getter, stack);

                double scale = 10/16D;
                TransformationList t = new TransformationList(
                        new Rotation(90.0F * MathHelper.torad, 1.0F, 0.0F, 0.0F),
                        new Scale(16.0F * scale, -16.0F * scale, 16.0F * scale),
                        new Translation(absPos.x() + 8 - scale*8, absPos.y() + 8 - scale*8, 0.0F)
                );

                RenderGate.instance().renderInv(type.renderIndex, 0, t, ccrs);

                getter.endBatch();
            }
        };

        this.addSingleButton(buttonController);
    }

    private void construct() {

        addGroup("IO");
        addGateButton(ICGateTileType.IO);

        addGroup("Basic");
        addGateButton(ICGateTileType.OR);
        addGateButton(ICGateTileType.NOR);
        addGateButton(ICGateTileType.NOT);
        addGateButton(ICGateTileType.AND);
        addGateButton(ICGateTileType.NAND);
        addGateButton(ICGateTileType.XOR);
        addGateButton(ICGateTileType.XNOR);
        addGateButton(ICGateTileType.BUFFER);
        addGateButton(ICGateTileType.MULTIPLEXER);

        addGroup("Timing");
        addGateButton(ICGateTileType.PULSE);
        addGateButton(ICGateTileType.REPEATER);
        addGateButton(ICGateTileType.RANDOMIZER);

        addGroup("Latches");
        addGateButton(ICGateTileType.SR_LATCH);
        addGateButton(ICGateTileType.TOGGLE_LATCH);
        addGateButton(ICGateTileType.TRANSPARENT_LATCH);
    }

    @Override
    public TabButtonNode createButtonNode() {
        return new TabButtonNode(this, TabButtonNode.TabSide.LEFT) {
            @Override
            public void renderIcon(MatrixStack stack, Point mouse, float partialFrame) {
                TextureUtils.changeTexture(ICWorkbenchScreen.BACKGROUND);
                AbstractGui.blit(stack, getFrame().x() + 3, getFrame().y() + 3, 390, 31, 14, 14, 512, 512);
            }

            @Override
            public void buildTooltip(List<ITextProperties> tooltip) {
                tooltip.add(new StringTextComponent("Logic")); //TODO Localize
            }
        };
    }
}
