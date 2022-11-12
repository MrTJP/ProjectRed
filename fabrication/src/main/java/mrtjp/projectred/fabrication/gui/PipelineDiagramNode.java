package mrtjp.projectred.fabrication.gui;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchInfoTab;
import mrtjp.projectred.fabrication.init.FabricationReferences;
import mrtjp.projectred.fabrication.lithography.LithographyPipeline;
import mrtjp.projectred.fabrication.lithography.YieldCalculator;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.AbstractGuiNode;
import mrtjp.projectred.redui.ItemStackNode;
import net.minecraft.client.gui.GuiComponent;
import net.minecraft.world.item.ItemStack;

import java.util.ArrayList;

public class PipelineDiagramNode extends AbstractGuiNode {

    private final YieldCalculator yieldCalculator;

    private BasicPipelineItems basicPipelineItems;
    private AbstractGuiNode advancedPipelineItems;

    public PipelineDiagramNode(YieldCalculator yieldCalculator) {
        this.yieldCalculator = yieldCalculator;
        setSize(280, 56);
        initSubNodes();
    }

    private void initSubNodes() {

        basicPipelineItems = new BasicPipelineItems();
        addChild(basicPipelineItems);

        advancedPipelineItems = new AbstractGuiNode() { }; //TODO
        addChild(advancedPipelineItems);

        updatePipelines();
    }

    @Override
    public void drawBack(PoseStack stack, Point mouse, float partialFrame) {

    }

    @Override
    public void update() {
        updatePipelines();
    }

    private void updatePipelines() {
        boolean isBasic = yieldCalculator.getPipeline() == LithographyPipeline.BASIC;
        basicPipelineItems.setHidden(!isBasic);
        advancedPipelineItems.setHidden(isBasic);
    }

    private class BasicPipelineItems extends AbstractGuiNode {

        private final ArrayList<ItemStackNode> items = new ArrayList<>();

        public BasicPipelineItems() {
            addItem(new ItemStack(FabricationReferences.IC_BLUEPRINT_ITEM), 30, 5);
            addItem(new ItemStack(FabricationReferences.PLOTTING_TABLE_BLOCK), 64, 5);
            addItem(new ItemStack(FabricationReferences.BLANK_PHOTOMASK_ITEM), 64, 39);
            addItem(new ItemStack(FabricationReferences.PHOTOMASK_SET_ITEM), 98, 5);
            addItem(new ItemStack(FabricationReferences.LITHOGRAPHY_TABLE_BLOCK), 132, 5);
            addItem(new ItemStack(FabricationReferences.ROUGH_SILICON_WAFER_ITEM), 132, 39);
            addItem(new ItemStack(FabricationReferences.VALID_DIE_ITEM), 166, 5);
            addItem(new ItemStack(FabricationReferences.PACKAGING_TABLE_BLOCK), 200, 5);
            addItem(new ItemStack(FabricationReferences.FABRICATED_GATE_ITEM), 234, 5);
        }

        private void addItem(ItemStack stack, int x, int y) {
            ItemStackNode stackNode = new ItemStackNode(stack);
            stackNode.setPosition(x, y);
            items.add(stackNode);
            addChild(stackNode);
        }

        @Override
        public void drawBack(PoseStack stack, Point mouse, float partialFrame) {
            RenderSystem.setShaderTexture(0, ICWorkbenchInfoTab.TAB_BACKGROUND);

            // Draw the diagram centered in this node
            GuiComponent.blit(stack, getFrame().x(), getFrame().y(), 1, 223, 280, 56, 512, 512);
        }

        @Override
        public void update() {
            // Update the wafer input slot to the Lithography table
            ItemStackNode node = items.get(5); //Wafer slot
            switch (yieldCalculator.getWaferType()) {
                case ROUGH_WAFER:
                    node.setItemStack(new ItemStack(FabricationReferences.ROUGH_SILICON_WAFER_ITEM));
                    break;
                case POLISHED_WAFER:
                case PURIFIED_WAFER:
                    node.setItemStack(ItemStack.EMPTY); //TODO
                    break;
            }
        }

        //TODO update final Gate stack to reflect proper IO
    }
}
