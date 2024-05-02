package mrtjp.projectred.fabrication.gui;

import mrtjp.projectred.fabrication.gui.screen.ICWorkbenchInfoTab;
import mrtjp.projectred.fabrication.lithography.LithographyPipeline;
import mrtjp.projectred.fabrication.lithography.YieldCalculator;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.redui.AbstractGuiNode;
import mrtjp.projectred.redui.ItemStackNode;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.world.item.ItemStack;

import java.util.ArrayList;

import static mrtjp.projectred.fabrication.init.FabricationBlocks.*;
import static mrtjp.projectred.fabrication.init.FabricationItems.*;
import static mrtjp.projectred.fabrication.init.FabricationParts.FABRICATED_GATE_ITEM;

@SuppressWarnings("NotNullFieldNotInitialized")
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
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {

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
            addItem(new ItemStack(IC_BLUEPRINT_ITEM.get()), 30, 5);
            addItem(new ItemStack(PLOTTING_TABLE_BLOCK.get()), 64, 5);
            addItem(new ItemStack(BLANK_PHOTOMASK_ITEM.get()), 64, 39);
            addItem(new ItemStack(PHOTOMASK_SET_ITEM.get()), 98, 5);
            addItem(new ItemStack(LITHOGRAPHY_TABLE_BLOCK.get()), 132, 5);
            addItem(new ItemStack(ROUGH_SILICON_WAFER_ITEM.get()), 132, 39);
            addItem(new ItemStack(VALID_DIE_ITEM.get()), 166, 5);
            addItem(new ItemStack(PACKAGING_TABLE_BLOCK.get()), 200, 5);
            addItem(new ItemStack(FABRICATED_GATE_ITEM.get()), 234, 5);
        }

        private void addItem(ItemStack stack, int x, int y) {
            ItemStackNode stackNode = new ItemStackNode(stack);
            stackNode.setPosition(x, y);
            items.add(stackNode);
            addChild(stackNode);
        }

        @Override
        public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {
            // Draw the diagram centered in this node
            graphics.blit(ICWorkbenchInfoTab.TAB_BACKGROUND, getFrame().x(), getFrame().y(), 1, 223, 280, 56, 512, 512);
        }

        @Override
        public void update() {
            // Update the wafer input slot to the Lithography table
            ItemStackNode node = items.get(5); //Wafer slot
            switch (yieldCalculator.getWaferType()) {
                case ROUGH_WAFER:
                    node.setItemStack(new ItemStack(ROUGH_SILICON_WAFER_ITEM.get()));
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
