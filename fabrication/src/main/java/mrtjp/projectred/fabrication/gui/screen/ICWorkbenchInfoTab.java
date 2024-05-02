package mrtjp.projectred.fabrication.gui.screen;

import codechicken.lib.colour.EnumColour;
import mrtjp.fengine.TileCoord;
import mrtjp.projectred.fabrication.ProjectRedFabrication;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.gui.ButtonArrayNode;
import mrtjp.projectred.fabrication.gui.PipelineDiagramNode;
import mrtjp.projectred.fabrication.gui.VerticalListNode;
import mrtjp.projectred.fabrication.lithography.LithographyPipeline;
import mrtjp.projectred.fabrication.lithography.ProcessNode;
import mrtjp.projectred.fabrication.lithography.WaferType;
import mrtjp.projectred.fabrication.lithography.YieldCalculator;
import mrtjp.projectred.lib.Point;
import mrtjp.projectred.lib.Rect;
import mrtjp.projectred.redui.AbstractGuiNode;
import mrtjp.projectred.redui.ScrollBarNode;
import mrtjp.projectred.redui.TextBoxNode;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;

import javax.annotation.Nullable;

import static mrtjp.projectred.fabrication.init.FabricationUnlocal.*;

public class ICWorkbenchInfoTab extends AbstractGuiNode {

    public static final ResourceLocation TAB_BACKGROUND = new ResourceLocation(ProjectRedFabrication.MOD_ID, "textures/gui/info_tab.png");

    private final ICWorkbenchEditor editor;

    private final YieldCalculator yieldCalculator = new YieldCalculator();

    private @Nullable VerticalListNode listNode;

    public ICWorkbenchInfoTab(ICWorkbenchEditor editor) {
        this.editor = editor;

        setSize(304, 222);
        initSubNodes();
    }

    private void initSubNodes() {

        listNode = new VerticalListNode();
        listNode.setPosition(6, 17);
        listNode.setSize(280, 198);
        addChild(listNode);

        NameTextBox nameTextBox = new NameTextBox();
        nameTextBox.setSize(80, 18);

        listNode.addTitleRow(Component.translatable(UL_BLUEPRINT_INFO));
        listNode.addTextWithNodeRow(Component.translatable(UL_BLUEPRINT_NAME), nameTextBox);
        listNode.addKeyValueRow(Component.translatable(UL_BLUEPRINT_OWNER), () -> Component.literal("//TODO"));

        listNode.addKeyValueRow(Component.translatable(UL_BLUEPRINT_DIM), () -> {
            TileCoord dimensions = editor.getTileMap().getDimensions();
            return Component.translatable(UL_DIMENSIONS_TILES, dimensions.x, dimensions.z);
        });

        listNode.addKeyValueRow(Component.translatable(UL_BLUEPRINT_LAYERS), () -> {
            TileCoord dimensions = editor.getTileMap().getDimensions();
            return Component.literal(String.valueOf(dimensions.y));
        });

        listNode.addTitleRow(Component.translatable(UL_YIELD_CALCULATOR));

        ButtonArrayNode pipelineButtonGrid = createPipelineButtons();
        pipelineButtonGrid.setGridSize(128, 18);
        listNode.addTextWithNodeRow(Component.translatable(UL_LITHOGRAPHY_PIPELINE), pipelineButtonGrid);

        ButtonArrayNode processNodeButtonGrid = createProcessNodeButtons();
        processNodeButtonGrid.setGridSize(128, 18);
        listNode.addTextWithNodeRow(Component.translatable(UL_PROCESS_NODE), processNodeButtonGrid);

        ButtonArrayNode waferTypeButtonGrid = createWaferTypeButtons();
        waferTypeButtonGrid.setGridSize(128, 18);
        listNode.addTextWithNodeRow(Component.translatable(UL_WAFER_TYPE), waferTypeButtonGrid);

        PipelineDiagramNode diagramNode = new PipelineDiagramNode(yieldCalculator);
        listNode.addSingleNodeRow(diagramNode);

        ScrollBarNode scrollBarNode = new ScrollBarNode(ScrollBarNode.ScrollAxis.VERTICAL) {
            @Override
            protected void drawSlider(GuiGraphics graphics, Rect sliderFrame) {
                graphics.blit(TAB_BACKGROUND, sliderFrame.x(), sliderFrame.y(), 305, 58, sliderFrame.width(), sliderFrame.height(), 512, 512);
            }

            @Override
            protected void adjustContent(double scrollPercentage) {
                listNode.setScrollPercentage(scrollPercentage);
            }
        };

        listNode.addKeyValueRow(Component.translatable(UL_DIE_SIZE), yieldCalculator::getDieDimensionsText);
        listNode.addKeyValueRow(Component.translatable(UL_WAFER_SIZE), yieldCalculator::getWaferDimensionsText);
        listNode.addKeyValueRow(Component.translatable(UL_DIES_PER_WAFER), yieldCalculator::getDieCountDimensionsText);
        listNode.addKeyValueRow(Component.translatable(UL_SINGLE_LAYER_YIELD), yieldCalculator::getSingleLayerYieldText);
        listNode.addKeyValueRow(Component.translatable(UL_YIELD), yieldCalculator::getYieldText);

        scrollBarNode.setPosition(290, 17);
        scrollBarNode.setSize(8, 198);
        scrollBarNode.setSliderSize(8, 16);
        addChild(scrollBarNode);
    }

    @Override
    public void update() {
        super.update();
        TileCoord dimensions = editor.getTileMap().getDimensions();
        yieldCalculator.setTileMapSize(dimensions.x, dimensions.z, dimensions.y);
    }

    @Override
    public void drawBack(GuiGraphics graphics, Point mouse, float partialFrame) {
        graphics.blit(TAB_BACKGROUND, getFrame().x(), getFrame().y(), 0, 0, getFrame().width(), getFrame().height(), 512, 512);

        // Blueprint name in top left corner
        graphics.drawString(getRoot().getFontRenderer(), editor.getIcName(), getFrame().x() + 8, getFrame().y() + 6, EnumColour.GRAY.argb(), false);
    }

    private ButtonArrayNode createPipelineButtons() {
        ButtonArrayNode.Listener listener =  new ButtonArrayNode.Listener() {
            @Override
            public String getButtonText(int index) {
                return LithographyPipeline.values()[index].getUnlocalizedName();
            }

            @Override
            public void onButtonClicked(int index) {
                LithographyPipeline pipeline = LithographyPipeline.values()[index];
                yieldCalculator.setPipeline(pipeline);
                if (!pipeline.isProcessNodeValid(yieldCalculator.getProcessNode())) {
                    yieldCalculator.setProcessNode(ProcessNode.PROCESS_64NM);
                }
                if (!pipeline.isWaferTypeValid(yieldCalculator.getWaferType())) {
                    yieldCalculator.setWaferType(WaferType.ROUGH_WAFER);
                }
            }

            @Override
            public boolean isButtonEnabled(int index) {
                return true;
            }

            @Override
            public boolean isButtonSelected(int index) {
                return yieldCalculator.getPipeline().ordinal() == index;
            }
        };
        return new ButtonArrayNode(listener, 1, LithographyPipeline.values().length, 2);
    }

    private ButtonArrayNode createProcessNodeButtons() {
        ButtonArrayNode.Listener listener = new ButtonArrayNode.Listener() {
            @Override
            public String getButtonText(int index) {
                return ProcessNode.values()[index].getDisplayName();
            }

            @Override
            public void onButtonClicked(int index) {
                yieldCalculator.setProcessNode(ProcessNode.values()[index]);
            }

            @Override
            public boolean isButtonEnabled(int index) {
                return yieldCalculator.getPipeline().isProcessNodeValid(ProcessNode.values()[index]);
            }

            @Override
            public boolean isButtonSelected(int index) {
                return yieldCalculator.getProcessNode().ordinal() == index;
            }
        };
        return new ButtonArrayNode(listener, 1, ProcessNode.values().length, 2);
    }

    private ButtonArrayNode createWaferTypeButtons() {
        ButtonArrayNode.Listener listener = new ButtonArrayNode.Listener() {
            @Override
            public String getButtonText(int index) {
                return WaferType.values()[index].getUnlocalizedName();
            }

            @Override
            public void onButtonClicked(int index) {
                yieldCalculator.setWaferType(WaferType.values()[index]);
            }

            @Override
            public boolean isButtonEnabled(int index) {
                return yieldCalculator.getPipeline().isWaferTypeValid(WaferType.values()[index]);
            }

            @Override
            public boolean isButtonSelected(int index) {
                return yieldCalculator.getWaferType().ordinal() == index;
            }
        };
        return new ButtonArrayNode(listener, 1, WaferType.values().length, 2);
    }

    private class NameTextBox extends TextBoxNode {

        public NameTextBox() {
            super(editor.getIcName());
        }

        @Override
        public void update() {
            super.update();
            if (!getText().equals(editor.getIcName()) && !isEditing()) {
                setText(editor.getIcName());
            }
        }

        @Override
        protected String getSuggestionString() {
            return editor.getIcName();
        }

        @Override
        protected void onTextChanged() {
        }

        @Override
        protected void onReturnPressed() {
            editor.sendNewICName(getText());
        }
    }
}
