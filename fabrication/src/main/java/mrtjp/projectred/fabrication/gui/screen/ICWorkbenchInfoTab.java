package mrtjp.projectred.fabrication.gui.screen;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.texture.TextureUtils;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
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
import net.minecraft.client.gui.GuiComponent;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.resources.ResourceLocation;

public class ICWorkbenchInfoTab extends AbstractGuiNode {

    public static final ResourceLocation TAB_BACKGROUND = new ResourceLocation(ProjectRedFabrication.MOD_ID, "textures/gui/info_tab.png");

    private final ICWorkbenchEditor editor;

    private final YieldCalculator yieldCalculator = new YieldCalculator();

    private VerticalListNode listNode;

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

        //TODO localize
        listNode.addTitleRow(new TranslatableComponent("Info"));
        listNode.addTextWithNodeRow(new TranslatableComponent("Name"), nameTextBox);
        listNode.addKeyValueRow(new TranslatableComponent("Owner"), () -> new TranslatableComponent("//TODO"));

        listNode.addKeyValueRow(new TranslatableComponent("Dimensions"), () -> {
            TileCoord dimensions = editor.getTileMap().getDimensions();
            Component tilesWord = new TranslatableComponent("tiles");
            Component byWord = new TranslatableComponent("by");
            return new TextComponent("" + dimensions.x).append(" ").append(tilesWord).append(" ").append(byWord).append(" ").append("" + dimensions.z).append(" ").append(tilesWord);
        });

        listNode.addKeyValueRow(new TranslatableComponent("Layers"), () -> {
            TileCoord dimensions = editor.getTileMap().getDimensions();
            return new TextComponent("" + dimensions.y);
        });

        listNode.addTitleRow(new TranslatableComponent("Yield Calculator"));

        ButtonArrayNode pipelineButtonGrid = createPipelineButtons();
        pipelineButtonGrid.setGridSize(128, 18);
        listNode.addTextWithNodeRow(new TranslatableComponent("Lithography Pipeline"), pipelineButtonGrid);

        ButtonArrayNode processNodeButtonGrid = createProcessNodeButtons();
        processNodeButtonGrid.setGridSize(128, 18);
        listNode.addTextWithNodeRow(new TranslatableComponent("Process Node"), processNodeButtonGrid);

        ButtonArrayNode waferTypeButtonGrid = createWaferTypeButtons();
        waferTypeButtonGrid.setGridSize(128, 18);
        listNode.addTextWithNodeRow(new TranslatableComponent("Wafer Type"), waferTypeButtonGrid);

        PipelineDiagramNode diagramNode = new PipelineDiagramNode(yieldCalculator);
        listNode.addSingleNodeRow(diagramNode);

        ScrollBarNode scrollBarNode = new ScrollBarNode(ScrollBarNode.ScrollAxis.VERTICAL) {
            @Override
            protected void drawSlider(PoseStack stack, Rect sliderFrame) {
                RenderSystem.setShaderTexture(0, TAB_BACKGROUND);
                GuiComponent.blit(stack, sliderFrame.x(), sliderFrame.y(), 305, 58, sliderFrame.width(), sliderFrame.height(), 512, 512);
            }

            @Override
            protected void adjustContent(double scrollPercentage) {
                listNode.setScrollPercentage(scrollPercentage);
            }
        };

        listNode.addKeyValueRow(new TranslatableComponent("Die size"), () -> new TextComponent(yieldCalculator.getDieSizeString()));
        listNode.addKeyValueRow(new TranslatableComponent("Wafer size"), () -> new TextComponent(yieldCalculator.getWaferSizeString()));
        listNode.addKeyValueRow(new TranslatableComponent("Dies per wafer"), () -> new TextComponent(yieldCalculator.getDiesPerWaferString()));
        listNode.addKeyValueRow(new TranslatableComponent("Single layer yield"), () -> new TextComponent(yieldCalculator.getSingleLayerYieldString()));
        listNode.addKeyValueRow(new TranslatableComponent("Yield"), () -> new TextComponent(yieldCalculator.getYieldString()));

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
    public void drawBack(PoseStack stack, Point mouse, float partialFrame) {
        RenderSystem.setShaderTexture(0, TAB_BACKGROUND);
        GuiComponent.blit(stack, getFrame().x(), getFrame().y(), 0, 0, getFrame().width(), getFrame().height(), 512, 512);

        if (editor.isActive()) {
            getRoot().getFontRenderer().draw(stack, editor.getIcName(), getFrame().x() + 8, getFrame().y() + 6, EnumColour.GRAY.argb());
        }
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
