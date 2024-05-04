package mrtjp.projectred.fabrication.editor.tools;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.render.BlockRenderer;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.vec.Cuboid6;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.fabrication.gui.ICRenderTypes;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.network.chat.Component;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import javax.annotation.Nullable;
import java.util.List;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class SimpleInteractionZone implements InteractionZone {

    private final Supplier<Cuboid6> boundsSupplier;
    private final Runnable leftClickAction;
    private final Runnable rightClickAction;
    private final Consumer<List<Component>> tooltipBuilder;
    private final int highlightColor;
    private final int boundingBoxColor;
    private final double boundingBoxLineWidth;
    @Nullable private final Component text;

    private SimpleInteractionZone(Builder builder) {
        this.boundsSupplier = builder.boundsSupplier;
        this.leftClickAction = builder.leftClickAction;
        this.rightClickAction = builder.rightClickAction;
        this.tooltipBuilder = builder.tooltipBuilder;
        this.highlightColor = builder.highlightColor;
        this.boundingBoxColor = builder.boundingBoxColor;
        this.boundingBoxLineWidth = builder.boundingBoxLineWidth;
        this.text = builder.text;
    }

    @Override
    public Cuboid6 getBounds() {
        return boundsSupplier.get();
    }

    @Override
    public void onLeftClick() {
        leftClickAction.run();
    }

    @Override
    public void onRightClick() {
        rightClickAction.run();
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void buildToolTip(List<Component> tooltip) {
        tooltipBuilder.accept(tooltip);
    }

    @Override
    public boolean canRespondToKey(int glfwKeyCode, int glfwFlags) {
        return false;
    }

    @Override
    public boolean onKeyPressed(int glfwKeyCode, int glfwFlags) {
        return false;
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void renderZone(CCRenderState ccrs, MultiBufferSource getter, PoseStack poseStack, boolean isSelected, boolean isMouseOver) {
        Cuboid6 bounds = getBounds();

        ccrs.reset();
        ccrs.bind(ICRenderTypes.selectionRenderType, getter, poseStack);
        ccrs.baseColour = highlightColor;
        ccrs.alphaOverride = isSelected && isMouseOver ? 255 : isSelected ? 128 : isMouseOver ? 64 : 32;
        BlockRenderer.renderCuboid(ccrs, bounds, 1);

        ccrs.reset();
        ccrs.bind(ICRenderTypes.interactionZoneLinesRenderType.apply(boundingBoxLineWidth), getter, poseStack);
        ccrs.baseColour = boundingBoxColor;
        RenderUtils.bufferCuboidOutline(ccrs.getConsumer(), bounds, 1, 1, 1, 1);

        if (text != null) {
            ICRenderTypes.renderCenteredTextTopOfCuboid(text, bounds, poseStack, getter);
        }
    }

    public static class Builder {

        private Supplier<Cuboid6> boundsSupplier = () -> Cuboid6.full;
        private Runnable leftClickAction = () -> {};
        private Runnable rightClickAction = () -> {};
        private Consumer<List<Component>> tooltipBuilder = (list) -> {};
        private int highlightColor = EnumColour.LIGHT_BLUE.rgba();
        private int boundingBoxColor = EnumColour.WHITE.rgba();
        private double boundingBoxLineWidth = 4.0;
        @Nullable private Component text;

        public Builder bounds(Supplier<Cuboid6> boundsSupplier) {
            this.boundsSupplier = boundsSupplier;
            return this;
        }

        public Builder bounds(Cuboid6 bounds) {
            this.boundsSupplier = () -> bounds;
            return this;
        }

        public Builder leftClickAction(Runnable leftClickAction) {
            this.leftClickAction = leftClickAction;
            return this;
        }

        public Builder rightClickAction(Runnable rightClickAction) {
            this.rightClickAction = rightClickAction;
            return this;
        }

        public Builder tooltip(Consumer<List<Component>> tooltipBuilder) {
            this.tooltipBuilder = tooltipBuilder;
            return this;
        }

        public Builder tooltip(Component tooltip) {
            this.tooltipBuilder = (list) -> list.add(tooltip);
            return this;
        }

        public Builder tooltip(Supplier<Component> tooltipSupplier) {
            this.tooltipBuilder = (list) -> list.add(tooltipSupplier.get());
            return this;
        }

        public Builder highlightColor(int rgba) {
            this.highlightColor = rgba;
            return this;
        }

        public Builder boundingBoxColor(int rgba) {
            this.boundingBoxColor = rgba;
            return this;
        }

        public Builder boundingBoxLineWidth(double width) {
            this.boundingBoxLineWidth = width;
            return this;
        }

        public Builder text(Component text) {
            this.text = text;
            return this;
        }

        public SimpleInteractionZone build() {
            return new SimpleInteractionZone(this);
        }
    }
}
