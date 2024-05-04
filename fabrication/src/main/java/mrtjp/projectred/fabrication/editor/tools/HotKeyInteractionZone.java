package mrtjp.projectred.fabrication.editor.tools;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.render.BlockRenderer;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Scale;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.uv.IconTransformation;
import com.mojang.blaze3d.vertex.PoseStack;
import mrtjp.projectred.fabrication.editor.ICWorkbenchEditor;
import mrtjp.projectred.fabrication.gui.ICRenderTypes;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.network.chat.Component;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import org.lwjgl.glfw.GLFW;

import java.util.List;
import java.util.Objects;
import java.util.function.Supplier;

public class HotKeyInteractionZone implements InteractionZone {

    private final Cuboid6 bounds;
    private final int keyCode;
    private final Runnable keyAction;
    private final int backgroundColor;
    private final Component text;
    private final Supplier<Object> icon; //Supplier<TextureAtlasSprite> //TODO Find better way to make this server-safe

    private final Transformation blockTransform;

    private HotKeyInteractionZone(Builder builder) {
        this.bounds = builder.bounds;
        this.keyCode = builder.keyCode;
        this.keyAction = builder.keyAction;
        this.backgroundColor = builder.backgroundColor;
        this.text = builder.text;
        this.icon = builder.icon;

        // Transform that takes a full cuboid block and moves it to bounds
        this.blockTransform = new Scale(bounds.max.copy().subtract(bounds.min)).with(new Translation(bounds.min));
    }

    @Override
    public Cuboid6 getBounds() {
        return bounds;
    }

    @Override
    public void onLeftClick() {
        keyAction.run();
    }

    @Override
    public void onRightClick() {
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void renderZone(CCRenderState ccrs, MultiBufferSource getter, PoseStack poseStack, boolean isSelected, boolean isMouseOver) {

        // Transparent box
        ccrs.reset();
        ccrs.bind(ICRenderTypes.selectionRenderType, getter, poseStack);
        ccrs.baseColour = backgroundColor;
        ccrs.alphaOverride = isSelected && isMouseOver ? 255 : isSelected ? 128 : isMouseOver ? 64 : 32;
        BlockRenderer.renderCuboid(ccrs, getBounds(), 1);

        // Bounds outline
        ccrs.reset();
        ccrs.bind(ICRenderTypes.interactionZoneLinesRenderType.apply(2.0), getter, poseStack);
        ccrs.baseColour = EnumColour.WHITE.rgba();
        RenderUtils.bufferCuboidOutline(ccrs.getConsumer(), getBounds(), 1, 1, 1, 1);

        // Icon
        ccrs.reset();
        ccrs.bind(ICRenderTypes.layersRenderType, getter, poseStack);
        ccrs.setPipeline(new IconTransformation((TextureAtlasSprite) icon.get()), blockTransform);
        BlockRenderer.renderFullBlock(ccrs, ~0x2); //Top face only
    }

    @Override
    @OnlyIn(Dist.CLIENT)
    public void buildToolTip(List<Component> tooltip) {
        String keyName = Objects.requireNonNullElse(GLFW.glfwGetKeyName(keyCode, 0), "???");
        tooltip.add(Component.literal("[" + keyName + "] ").append(text).withStyle(ICWorkbenchEditor.UNIFORM_GRAY));
    }

    @Override
    public boolean canRespondToKey(int glfwKeyCode, int glfwFlags) {
        return glfwKeyCode == keyCode;
    }

    @Override
    public boolean onKeyPressed(int glfwKeyCode, int glfwFlags) {
        if (glfwKeyCode == keyCode) {
            keyAction.run();
            return true;
        }
        return false;
    }

    public static class Builder {

        private Cuboid6 bounds = Cuboid6.full;
        private int keyCode = GLFW.GLFW_KEY_UNKNOWN;
        private Runnable keyAction = () -> {};
        private int backgroundColor = EnumColour.GRAY.rgba(64);
        private Component text = Component.nullToEmpty("//TODO label");
        private Supplier<Object> icon = () -> null;

        public Builder bounds(Cuboid6 bounds) {
            this.bounds = bounds;
            return this;
        }

        public Builder keyCode(int keyCode) {
            this.keyCode = keyCode;
            return this;
        }

        public Builder keyAction(Runnable keyAction) {
            this.keyAction = keyAction;
            return this;
        }

        public Builder backgroundColor(int backgroundColor) {
            this.backgroundColor = backgroundColor;
            return this;
        }

        public Builder text(Component text) {
            this.text = text;
            return this;
        }

        // Supplier<TextureAtlastSprite>
        public Builder icon(Supplier<Object> icon) {
            this.icon = icon;
            return this;
        }

        public InteractionZone build() {
            return new HotKeyInteractionZone(this);
        }
    }
}
