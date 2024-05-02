package mrtjp.projectred.core.client;

import com.google.gson.JsonSyntaxException;
import com.mojang.blaze3d.pipeline.RenderTarget;
import mrtjp.projectred.core.ProjectRedCore;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.PostChain;
import net.minecraft.client.renderer.PostPass;
import net.minecraft.client.renderer.texture.TextureManager;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.packs.resources.ResourceManager;
import org.joml.Matrix4f;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

/**
 * Implementation of PostChain that allows intermediary targets to be different sizes vs the main target, and maintains
 * their scale relative to main target. This allows use up-scaling/downscaling shaders:
 * <p>
 * 1. All passes will receive an ortho matrix scaled to its input target rather than always the main target.
 * 2. Targets that need to maintain a fixed scale relative to main target can be registered via {@link FixedScalePostChain#addFixedTargetScale(String, double)}.
 */
public class FixedScalePostChain extends PostChain {

    private final Map<String, Double> fixedRenderTargetScales = new HashMap<>();

    public FixedScalePostChain(TextureManager textureManager, ResourceManager resourceManager, RenderTarget renderTarget, ResourceLocation location) throws IOException, JsonSyntaxException {
        super(textureManager, resourceManager, renderTarget, location);
    }

    public FixedScalePostChain(ResourceLocation location) throws IOException {
        this(Minecraft.getInstance().getTextureManager(),
                Minecraft.getInstance().getResourceManager(),
                Minecraft.getInstance().getMainRenderTarget(),
                location);
    }

    public FixedScalePostChain(String modId, String filename) throws IOException {
        this(new ResourceLocation(modId, "shaders/post/" + filename + ".json"));
    }

    /**
     * Attach a fixed scale to a render target. It will be applied on resize
     * @param name Name of render target defined in JSON
     * @param scale Scale relative to main target
     */
    public void addFixedTargetScale(String name, double scale) {
        fixedRenderTargetScales.put(name, scale);
    }

    /**
     * Utility to check and resize targets if screen is resized. Useful since as far as I know, there is no Forge "onScreenResized" event.
     * Call this from the render thread before drawing into inputs
     */
    public void resizeIfNeeded() {
        if (screenWidth != screenTarget.width || screenHeight != screenTarget.height) {
            resize(screenTarget.width, screenTarget.height);
        }
    }

    @Override
    public void resize(int newWidth, int newHeight) {
        ProjectRedCore.LOGGER.debug("Resizing FixedScalePostChain from {}x{} to {}x{}", screenWidth, screenHeight, newWidth, newHeight);
        super.resize(newWidth, newHeight);

        // Resize targets with fixed scales
        for (var entry : fixedRenderTargetScales.entrySet()) {
            String name = entry.getKey();
            Double scale = entry.getValue();

            RenderTarget target = customRenderTargets.get(name);

            int newTargetWidth = (int) Math.ceil(newWidth * scale);
            int newTargetHeight = (int) Math.ceil(newHeight * scale);

            // Resize the target
            ProjectRedCore.LOGGER.debug("Resizing target {} from {}x{} to {}x{}", entry.getKey(), target.width, target.height, newTargetWidth, newTargetHeight);
            target.resize(newTargetWidth, newTargetHeight, Minecraft.ON_OSX);
        }

        // Re-compute projection matrices scaled to each pass's input
        for (PostPass pass : this.passes) {
            Matrix4f orthoMatrix = new Matrix4f().setOrtho(0.0F, (float) pass.inTarget.width,  0.0F, (float) pass.inTarget.height, 0.1F, 1000.0F);
            pass.setOrthoMatrix(orthoMatrix);
        }
    }
}
