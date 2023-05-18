package mrtjp.projectred.core.client;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.render.BlockRenderer;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexFormat;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderStateShard;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.core.BlockPos;
import net.minecraftforge.client.event.RenderLevelStageEvent;

import java.util.LinkedList;

public class HaloRenderer {

    public static final RenderType HALO_RENDER_TYPE = RenderType.create("projectred-core:halo",
            DefaultVertexFormat.POSITION_COLOR, VertexFormat.Mode.QUADS, 8192, false, true,
            RenderType.CompositeState.builder().setTransparencyState(RenderStateShard.LIGHTNING_TRANSPARENCY)
                    .setShaderState(RenderType.POSITION_COLOR_SHADER)
                    .setTextureState(RenderStateShard.NO_TEXTURE)
                    .setCullState(RenderStateShard.CULL)
                    .setOutputState(RenderStateShard.TRANSLUCENT_TARGET)
                    .createCompositeState(false));

    private static final LinkedList<WorldLight> worldLights = new LinkedList<>();

    //region World renderer
    public static void addLight(BlockPos pos, int colour, Cuboid6 box) {
        worldLights.add(new WorldLight(pos, colour, box));
    }

    public static void onRenderWorldLastEvent(final RenderLevelStageEvent event) {

        WorldLight light = null;
        while ((light = worldLights.poll()) != null) {
            //TODO
            System.out.print("RENDER: " + light.pos.toString() + ": " + light.box.toString());
        }
    }
    //endregion

    //region Render functions
    public static void prepareRenderState(CCRenderState ccrs, PoseStack mStack, MultiBufferSource buffers) {
        ccrs.reset();
        ccrs.bind(HALO_RENDER_TYPE, buffers, mStack);
    }

    public static void renderToCCRS(CCRenderState ccrs, Cuboid6 cuboid, int colour, Transformation t) {
        ccrs.setPipeline(t);
        ccrs.baseColour = EnumColour.values()[colour].rgba();
        ccrs.alphaOverride = 160;
        BlockRenderer.renderCuboid(ccrs, cuboid, 0);
    }

    public static void renderHalo(CCRenderState ccrs, PoseStack mStack, MultiBufferSource buffers, Cuboid6 cuboid, int colour, Vector3 pos) {
        prepareRenderState(ccrs, mStack, buffers);
        renderToCCRS(ccrs, cuboid, colour, pos.translation());
    }
    //endregion

    private static class WorldLight {
        public BlockPos pos;
        public int colour;
        public Cuboid6 box;

        public WorldLight(BlockPos pos, int colour, Cuboid6 box) {
            this.pos = pos;
            this.colour = colour;
            this.box = box;
        }
    }
}
