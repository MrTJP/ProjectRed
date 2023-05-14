package mrtjp.projectred.core.client;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.render.BlockRenderer;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.client.renderer.RenderState;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.util.math.BlockPos;
import net.minecraftforge.client.event.RenderWorldLastEvent;

import java.util.LinkedList;

public class HaloRenderer {

    public static final RenderType HALO_RENDER_TYPE = RenderType.create("projectred-core:halo",
            DefaultVertexFormats.POSITION_COLOR, 7, 8192, false, true,
            RenderType.State.builder().setTransparencyState(RenderState.LIGHTNING_TRANSPARENCY)
                    .setTextureState(RenderState.NO_TEXTURE)
                    .setCullState(RenderState.CULL)
                    .setOutputState(RenderState.TRANSLUCENT_TARGET)
                    .createCompositeState(false));

    private static final LinkedList<WorldLight> worldLights = new LinkedList<>();

    //region World renderer
    public static void addLight(BlockPos pos, int colour, Cuboid6 box) {
        worldLights.add(new WorldLight(pos, colour, box));
    }

    public static void onRenderWorldLastEvent(final RenderWorldLastEvent event) {

        WorldLight light = null;
        while ((light = worldLights.poll()) != null) {
            //TODO
            System.out.print("RENDER: " + light.pos.toString() + ": " + light.box.toString());
        }
    }
    //endregion

    //region Render functions
    public static void prepareRenderState(CCRenderState ccrs, MatrixStack mStack, IRenderTypeBuffer buffers) {
        ccrs.reset();
        ccrs.bind(HALO_RENDER_TYPE, buffers, mStack);
    }

    public static void renderToCCRS(CCRenderState ccrs, Cuboid6 cuboid, int colour, Transformation t) {
        ccrs.setPipeline(t);
        ccrs.baseColour = EnumColour.values()[colour].rgba();
        ccrs.alphaOverride = 160;
        BlockRenderer.renderCuboid(ccrs, cuboid, 0);
    }

    public static void renderHalo(CCRenderState ccrs, MatrixStack mStack, IRenderTypeBuffer buffers, Cuboid6 cuboid, int colour, Vector3 pos) {
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
