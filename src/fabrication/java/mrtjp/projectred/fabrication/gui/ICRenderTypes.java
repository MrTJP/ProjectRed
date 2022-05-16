package mrtjp.projectred.fabrication.gui;

import codechicken.lib.render.BlockRenderer;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.texture.AtlasRegistrar;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.matrix.MatrixStack;
import mrtjp.projectred.ProjectRedFabrication;
import net.minecraft.client.renderer.IRenderTypeBuffer;
import net.minecraft.client.renderer.RenderState;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.renderer.vertex.DefaultVertexFormats;
import net.minecraft.util.ResourceLocation;
import org.lwjgl.opengl.GL11;

public class ICRenderTypes {

    public static RenderType layersRenderType = RenderType.create("ic_workbench_layers", DefaultVertexFormats.BLOCK, GL11.GL_QUADS, 262144, false, true,
            RenderType.State.builder()
                    .setTextureState(RenderState.BLOCK_SHEET) // Mipped: Strange artifacts on render. Our normal world rendering is not mipped
                    .setTransparencyState(RenderState.TRANSLUCENT_TRANSPARENCY)
                    .setDiffuseLightingState(RenderState.DIFFUSE_LIGHTING) // No diffuse brightens up everything but looks flat
                    .setShadeModelState(RenderState.SMOOTH_SHADE) //no noticeable difference
                    .setAlphaState(RenderState.DEFAULT_ALPHA) // Alpha must be very tiny to get clipped out
                    .setDepthTestState(RenderState.LEQUAL_DEPTH_TEST)
                    .setCullState(RenderState.CULL)
                    .setLightmapState(RenderState.NO_LIGHTMAP) // No difference
                    .setOverlayState(RenderState.NO_OVERLAY)
                    .setFogState(RenderState.FOG)
                    .setLayeringState(RenderState.NO_LAYERING)
                    .setOutputState(RenderState.MAIN_TARGET)
                    .setTexturingState(RenderState.DEFAULT_TEXTURING)
                    .setWriteMaskState(RenderState.COLOR_DEPTH_WRITE)
                    .setLineState(RenderState.DEFAULT_LINE)
                    .createCompositeState(true));

    public static RenderType selectionRenderType = RenderType.create("ic_workbench_selection", DefaultVertexFormats.BLOCK, GL11.GL_QUADS, 256, false, false,
            RenderType.State.builder()
                    .setTextureState(RenderState.NO_TEXTURE) // Mipped: Strange artifacts on render. Our normal world rendering is not mipped
                    .setTransparencyState(RenderState.LIGHTNING_TRANSPARENCY)
                    .setDiffuseLightingState(RenderState.NO_DIFFUSE_LIGHTING) // No diffuse brightens up everything but looks flat
                    .setShadeModelState(RenderState.FLAT_SHADE) //no noticeable difference
                    .setAlphaState(RenderState.DEFAULT_ALPHA) // Alpha must be very tiny to get clipped out
                    .setDepthTestState(RenderState.LEQUAL_DEPTH_TEST)
                    .setCullState(RenderState.CULL)
                    .setLightmapState(RenderState.NO_LIGHTMAP) // No difference
                    .setOverlayState(RenderState.NO_OVERLAY)
                    .setFogState(RenderState.FOG)
                    .setLayeringState(RenderState.NO_LAYERING)
                    .setOutputState(RenderState.MAIN_TARGET)
                    .setTexturingState(RenderState.DEFAULT_TEXTURING)
                    .setWriteMaskState(RenderState.COLOR_DEPTH_WRITE)
                    .setLineState(RenderState.DEFAULT_LINE)
                    .createCompositeState(true));

    public static RenderType gridRenderType = RenderType.create("ic_workbench_grid", DefaultVertexFormats.BLOCK, GL11.GL_QUADS, 256, false, false,
            RenderType.State.builder()
                    .setTextureState(new RenderState.TextureState(new ResourceLocation(ProjectRedFabrication.MOD_ID, "textures/block/prefboard.png"), false, false)) // Mipped: Strange artifacts on render. Our normal world rendering is not mipped
                    .setTransparencyState(RenderState.TRANSLUCENT_TRANSPARENCY)
                    .setDiffuseLightingState(RenderState.DIFFUSE_LIGHTING) // No diffuse brightens up everything but looks flat
                    .setShadeModelState(RenderState.SMOOTH_SHADE) //no noticeable difference
                    .setAlphaState(RenderState.DEFAULT_ALPHA) // Alpha must be very tiny to get clipped out
                    .setDepthTestState(RenderState.LEQUAL_DEPTH_TEST)
                    .setCullState(RenderState.CULL)
                    .setLightmapState(RenderState.NO_LIGHTMAP) // No difference
                    .setOverlayState(RenderState.NO_OVERLAY)
                    .setFogState(RenderState.FOG)
                    .setLayeringState(RenderState.NO_LAYERING)
                    .setOutputState(RenderState.MAIN_TARGET)
                    .setTexturingState(RenderState.DEFAULT_TEXTURING)
                    .setWriteMaskState(RenderState.COLOR_DEPTH_WRITE)
                    .setLineState(RenderState.DEFAULT_LINE)
                    .createCompositeState(true));

    public static RenderType gridEdgeRenderType = RenderType.create("ic_workbench_grid_edge", DefaultVertexFormats.BLOCK, GL11.GL_QUADS, 256, false, false,
            RenderType.State.builder()
                    .setTextureState(new RenderState.TextureState(new ResourceLocation(ProjectRedFabrication.MOD_ID, "textures/block/prefboard_edge.png"), false, false))
                    .setTransparencyState(RenderState.TRANSLUCENT_TRANSPARENCY)
                    .setDiffuseLightingState(RenderState.DIFFUSE_LIGHTING) // No diffuse brightens up everything but looks flat
                    .setShadeModelState(RenderState.SMOOTH_SHADE) //no noticeable difference
                    .setAlphaState(RenderState.DEFAULT_ALPHA) // Alpha must be very tiny to get clipped out
                    .setDepthTestState(RenderState.LEQUAL_DEPTH_TEST)
                    .setCullState(RenderState.CULL)
                    .setLightmapState(RenderState.NO_LIGHTMAP) // No difference
                    .setOverlayState(RenderState.NO_OVERLAY)
                    .setFogState(RenderState.FOG)
                    .setLayeringState(RenderState.NO_LAYERING)
                    .setOutputState(RenderState.MAIN_TARGET)
                    .setTexturingState(RenderState.DEFAULT_TEXTURING)
                    .setWriteMaskState(RenderState.COLOR_DEPTH_WRITE)
                    .setLineState(RenderState.DEFAULT_LINE)
                    .createCompositeState(true));

    public static RenderType gridCornerRenderType = RenderType.create("ic_workbench_grid_corner", DefaultVertexFormats.BLOCK, GL11.GL_QUADS, 256, false, false,
            RenderType.State.builder()
                    .setTextureState(new RenderState.TextureState(new ResourceLocation(ProjectRedFabrication.MOD_ID, "textures/block/prefboard_corner.png"), false, false))
                    .setTransparencyState(RenderState.TRANSLUCENT_TRANSPARENCY)
                    .setDiffuseLightingState(RenderState.DIFFUSE_LIGHTING) // No diffuse brightens up everything but looks flat
                    .setShadeModelState(RenderState.SMOOTH_SHADE) //no noticeable difference
                    .setAlphaState(RenderState.DEFAULT_ALPHA) // Alpha must be very tiny to get clipped out
                    .setDepthTestState(RenderState.LEQUAL_DEPTH_TEST)
                    .setCullState(RenderState.CULL)
                    .setLightmapState(RenderState.NO_LIGHTMAP) // No difference
                    .setOverlayState(RenderState.NO_OVERLAY)
                    .setFogState(RenderState.FOG)
                    .setLayeringState(RenderState.NO_LAYERING)
                    .setOutputState(RenderState.MAIN_TARGET)
                    .setTexturingState(RenderState.DEFAULT_TEXTURING)
                    .setWriteMaskState(RenderState.COLOR_DEPTH_WRITE)
                    .setLineState(RenderState.DEFAULT_LINE)
                    .createCompositeState(true));

    public static TextureAtlasSprite icSurfaceIcon;
    public static TextureAtlasSprite icSurfaceBorderIcon;
    public static TextureAtlasSprite icSurfaceCornerIcon;

    public static void renderICGrid(MatrixStack renderStack, IRenderTypeBuffer getter, Cuboid6 bounds, CCRenderState ccrs) {
        // Main
        double ymin = -2/16D;
        Cuboid6 box = new Cuboid6(bounds.min.x, ymin, bounds.min.z, bounds.max.x, 0, bounds.max.z);
        ccrs.bind(ICRenderTypes.gridRenderType, getter, renderStack);
        BlockRenderer.renderCuboid(ccrs, box, 1);

        // Edges
        double h = 0.01D;
        ccrs.bind(ICRenderTypes.gridEdgeRenderType, getter, renderStack);
        // Top
        box.set(bounds.min.x,     ymin, bounds.min.z,     bounds.max.x,     h, bounds.min.z + 1);
        BlockRenderer.renderCuboid(ccrs, box, 1);
        // bottom
        box.set(bounds.min.x,     ymin, bounds.max.z - 1, bounds.max.x,     h, bounds.max.z);
        BlockRenderer.renderCuboid(ccrs, box, 1);
        // left
        box.set(bounds.min.x,     ymin, bounds.min.z,     bounds.min.x + 1, h, bounds.max.z);
        BlockRenderer.renderCuboid(ccrs, box, 1);
        // right
        box.set(bounds.max.x - 1, ymin, bounds.min.z,     bounds.max.x,     h, bounds.max.z);
        BlockRenderer.renderCuboid(ccrs, box, 1);

        // Corners
        h = 0.02D;
        ccrs.bind(ICRenderTypes.gridCornerRenderType, getter, renderStack);
        // Top left
        box.set(bounds.min.x, ymin, bounds.min.z, bounds.min.x + 1, h, bounds.min.z + 1);
        BlockRenderer.renderCuboid(ccrs, box, 1);
        // Top right
        box.set(bounds.max.x - 1, ymin, bounds.min.z, bounds.max.x, h, bounds.min.z + 1);
        BlockRenderer.renderCuboid(ccrs, box, 1);
        // bottom right
        box.set(bounds.max.x - 1, ymin, bounds.max.z - 1, bounds.max.x, h, bounds.max.z);
        BlockRenderer.renderCuboid(ccrs, box, 1);
        // bottom left
        box.set(bounds.min.x, ymin, bounds.max.z - 1, bounds.min.x + 1, h, bounds.max.z);
        BlockRenderer.renderCuboid(ccrs, box, 1);

    }

    public static void renderSelection(CCRenderState ccrs, Vector3 a, Vector3 b, double height, double th) {

        Cuboid6 ac = new Cuboid6(a.copy().floor(), a.copy().ceil());
        ac.enclose(b.copy().floor());
        ac.enclose(b.copy().ceil());
        ac.expand(0.002);

        Cuboid6 box = new Cuboid6();

        //Top
        box.min.x = ac.min.x;
        box.min.y = ac.min.y;
        box.min.z = ac.min.z;
        box.max.x = ac.max.x;
        box.max.y = ac.min.y + height;
        box.max.z = ac.min.z + th;
        BlockRenderer.renderCuboid(ccrs, box, 1);

        // Bottom
        box.min.x = ac.min.x;
        box.min.y = ac.min.y;
        box.min.z = ac.max.z - th;
        box.max.x = ac.max.x;
        box.max.y = ac.min.y + height;
        box.max.z = ac.max.z;
        BlockRenderer.renderCuboid(ccrs, box, 1);

        // Left
        box.min.x = ac.min.x;
        box.min.y = ac.min.y;
        box.min.z = ac.min.z + th;
        box.max.x = ac.min.x + th;
        box.max.y = ac.min.y + height;
        box.max.z = ac.max.z - th;
        BlockRenderer.renderCuboid(ccrs, box, 1 | 1<<2 | 1<<3);

        // Right
        box.min.x = ac.max.x - th;
        box.min.y = ac.min.y;
        box.min.z = ac.min.z + th;
        box.max.x = ac.max.x;
        box.max.y = ac.min.y + height;
        box.max.z = ac.max.z - th;
        BlockRenderer.renderCuboid(ccrs, box, 1 | 1<<2 | 1<<3);
    }

    public void sortComponents(Cuboid6 c) {

        if (c.min.x > c.max.x) {
            double tmp = c.max.x;
            c.max.x = c.min.x;
            c.min.x = tmp;
        }

        if (c.min.y > c.max.y) {
            double tmp = c.max.y;
            c.max.y = c.min.y;
            c.min.y = tmp;
        }

        if (c.min.z > c.max.z) {
            double tmp = c.max.z;
            c.max.z = c.min.z;
            c.min.z = tmp;
        }
    }

    public static void registerIcons(AtlasRegistrar registrar) {
        registrar.registerSprite(new ResourceLocation(ProjectRedFabrication.MOD_ID, "block/prefboard"), i -> icSurfaceIcon = i);
        registrar.registerSprite(new ResourceLocation(ProjectRedFabrication.MOD_ID, "block/prefboard_edge"), i -> icSurfaceBorderIcon = i);
        registrar.registerSprite(new ResourceLocation(ProjectRedFabrication.MOD_ID, "block/prefboard_corner"), i -> icSurfaceCornerIcon = i);
    }
}
