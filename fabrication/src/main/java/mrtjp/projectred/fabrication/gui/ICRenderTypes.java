package mrtjp.projectred.fabrication.gui;

import codechicken.lib.render.BlockRenderer;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.texture.AtlasRegistrar;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexFormat;
import com.mojang.math.Vector3f;
import mrtjp.projectred.fabrication.ProjectRedFabrication;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderStateShard;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.resources.ResourceLocation;

import java.util.HashMap;
import java.util.Map;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.MOD_ID;

public class ICRenderTypes {

    public static ResourceLocation PERFBOARD_TEXTURE = new ResourceLocation(MOD_ID, "textures/block/perfboard.png");
    public static ResourceLocation PERFBOARD_EDGE_TEXTURE = new ResourceLocation(MOD_ID, "textures/block/perfboard_edge.png");
    public static ResourceLocation PERFBOARD_CORNER_TEXTURE = new ResourceLocation(MOD_ID, "textures/block/perfboard_corner.png");

    public static RenderType layersRenderType = RenderType.create(MOD_ID + ":ic_block", DefaultVertexFormat.BLOCK, VertexFormat.Mode.QUADS, 10000, false, true,
            RenderType.CompositeState.builder()
                    .setShaderState(RenderStateShard.RENDERTYPE_CUTOUT_SHADER) //TODO only default one that discards alpha frags
                    .setTextureState(RenderStateShard.BLOCK_SHEET)
                    .setTransparencyState(RenderStateShard.TRANSLUCENT_TRANSPARENCY)
                    .setDepthTestState(RenderStateShard.LEQUAL_DEPTH_TEST)
                    .setCullState(RenderStateShard.CULL)
                    .setLightmapState(RenderStateShard.LIGHTMAP)
                    .setOverlayState(RenderStateShard.NO_OVERLAY)
                    .setLayeringState(RenderStateShard.NO_LAYERING)
                    .setOutputState(RenderStateShard.MAIN_TARGET)
                    .setTexturingState(RenderStateShard.DEFAULT_TEXTURING)
                    .setWriteMaskState(RenderStateShard.COLOR_DEPTH_WRITE)
                    .setLineState(RenderStateShard.DEFAULT_LINE)
                    .createCompositeState(true));

    public static RenderType selectionRenderType = RenderType.create(MOD_ID + ":ic_selection", DefaultVertexFormat.POSITION_COLOR, VertexFormat.Mode.QUADS, 256, false, false,
            RenderType.CompositeState.builder()
                    .setShaderState(RenderStateShard.POSITION_COLOR_SHADER)
                    .setTextureState(RenderStateShard.NO_TEXTURE)
                    .setTransparencyState(RenderStateShard.LIGHTNING_TRANSPARENCY)
                    .setDepthTestState(RenderStateShard.LEQUAL_DEPTH_TEST)
                    .setCullState(RenderStateShard.CULL)
                    .setLightmapState(RenderStateShard.NO_LIGHTMAP)
                    .setOverlayState(RenderStateShard.NO_OVERLAY)
                    .setLayeringState(RenderStateShard.NO_LAYERING)
                    .setOutputState(RenderStateShard.MAIN_TARGET)
                    .setTexturingState(RenderStateShard.DEFAULT_TEXTURING)
                    .setWriteMaskState(RenderStateShard.COLOR_DEPTH_WRITE)
                    .setLineState(RenderStateShard.DEFAULT_LINE)
                    .createCompositeState(true));

    public static RenderType gridRenderType = RenderType.create(MOD_ID + ":ic_grid", DefaultVertexFormat.BLOCK, VertexFormat.Mode.QUADS, 256, false, false,
            RenderType.CompositeState.builder()
                    .setShaderState(RenderStateShard.BLOCK_SHADER)
                    .setTextureState(new RenderStateShard.TextureStateShard(PERFBOARD_TEXTURE, false, false)) // Mipped: Strange artifacts on render. Our normal world rendering is not mipped
                    .setTransparencyState(RenderStateShard.TRANSLUCENT_TRANSPARENCY)
                    .setDepthTestState(RenderStateShard.LEQUAL_DEPTH_TEST)
                    .setCullState(RenderStateShard.CULL)
                    .setLightmapState(RenderStateShard.NO_LIGHTMAP)
                    .setOverlayState(RenderStateShard.NO_OVERLAY)
                    .setLayeringState(RenderStateShard.NO_LAYERING)
                    .setOutputState(RenderStateShard.MAIN_TARGET)
                    .setTexturingState(RenderStateShard.DEFAULT_TEXTURING)
                    .setWriteMaskState(RenderStateShard.COLOR_DEPTH_WRITE)
                    .setLineState(RenderStateShard.DEFAULT_LINE)
                    .createCompositeState(true));

    public static RenderType gridEdgeRenderType = RenderType.create(MOD_ID + ":ic_grid_edge", DefaultVertexFormat.BLOCK, VertexFormat.Mode.QUADS, 256, false, false,
            RenderType.CompositeState.builder()
                    .setShaderState(RenderStateShard.BLOCK_SHADER)
                    .setTextureState(new RenderStateShard.TextureStateShard(PERFBOARD_EDGE_TEXTURE, false, false))
                    .setTransparencyState(RenderStateShard.TRANSLUCENT_TRANSPARENCY)
                    .setDepthTestState(RenderStateShard.LEQUAL_DEPTH_TEST)
                    .setCullState(RenderStateShard.CULL)
                    .setLightmapState(RenderStateShard.NO_LIGHTMAP)
                    .setOverlayState(RenderStateShard.NO_OVERLAY)
                    .setLayeringState(RenderStateShard.NO_LAYERING)
                    .setOutputState(RenderStateShard.MAIN_TARGET)
                    .setTexturingState(RenderStateShard.DEFAULT_TEXTURING)
                    .setWriteMaskState(RenderStateShard.COLOR_DEPTH_WRITE)
                    .setLineState(RenderStateShard.DEFAULT_LINE)
                    .createCompositeState(true));

    public static RenderType gridCornerRenderType = RenderType.create(MOD_ID + ":ic_grid_corner", DefaultVertexFormat.BLOCK, VertexFormat.Mode.QUADS, 256, false, false,
            RenderType.CompositeState.builder()
                    .setShaderState(RenderStateShard.BLOCK_SHADER)
                    .setTextureState(new RenderStateShard.TextureStateShard(PERFBOARD_CORNER_TEXTURE, false, false))
                    .setTransparencyState(RenderStateShard.TRANSLUCENT_TRANSPARENCY)
                    .setDepthTestState(RenderStateShard.LEQUAL_DEPTH_TEST)
                    .setCullState(RenderStateShard.CULL)
                    .setLightmapState(RenderStateShard.NO_LIGHTMAP)
                    .setOverlayState(RenderStateShard.NO_OVERLAY)
                    .setLayeringState(RenderStateShard.NO_LAYERING)
                    .setOutputState(RenderStateShard.MAIN_TARGET)
                    .setTexturingState(RenderStateShard.DEFAULT_TEXTURING)
                    .setWriteMaskState(RenderStateShard.COLOR_DEPTH_WRITE)
                    .setLineState(RenderStateShard.DEFAULT_LINE)
                    .createCompositeState(true));

    public static TextureAtlasSprite icSurfaceIcon;
    public static TextureAtlasSprite icSurfaceBorderIcon;
    public static TextureAtlasSprite icSurfaceCornerIcon;

    public static void renderICGrid(PoseStack renderStack, MultiBufferSource getter, Cuboid6 bounds, CCRenderState ccrs) {
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

    public static void renderTextCenteredAt(PoseStack stack, Vector3 pos, String text, int bgColor, int textColor) {
        Font fontRenderer = Minecraft.getInstance().font;

        stack.pushPose();

        stack.translate(pos.x, pos.y, pos.z);
        stack.scale(1.0f/fontRenderer.lineHeight, 1, 1.0f/fontRenderer.lineHeight);
        stack.mulPose(Vector3f.XP.rotationDegrees(90.0F));

        fontRenderer.draw(stack, text, (float) (0 - fontRenderer.width(text) / 2), (float) (0 - fontRenderer.lineHeight / 2), textColor);

        stack.popPose();
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
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/perfboard"), i -> icSurfaceIcon = i);
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/perfboard_edge"), i -> icSurfaceBorderIcon = i);
        registrar.registerSprite(new ResourceLocation(MOD_ID, "block/perfboard_corner"), i -> icSurfaceCornerIcon = i);
    }
}
