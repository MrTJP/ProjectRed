package mrtjp.projectred.fabrication.gui;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.render.BlockRenderer;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.mojang.blaze3d.vertex.VertexFormat;
import com.mojang.math.Axis;
import net.minecraft.Util;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderStateShard;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.client.renderer.texture.TextureAtlas;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceLocation;
import net.neoforged.neoforge.client.event.TextureAtlasStitchedEvent;
import org.joml.Matrix4f;

import java.util.OptionalDouble;
import java.util.function.Function;

import static mrtjp.projectred.fabrication.ProjectRedFabrication.MOD_ID;

@SuppressWarnings("NotNullFieldNotInitialized")
public class ICRenderTypes {

    public static ResourceLocation PERFBOARD_TEXTURE = ResourceLocation.fromNamespaceAndPath(MOD_ID, "textures/block/workbench_ui/perfboard.png");
    public static ResourceLocation PERFBOARD_EDGE_TEXTURE = ResourceLocation.fromNamespaceAndPath(MOD_ID, "textures/block/workbench_ui/perfboard_edge.png");
    public static ResourceLocation PERFBOARD_CORNER_TEXTURE = ResourceLocation.fromNamespaceAndPath(MOD_ID, "textures/block/workbench_ui/perfboard_corner.png");

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
                    .setShaderState(RenderStateShard.RENDERTYPE_CUTOUT_SHADER)
                    .setTextureState(new RenderStateShard.TextureStateShard(PERFBOARD_TEXTURE, false, false)) // Mipped: Strange artifacts on render. Our normal world rendering is not mipped
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

    public static RenderType gridEdgeRenderType = RenderType.create(MOD_ID + ":ic_grid_edge", DefaultVertexFormat.BLOCK, VertexFormat.Mode.QUADS, 256, false, false,
            RenderType.CompositeState.builder()
                    .setShaderState(RenderStateShard.RENDERTYPE_CUTOUT_SHADER)
                    .setTextureState(new RenderStateShard.TextureStateShard(PERFBOARD_EDGE_TEXTURE, false, false))
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

    public static RenderType gridCornerRenderType = RenderType.create(MOD_ID + ":ic_grid_corner", DefaultVertexFormat.BLOCK, VertexFormat.Mode.QUADS, 256, false, false,
            RenderType.CompositeState.builder()
                    .setShaderState(RenderStateShard.RENDERTYPE_CUTOUT_SHADER)
                    .setTextureState(new RenderStateShard.TextureStateShard(PERFBOARD_CORNER_TEXTURE, false, false))
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

    public static RenderType holoGridRenderType = RenderType.create(MOD_ID + ":holo_grid_lines", DefaultVertexFormat.POSITION_COLOR_NORMAL, VertexFormat.Mode.LINES, 256, false, false,
            RenderType.CompositeState.builder()
                    .setShaderState(RenderStateShard.RENDERTYPE_LINES_SHADER)
                    .setTextureState(RenderStateShard.NO_TEXTURE)
                    .setTransparencyState(RenderStateShard.TRANSLUCENT_TRANSPARENCY)
                    .setDepthTestState(RenderStateShard.LEQUAL_DEPTH_TEST)
                    .setCullState(RenderStateShard.NO_CULL)
                    .setLightmapState(RenderStateShard.NO_LIGHTMAP)
                    .setOverlayState(RenderStateShard.NO_OVERLAY)
                    .setLayeringState(RenderStateShard.VIEW_OFFSET_Z_LAYERING)
                    .setOutputState(RenderStateShard.MAIN_TARGET)
                    .setTexturingState(RenderStateShard.DEFAULT_TEXTURING)
                    .setWriteMaskState(RenderStateShard.COLOR_DEPTH_WRITE)
                    .setLineState(new RenderStateShard.LineStateShard(OptionalDouble.empty()))
                    .createCompositeState(false));

    public static Function<Double, RenderType> interactionZoneLinesRenderType = Util.memoize(a -> RenderType.create(MOD_ID + ":interact_zone_lines", DefaultVertexFormat.POSITION_COLOR_NORMAL, VertexFormat.Mode.LINES, 256, false, false,
            RenderType.CompositeState.builder()
                    .setShaderState(RenderStateShard.RENDERTYPE_LINES_SHADER)
                    .setTextureState(RenderStateShard.NO_TEXTURE)
                    .setTransparencyState(RenderStateShard.TRANSLUCENT_TRANSPARENCY)
                    .setDepthTestState(RenderStateShard.LEQUAL_DEPTH_TEST)
                    .setCullState(RenderStateShard.NO_CULL)
                    .setLightmapState(RenderStateShard.NO_LIGHTMAP)
                    .setOverlayState(RenderStateShard.NO_OVERLAY)
                    .setLayeringState(RenderStateShard.VIEW_OFFSET_Z_LAYERING)
                    .setOutputState(RenderStateShard.MAIN_TARGET)
                    .setTexturingState(RenderStateShard.DEFAULT_TEXTURING)
                    .setWriteMaskState(RenderStateShard.COLOR_DEPTH_WRITE)
                    .setLineState(new RenderStateShard.LineStateShard(OptionalDouble.of(a)))
                    .createCompositeState(false)));


    public static TextureAtlasSprite icSurfaceIcon;
    public static TextureAtlasSprite icSurfaceBorderIcon;
    public static TextureAtlasSprite icSurfaceCornerIcon;
    public static TextureAtlasSprite rotateIcon;
    public static TextureAtlasSprite reflectIcon;

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

    public static void renderGridInBounds(PoseStack stack, MultiBufferSource getter, Cuboid6 bounds, int amask) {

        VertexConsumer vertexConsumer = getter.getBuffer(holoGridRenderType);
        Matrix4f pose = stack.last().pose();

        float r = 0.7f;
        float g = 0.7f;
        float b = 0.7f;
        float alpha = 0.2f;

        // Render X axis lines
        if ((amask & 1) != 0) {
            for (int y = (int) bounds.min.y; y <= bounds.max.y; y++) {
                for (int z = (int) bounds.min.z; z <= bounds.max.z; z++) {
                    vertexConsumer.addVertex(pose, (float) bounds.min.x, y, z).setColor(r, g, b, alpha).setNormal(1, 0, 0);
                    vertexConsumer.addVertex(pose, (float) bounds.max.x, y, z).setColor(r, g, b, alpha).setNormal(1, 0, 0);
                }
            }
        }

        // Render Y axis lines
        if ((amask & 2) != 0) {
            for (int x = (int) bounds.min.x; x <= bounds.max.x; x++) {
                for (int z = (int) bounds.min.z; z <= bounds.max.z; z++) {
                    vertexConsumer.addVertex(pose, x, (float) bounds.min.y, z).setColor(r, g, b, alpha).setNormal(0, 1, 0);
                    vertexConsumer.addVertex(pose, x, (float) bounds.max.y, z).setColor(r, g, b, alpha).setNormal(0, 1, 0);
                }
            }
        }

        // Render Z axis lines
        if ((amask & 4) != 0) {
            for (int x = (int) bounds.min.x; x <= bounds.max.x; x++) {
                for (int y = (int) bounds.min.y; y <= bounds.max.y; y++) {
                    vertexConsumer.addVertex(pose, x, y, (float) bounds.min.z).setColor(r, g, b, alpha).setNormal(0, 0, 1);
                    vertexConsumer.addVertex(pose, x, y, (float) bounds.max.z).setColor(r, g, b, alpha).setNormal(0, 0, 1);
                }
            }
        }
    }

    public static void renderCenteredTextTopOfCuboid(Component text, Cuboid6 bounds, PoseStack stack, MultiBufferSource getter) {
        double w = bounds.max.x - bounds.min.x;
        double h = bounds.max.z - bounds.min.z;
        double x = bounds.min.x + w / 2;
        double y = bounds.max.y + 0.002;
        double z = bounds.min.z + h / 2;

        renderConstrainedCenteredText(text, stack, getter, x, y, z, w, h);
    }

    public static void renderConstrainedText(Component text, PoseStack stack, MultiBufferSource getter, double x, double y, double z, double maxW, double maxH) {
        Font fontRenderer = Minecraft.getInstance().font;
        float h = fontRenderer.lineHeight;
        float w = fontRenderer.width(text);
        float s = (float) Math.min(maxW/w, maxH/h);

        stack.pushPose();
        stack.translate(x, y, z);
        stack.scale(s, 1.0F, s);
        stack.mulPose(Axis.XP.rotationDegrees(90.0F));

        // Render text
        MultiBufferSource.BufferSource buffer = Minecraft.getInstance().renderBuffers().bufferSource();
        fontRenderer.drawInBatch(text, 0, 0, EnumColour.WHITE.rgba(), false, stack.last().pose(), buffer, Font.DisplayMode.NORMAL, 0, 15728880);
        buffer.endBatch();

        stack.popPose();
    }

    public static void renderConstrainedCenteredText(Component text, PoseStack stack, MultiBufferSource getter, double x, double y, double z, double maxW, double maxH) {
        Font fontRenderer = Minecraft.getInstance().font;
        float h = fontRenderer.lineHeight;
        float w = fontRenderer.width(text);
        float s = (float) Math.min(maxW/w, maxH/h);

        stack.pushPose();
        stack.translate(x, y, z);
        stack.scale(s, 1.0F, s);
        stack.mulPose(Axis.XP.rotationDegrees(90.0F));

        // Render text ceneterd at desired position
        MultiBufferSource.BufferSource buffer = Minecraft.getInstance().renderBuffers().bufferSource();
        fontRenderer.drawInBatch(text, (float) (-fontRenderer.width(text) / 2), (float) (-fontRenderer.lineHeight / 2), EnumColour.WHITE.rgba(), false, stack.last().pose(), buffer, Font.DisplayMode.NORMAL, 0, 15728880);
        buffer.endBatch();

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

    public static void onTextureStitchEvent(TextureAtlasStitchedEvent event) {
        if (!event.getAtlas().location().equals(TextureAtlas.LOCATION_BLOCKS)) return;

        icSurfaceIcon = event.getAtlas().getSprite(ResourceLocation.fromNamespaceAndPath(MOD_ID, "block/workbench_ui/perfboard"));
        icSurfaceBorderIcon = event.getAtlas().getSprite(ResourceLocation.fromNamespaceAndPath(MOD_ID, "block/workbench_ui/perfboard_edge"));
        icSurfaceCornerIcon = event.getAtlas().getSprite(ResourceLocation.fromNamespaceAndPath(MOD_ID, "block/workbench_ui/perfboard_corner"));
        rotateIcon = event.getAtlas().getSprite(ResourceLocation.fromNamespaceAndPath(MOD_ID, "block/workbench_ui/rotate"));
        reflectIcon = event.getAtlas().getSprite(ResourceLocation.fromNamespaceAndPath(MOD_ID, "block/workbench_ui/reflect"));
    }
}
