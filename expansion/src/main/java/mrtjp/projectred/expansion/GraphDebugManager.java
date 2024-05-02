package mrtjp.projectred.expansion;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.data.MCDataInput;
import codechicken.lib.packet.PacketCustom;
import codechicken.lib.render.BlockRenderer;
import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.VertexConsumer;
import com.mojang.blaze3d.vertex.VertexFormat;
import com.mojang.brigadier.arguments.BoolArgumentType;
import mrtjp.projectred.expansion.graphs.ClientSideLinkCache;
import mrtjp.projectred.expansion.graphs.GraphContainer;
import mrtjp.projectred.expansion.part.PneumaticTubePart;
import net.minecraft.Util;
import net.minecraft.client.Minecraft;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.RenderStateShard;
import net.minecraft.client.renderer.RenderType;
import net.minecraft.core.BlockPos;
import net.minecraft.network.chat.Component;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.level.Level;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.client.event.RegisterClientCommandsEvent;
import net.minecraftforge.client.event.RenderLevelStageEvent;
import net.minecraftforge.event.TickEvent;
import net.minecraftforge.event.level.LevelEvent;

import java.util.*;
import java.util.function.Function;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;
import static net.minecraft.commands.Commands.argument;
import static net.minecraft.commands.Commands.literal;

public class GraphDebugManager {

    private static final HashMap<ResourceKey<Level>, GraphDebugManager> CLIENT_INSTANCE = new HashMap<>();

    private static boolean enableRendering = false;

    private final ResourceKey<Level> dimension;
    private final HashSet<GraphContainer> containers = new HashSet<>();

    public GraphDebugManager(ResourceKey<Level> dimension) {
        this.dimension = dimension;
    }

    public static GraphDebugManager getInstance(Level level) {
        return CLIENT_INSTANCE.computeIfAbsent(level.dimension(), GraphDebugManager::new);
    }

    private PacketCustom createPacket(int key) {
        return new PacketCustom(ExpansionNetwork.NET_CHANNEL, ExpansionNetwork.LINK_DEBUG_RENDERER_FROM_SERVER)
                .writeByte(key);
    }

    public void read(MCDataInput input, Level level) {
        int key = input.readUByte();
        switch (key) {
            // Nothing yet!
            default -> throw new RuntimeException("Unknown key: " + key);
        }
    }

    public void onWorldJoin(GraphContainer container) {
        containers.add(container);
    }

    public void onWorldSeparate(GraphContainer container) {
        containers.remove(container);
    }
    //endregion

    public static void registerClientCommands(RegisterClientCommandsEvent event) {
        event.getDispatcher().register(literal("project_red")
                .then(literal("tube_graph_debug")
                        .then(argument("enable", BoolArgumentType.bool())
                                .executes(ctx -> {
                                    enableRendering = BoolArgumentType.getBool(ctx, "enable");
                                    ctx.getSource().sendSuccess(() -> Component.literal("Tube graph debugging " + (enableRendering ? "enabled" : "disabled")), false);
                                    return 0;
                                })
                        )
                )
        );
    }

    //region Level events
    public static void onLevelUnload(LevelEvent.Unload event) {
        if (event.getLevel() instanceof Level level) {
            getInstance(level).containers.clear();
        }
    }

    public static void onLevelTick(TickEvent.LevelTickEvent event) {

    }
    //endregion

    //region Client-side rendering
    @OnlyIn(Dist.CLIENT)
    private static class GraphDebugManagerClientRenderTypes {

        @OnlyIn(Dist.CLIENT)
        private static RenderType DEBUG_CUBE_RENDER_TYPE = RenderType.create(MOD_ID + ":graph_debug_cube", DefaultVertexFormat.POSITION_COLOR, VertexFormat.Mode.QUADS, 256, false, false,
                RenderType.CompositeState.builder()
                        .setShaderState(RenderStateShard.POSITION_COLOR_SHADER)
                        .setTextureState(RenderStateShard.NO_TEXTURE)
                        .setTransparencyState(RenderStateShard.TRANSLUCENT_TRANSPARENCY)
                        .setDepthTestState(RenderStateShard.LEQUAL_DEPTH_TEST)
                        .setCullState(RenderStateShard.NO_CULL)
                        .setLightmapState(RenderStateShard.NO_LIGHTMAP)
                        .setOverlayState(RenderStateShard.NO_OVERLAY)
                        .setLayeringState(RenderStateShard.NO_LAYERING)
                        .setOutputState(RenderStateShard.PARTICLES_TARGET)
                        .setTexturingState(RenderStateShard.DEFAULT_TEXTURING)
                        .setWriteMaskState(RenderStateShard.COLOR_DEPTH_WRITE)
                        .setLineState(RenderStateShard.DEFAULT_LINE)
                        .createCompositeState(true));

        @OnlyIn(Dist.CLIENT)
        public static Function<Double, RenderType> DEBUG_LINES_RENDER_TYPE = Util.memoize(a -> RenderType.create(MOD_ID + ":graph_debug_lines", DefaultVertexFormat.POSITION_COLOR_NORMAL, VertexFormat.Mode.LINES, 256, false, false,
                RenderType.CompositeState.builder()
                        .setShaderState(RenderStateShard.RENDERTYPE_LINES_SHADER)
                        .setTextureState(RenderStateShard.NO_TEXTURE)
                        .setTransparencyState(RenderStateShard.TRANSLUCENT_TRANSPARENCY)
                        .setDepthTestState(RenderStateShard.LEQUAL_DEPTH_TEST)
                        .setCullState(RenderStateShard.NO_CULL)
                        .setLightmapState(RenderStateShard.NO_LIGHTMAP)
                        .setOverlayState(RenderStateShard.NO_OVERLAY)
                        .setLayeringState(RenderStateShard.VIEW_OFFSET_Z_LAYERING)
                        .setOutputState(RenderStateShard.PARTICLES_TARGET)
                        .setTexturingState(RenderStateShard.DEFAULT_TEXTURING)
                        .setWriteMaskState(RenderStateShard.COLOR_DEPTH_WRITE)
                        .setLineState(new RenderStateShard.LineStateShard(OptionalDouble.of(a)))
                        .createCompositeState(false)));
    }

    @OnlyIn(Dist.CLIENT)
    public static void onRenderLevelStage(RenderLevelStageEvent event) {
        // Return if debug rendering is disabled. This can be toggled via command
        if (!enableRendering) return;

        Level level = Minecraft.getInstance().level;
        if (level == null) return;

        GraphDebugManager renderer = getInstance(level);

        Map<BlockPos, ClientSideLinkCache> links = new HashMap<>();
        for (var container : renderer.containers) {
            // TODO create some kind of interface here
            if (!(container instanceof PneumaticTubePart ptp)) continue;
            if (!ptp.linkCache.isActive()) continue;
            links.put(ptp.pos(), ptp.linkCache);
        }

        if (links.isEmpty()) return;
        if (event.getStage() != RenderLevelStageEvent.Stage.AFTER_PARTICLES) return;

        // Translate pose to camera position
        Vec3 cam = event.getCamera().getPosition();
        PoseStack stack = event.getPoseStack();
        stack.pushPose();
        stack.translate(-cam.x, -cam.y, -cam.z);

        MultiBufferSource.BufferSource buffers = Minecraft.getInstance().renderBuffers().bufferSource();

        renderLinks(stack, buffers, links);
    }

    @OnlyIn(Dist.CLIENT)
    private static void renderLinks(PoseStack stack, MultiBufferSource.BufferSource buffers, Map<BlockPos, ClientSideLinkCache> links) {
        CCRenderState ccrs = CCRenderState.instance();
        ccrs.reset();

        // Render node cube outlines
        ccrs.bind(GraphDebugManagerClientRenderTypes.DEBUG_LINES_RENDER_TYPE.apply(2.0), buffers, stack);
        ccrs.baseColour = EnumColour.ORANGE.rgba();
        for (var entry : links.entrySet()) {
            RenderUtils.bufferCuboidOutline(ccrs.getConsumer(), Cuboid6.full.copy().expand(-2/16D).add(entry.getKey()), 1, 1, 1, 1);
        }

        // Render link lines
        ccrs.bind(GraphDebugManagerClientRenderTypes.DEBUG_LINES_RENDER_TYPE.apply(1.5), buffers, stack);
        ccrs.baseColour = EnumColour.LIGHT_BLUE.rgba();
        for (var entry : links.entrySet()) {
            for (var link : entry.getValue().links) {
                BlockPos segStart = entry.getKey();
                LinkedList<Vector3> segmentPoints = link.getPointListFor(segStart);
                if (segmentPoints.isEmpty()) continue;

                Iterator<Vector3> pointsIt = segmentPoints.iterator();
                Vector3 p1 = pointsIt.next();

                while (pointsIt.hasNext()) {
                    Vector3 p2 = pointsIt.next();
                    bufferLinePair(ccrs.getConsumer(),
                            p1.x, p1.y, p1.z,
                            p2.x, p2.y, p2.z,
                            1, 1, 1, 1);
                    p1 = p2;
                }

                // Link start box
                double s = 4;
                RenderUtils.bufferCuboidOutline(ccrs.getConsumer(), new Cuboid6(-s/16, -s/16, -s/16, s/16, s/16, s/16).add(segmentPoints.getFirst()), 0, 0, 1, 1);
                // Link end box
                s = 5;
                RenderUtils.bufferCuboidOutline(ccrs.getConsumer(), new Cuboid6(-s/16, -s/16, -s/16, s/16, s/16, s/16).add(segmentPoints.getLast()), 1, 0, 0, 1);
            }
        }

        // Render cubes at ends of segments
        ccrs.bind(GraphDebugManagerClientRenderTypes.DEBUG_CUBE_RENDER_TYPE, buffers, stack);
        ccrs.baseColour = EnumColour.LIGHT_BLUE.rgba(128);
        double b = 0.5/16D;
        Cuboid6 box = new Cuboid6(-b, -b, -b, b, b, b);

        for (var entry : links.entrySet()) {
            for (var link : entry.getValue().links) {
                var points = link.getPointListFor(entry.getKey());
                for (var p : points) {
                    ccrs.setPipeline(new Translation(p));
                    BlockRenderer.renderCuboid(ccrs, box, 0);
                }
            }
        }

        buffers.endBatch();
        stack.popPose();
    }

    @OnlyIn(Dist.CLIENT)
    private static void bufferLinePair(VertexConsumer builder, double x1, double y1, double z1, double x2, double y2, double z2, float r, float g, float b, float a) {
        Vector3 v1 = new Vector3(x1, y1, z1).subtract(x2, y2, z2);
        double d = v1.mag();
        v1.divide(d);
        builder.vertex(x1, y1, z1).color(r, g, b, a).normal((float) v1.x, (float) v1.y, (float) v1.z).endVertex();
        builder.vertex(x2, y2, z2).color(r, g, b, a).normal((float) v1.x, (float) v1.y, (float) v1.z).endVertex();
    }
    //endregion
}
