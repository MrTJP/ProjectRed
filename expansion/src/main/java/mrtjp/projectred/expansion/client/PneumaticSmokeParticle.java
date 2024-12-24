package mrtjp.projectred.expansion.client;

import codechicken.lib.colour.EnumColour;
import codechicken.lib.render.buffer.TransformingVertexConsumer;
import codechicken.lib.vec.*;
import com.mojang.blaze3d.platform.GlStateManager;
import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.*;
import mrtjp.projectred.core.client.particle.BaseActionParticle;
import net.minecraft.client.Camera;
import net.minecraft.client.multiplayer.ClientLevel;
import net.minecraft.client.particle.ParticleRenderType;
import net.minecraft.client.renderer.texture.TextureAtlas;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.client.renderer.texture.TextureManager;
import net.minecraft.resources.ResourceLocation;
import net.neoforged.neoforge.client.event.TextureAtlasStitchedEvent;

import java.util.LinkedList;
import java.util.List;

import static mrtjp.projectred.expansion.ProjectRedExpansion.MOD_ID;

@SuppressWarnings("NotNullFieldNotInitialized")
public class PneumaticSmokeParticle extends BaseActionParticle {

    public static ResourceLocation SMOKE_PARTICLE_LOCATION = new ResourceLocation(MOD_ID, "textures/particle/smoke.png");

    public static TextureAtlasSprite SMOKE_PARTICLE_SPRITE;

    private static ParticleRenderType PARTICLE_TYPE_SMOKE = new ParticleRenderType() {
        public void begin(BufferBuilder buffer, TextureManager textureManager) {
            RenderSystem.depthMask(true);
            RenderSystem.setShaderTexture(0, SMOKE_PARTICLE_LOCATION);
            RenderSystem.enableBlend();
            RenderSystem.disableCull();
            RenderSystem.blendFunc(GlStateManager.SourceFactor.SRC_ALPHA, GlStateManager.DestFactor.ONE_MINUS_SRC_ALPHA);
            buffer.begin(VertexFormat.Mode.QUADS, DefaultVertexFormat.PARTICLE);
        }

        public void end(Tesselator tesselator) {
            tesselator.end();
        }

        public String toString() {
            return "PARTICLE_TYPE_SMOKE";
        }
    };

    private final List<Vector3> points = new LinkedList<>();

    public PneumaticSmokeParticle(ClientLevel pLevel, List<Vector3> points) {
        super(pLevel, points.get(0).x, points.get(0).y, points.get(0).z);

        // Set points
        this.points.addAll(points);

        // Set properties
        setParticleSpeed(0, 0, 0);
        setColor(EnumColour.LIGHT_GRAY.rF(), EnumColour.LIGHT_GRAY.gF(), EnumColour.LIGHT_GRAY.bF());
        setAlpha(1);
        setLifetime(20 * 5); // 5 seconds
        gravity = 0;

        // Set bounds
        Cuboid6 bounds = new Cuboid6(x, y, z, x, y, z);
        for (Vector3 point : points) {
            bounds.enclose(point);
        }
        setBoundingBox(bounds.aabb());
    }


    @Override
    public void render(VertexConsumer pBuffer, Camera pRenderInfo, float pPartialTicks) {

        runActions(pPartialTicks);

        var t = new Translation(-pRenderInfo.getPosition().x, -pRenderInfo.getPosition().y, -pRenderInfo.getPosition().z);
        VertexConsumer vc = new TransformingVertexConsumer(pBuffer, t);

        // Draw lines from point to point
        Vector3 lastPoint = null;

        for (Vector3 point : points) {
            if (lastPoint != null) {
                drawSmokeLine(vc, lastPoint, point, pPartialTicks);
            }
            lastPoint = point;
        }
    }

    private void drawSmokeLine(VertexConsumer buffer, Vector3 p1, Vector3 p2, float f) {

        // Vector from start to end point
        Vector3 dp = p2.copy().subtract(p1);

        double width = 4/16D;
        double length = dp.mag();

        // Virtual point at which the quad is created at
        Vector3 quadPoint = new Vector3(0, length, 0);

        // Angle/axis transforms to change quadPoint to dp
        double angle = quadPoint.angle(dp);
        Vector3 axis = quadPoint.copy().crossProduct(dp).normalize();
        Transformation t = new Rotation(angle, axis);

        float slideProg = (age + f) / lifetime * 8;

        float u1 = 0.0f;
        float v1 = slideProg;
        float u2 = 1.0f;
        float v2 = v1 + (float) length;

        int j = getLightColor(f);

        // 90-degree rotated on Y axis for crossed quad render
        Transformation[] transforms = {
                new TransformationList(t, p1.translation()),
                new TransformationList(Rotation.quarterRotations[1], t, p1.translation()),
        };

        for (var tf : transforms) {
            VertexConsumer bc = new TransformingVertexConsumer(buffer, tf);
            bc.vertex(-width, 0, 0)         .uv(u1, v2).color(rCol, gCol, bCol, alpha).uv2(j).endVertex();
            bc.vertex(-width, length, 0)    .uv(u1, v1).color(rCol, gCol, bCol, alpha).uv2(j).endVertex();
            bc.vertex(width, length, 0)     .uv(u2, v1).color(rCol, gCol, bCol, alpha).uv2(j).endVertex();
            bc.vertex(width, 0, 0)          .uv(u2, v2).color(rCol, gCol, bCol, alpha).uv2(j).endVertex();
        }
    }

    @Override
    public ParticleRenderType getRenderType() {
        return PARTICLE_TYPE_SMOKE;
    }

    public static void onTextureStitchEvent(TextureAtlasStitchedEvent event) {
        if (!event.getAtlas().location().equals(TextureAtlas.LOCATION_BLOCKS)) return;
        SMOKE_PARTICLE_SPRITE = event.getAtlas().getSprite(new ResourceLocation(MOD_ID, "particle/smoke"));
    }
}
