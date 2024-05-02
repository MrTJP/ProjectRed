package mrtjp.projectred.lib;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.BufferBuilder;
import com.mojang.blaze3d.vertex.BufferUploader;
import com.mojang.blaze3d.vertex.DefaultVertexFormat;
import com.mojang.blaze3d.vertex.Tesselator;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.resources.ResourceLocation;
import org.joml.Matrix4f;

import static com.mojang.blaze3d.vertex.VertexFormat.Mode.QUADS;

public class GuiLib {

    public static final ResourceLocation WIDGETS_TEXTURE = new ResourceLocation("minecraft", "textures/gui/widgets.png");

    public static void drawVerticalTank(GuiGraphics graphics, ResourceLocation loc, int x, int y, int u, int v, int w, int h, int prog) {
        graphics.blit(loc, x, y + h - prog, u, v + h - prog, w, prog);
    }

    public static void drawLine(GuiGraphics graphics, int x1, int y1, int x2, int y2, int th, int argb) {

        // Two points to rect
        Vec2 p1 = new Vec2(x1, y1);
        Vec2 p2 = new Vec2(x2, y2);
        Vec2 v = p2.subtract(p1);
        Vec2 p = new Vec2(v.dy, -v.dx);
        Vec2 pnorm = p.normalize();
        Vec2 widthVec = pnorm.multiply(th / 2.0);

        Vec2 r1 = p1.subtract(widthVec);
        Vec2 r2 = p2.subtract(widthVec);
        Vec2 r3 = p2.add(widthVec);
        Vec2 r4 = p1.add(widthVec);

        Matrix4f p_238460_0_ = graphics.pose().last().pose();

        float f3 = (argb >> 24 & 255) / 255.0F;
        float f = (argb >> 16 & 255) / 255.0F;
        float f1 = (argb >> 8 & 255) / 255.0F;
        float f2 = (argb & 255) / 255.0F;
        BufferBuilder bufferbuilder = Tesselator.getInstance().getBuilder();
        RenderSystem.enableBlend();
        RenderSystem.defaultBlendFunc();
        RenderSystem.setShader(GameRenderer::getPositionColorShader);
        bufferbuilder.begin(QUADS, DefaultVertexFormat.POSITION_COLOR);
        bufferbuilder.vertex(p_238460_0_, (float) r1.dx, (float) r1.dy, 0.0F).color(f, f1, f2, f3).endVertex();
        bufferbuilder.vertex(p_238460_0_, (float) r2.dx, (float) r2.dy, 0.0F).color(f, f1, f2, f3).endVertex();
        bufferbuilder.vertex(p_238460_0_, (float) r3.dx, (float) r3.dy, 0.0F).color(f, f1, f2, f3).endVertex();
        bufferbuilder.vertex(p_238460_0_, (float) r4.dx, (float) r4.dy, 0.0F).color(f, f1, f2, f3).endVertex();
        BufferUploader.draw(bufferbuilder.end());
        RenderSystem.disableBlend();
    }
}
