package mrtjp.projectred.illumination;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import mrtjp.projectred.core.Configurator;
import mrtjp.projectred.core.libmc.PRColors;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.world.World;
import net.minecraftforge.client.event.RenderWorldLastEvent;
import net.minecraftforge.event.ForgeSubscribe;

import org.lwjgl.opengl.GL11;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Transformation;
import codechicken.lib.vec.Translation;
import codechicken.lib.vec.Vector3;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class RenderHalo
{
    public static RenderHalo instance = new RenderHalo();

    private static List<LightCache> renderList = new ArrayList<LightCache>();
    private static Vector3 renderEntityPos = new Vector3();
    private static Vector3 vec = new Vector3();

    private static class LightCache implements Comparable<LightCache>
    {
        final BlockCoord pos;
        final int color;
        final Cuboid6 cube;

        public LightCache(int x, int y, int z, int colorIndex, Cuboid6 cube)
        {
            this.pos = new BlockCoord(x, y, z);
            this.color = colorIndex;
            this.cube = cube;
        }

        private double renderDist() {
            return vec.set(pos.x, pos.y, pos.z).sub(renderEntityPos).magSquared();
        }

		@Override
		public int compareTo(LightCache o) {
			double ra = renderDist();
            double rb = o.renderDist();
            return ra == rb ? 0 : ra < rb ? 1 : -1;
		}
    }

    public static void addLight(int x, int y, int z, int color, Cuboid6 box)
    {
        renderList.add(new LightCache(x, y, z, color, box));
    }

    @ForgeSubscribe
    public void onRenderWorldLast(RenderWorldLastEvent event)
    {
        Tessellator tess = Tessellator.instance;
        WorldClient w = event.context.theWorld;
        EntityLivingBase entity = event.context.mc.renderViewEntity;
        renderEntityPos.set(entity.posX, entity.posY+entity.getEyeHeight(), entity.posZ);

        Collections.sort(renderList);

        GL11.glPushMatrix();
        RenderUtils.translateToWorldCoords(event.context.mc.renderViewEntity, event.partialTicks);
        prepareRenderState();

        Iterator<LightCache> it = renderList.iterator();
        int max = Configurator.lightHaloMax < 0 ? renderList.size() : Configurator.lightHaloMax;
        for(int i = 0; i < max && it.hasNext(); i++)
        {
            LightCache cc = it.next();
            renderHalo(tess, w, cc);
        }

        renderList.clear();
        restoreRenderState();
        GL11.glPopMatrix();
    }

    public static void prepareRenderState()
    {
        GL11.glEnable(GL11.GL_BLEND);
        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE);
        GL11.glDisable(GL11.GL_TEXTURE_2D);
        GL11.glDisable(GL11.GL_LIGHTING);
        GL11.glDisable(GL11.GL_CULL_FACE);
        GL11.glDepthMask(false);
        CCRenderState.reset();
        CCRenderState.startDrawing(7);
    }

    public static void restoreRenderState()
    {
        CCRenderState.draw();
        GL11.glDepthMask(true);
        GL11.glColor3f(1, 1, 1);
        GL11.glEnable(GL11.GL_CULL_FACE);
        GL11.glEnable(GL11.GL_LIGHTING);
        GL11.glEnable(GL11.GL_TEXTURE_2D);
        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA);
        GL11.glDisable(GL11.GL_BLEND);
    }

    private static void renderHalo(Tessellator tess, World world, LightCache cc)
    {
        CCRenderState.setBrightness(world, cc.pos.x, cc.pos.y, cc.pos.z);
        renderHalo(tess, cc.cube, cc.color, new Translation(cc.pos.x, cc.pos.y, cc.pos.z));
    }

    public static void renderHalo(Tessellator tess, Cuboid6 cuboid, int colour, Transformation t)
    {
        tess.setColorRGBA_I(PRColors.VALID_COLORS[colour].rgb, 128);
        RenderUtils.renderBlock(cuboid, 0, t, null, null);
    }
}
