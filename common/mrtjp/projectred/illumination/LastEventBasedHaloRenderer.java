package mrtjp.projectred.illumination;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import mrtjp.projectred.core.BasicRenderUtils;
import mrtjp.projectred.core.BasicUtils;
import mrtjp.projectred.core.PRColors;
import net.minecraft.client.Minecraft;
import net.minecraft.client.multiplayer.WorldClient;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.tileentity.TileEntity;
import net.minecraft.world.World;
import net.minecraftforge.client.event.RenderWorldLastEvent;
import net.minecraftforge.event.ForgeSubscribe;

import org.lwjgl.opengl.GL11;

import codechicken.lib.render.CCRenderState;
import codechicken.lib.render.RenderUtils;
import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Translation;
import codechicken.multipart.TMultiPart;
import codechicken.multipart.TileMultipart;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class LastEventBasedHaloRenderer {
    public static LastEventBasedHaloRenderer instance = new LastEventBasedHaloRenderer();

    private static Set<CoordCache> renderQueue = new HashSet<CoordCache>();

    private static class CoordCache {
        final int x, y, z;
        final int color;
        final Cuboid6 cube;
        final int multipartSlot;
        
        public CoordCache(int x, int y, int z, int colorIndex, int slot, Cuboid6 cube) {
            this.x = x;
            this.y = y;
            this.z = z;
            this.color = colorIndex;
            this.multipartSlot = slot;
            this.cube = cube;
        }

        @Override
        public boolean equals(Object o) {
            if (o instanceof CoordCache) {
                CoordCache coord = (CoordCache) o;
                return x == coord.x && y == coord.y && z == coord.z && coord.cube.min.equals(cube.min) && coord.cube.max.equals(cube.max);
            }
            return false;
        }

    }
    
    public static void addLight(int x, int y, int z, int color, int slot, Cuboid6 box) {
        CoordCache cc = new CoordCache(x, y, z, color, slot, box);
        for (CoordCache c : renderQueue) {
            if (cc.equals(c))
                return;
        }
        renderQueue.add(cc);
    }


    @ForgeSubscribe
    public void onRenderWorldLast(RenderWorldLastEvent event) {
        List<CoordCache> removeQueue = new ArrayList<CoordCache>();
        Tessellator tess = Tessellator.instance;
        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE);
        GL11.glDisable(GL11.GL_TEXTURE_2D);
        GL11.glEnable(GL11.GL_BLEND);
        GL11.glPushMatrix();
        GL11.glDepthMask(false);
        EntityLivingBase view = Minecraft.getMinecraft().renderViewEntity;
        if (view != null) {
            double partials = event.partialTicks;
            double x = view.prevPosX + (view.posX - view.prevPosX) * partials;
            double y = view.prevPosY + (view.posY - view.prevPosY) * partials;
            double z = view.prevPosZ + (view.posZ - view.prevPosZ) * partials;
            GL11.glTranslated(-1 * x, -1 * y, -1 * z);
        }
        WorldClient w = event.context.theWorld;

        CCRenderState.reset();
        CCRenderState.startDrawing(7);
        for (CoordCache r : renderQueue) {
            if (shouldRemove(w, r))
                removeQueue.add(r);
            else
                renderHalo(tess, r);
        }
        CCRenderState.draw();

        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA);
        GL11.glEnable(GL11.GL_TEXTURE_2D);
        GL11.glColor3f(1, 1, 1);
        GL11.glDisable(GL11.GL_BLEND);
        GL11.glPopMatrix();
        GL11.glDepthMask(true);

        renderQueue.removeAll(removeQueue);
    }

    private static void renderHalo(Tessellator tess, CoordCache cc) {
        BasicRenderUtils.setFullBrightness();
        tess.setColorRGBA_I(PRColors.VALID_COLORS[cc.color].hex, 128);
        RenderUtils.renderBlock(cc.cube, 0, new Translation(cc.x, cc.y, cc.z), null, null);
    }

    private static boolean shouldRemove(World w, CoordCache cc) {
        TileMultipart t = BasicUtils.getMultipartTile(w, new BlockCoord(cc.x, cc.y, cc.z));
        if (t != null) {
            TMultiPart tp = t.partMap(cc.multipartSlot);
            if (tp instanceof ILight)
                return !((ILight)tp).isOn();
        }
        TileEntity te = BasicUtils.getTileEntity(w, new BlockCoord(cc.x, cc.y, cc.z), TileEntity.class);
        if (te instanceof ILight)
            return !((ILight)te).isOn();
        return true;
    }

}
