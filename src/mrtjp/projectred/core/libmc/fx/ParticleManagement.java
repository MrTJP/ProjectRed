package mrtjp.projectred.core.libmc.fx;

import codechicken.lib.render.TextureUtils;
import cpw.mods.fml.common.eventhandler.SubscribeEvent;
import cpw.mods.fml.common.gameevent.TickEvent.ClientTickEvent;
import cpw.mods.fml.common.gameevent.TickEvent.Phase;
import net.minecraft.client.Minecraft;
import net.minecraft.client.particle.EntityFX;
import net.minecraft.client.renderer.ActiveRenderInfo;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.world.World;
import net.minecraftforge.client.event.RenderWorldLastEvent;
import net.minecraftforge.event.world.WorldEvent;
import org.lwjgl.opengl.GL11;

import java.util.ArrayList;
import java.util.Iterator;

public class ParticleManagement
{
    public static String name = "pr-fx";

    private final ArrayList<CoreParticle> particles = new ArrayList<CoreParticle>();
    private final ArrayList<CoreParticle> particleQueue = new ArrayList<CoreParticle>();

    public static final ParticleManagement instance = new ParticleManagement();

    public CoreParticle spawn(World world, String name, double x, double y, double z)
    {
        if (!world.isRemote)
            return null;

        if (Minecraft.getMinecraft().gameSettings.particleSetting == 2)
            return null;

        CoreParticle particle = new CoreParticle(world, x, y, z);
        particle.setTextureByName(name);

        addEffect(particle);

        return particle;
    }

    public void addEffect(CoreParticle effect)
    {
        particleQueue.add(effect);
    }

    @SubscribeEvent
    public void onRenderWorldLast(RenderWorldLastEvent event)
    {
        render(event.partialTicks);
    }

    @SubscribeEvent
    public void onWorldUnload(WorldEvent.Unload event)
    {
        particles.clear();
        particleQueue.clear();
    }

    @SubscribeEvent
    public void tickEnd(ClientTickEvent event)
    {
        if (event.phase == Phase.END)
            updateParticles();
    }

    private void updateParticles()
    {
        Minecraft.getMinecraft().mcProfiler.startSection(name + "-update");

        particles.addAll(particleQueue);
        particleQueue.clear();

        for (Iterator<CoreParticle> it = particles.iterator(); it.hasNext();)
        {
            EntityFX particle = it.next();

            particle.onUpdate();

            if (particle.isDead) it.remove();
        }
        Minecraft.getMinecraft().mcProfiler.endSection();
    }

    private void render(float partialTicks)
    {
        Minecraft.getMinecraft().mcProfiler.startSection(name + "-render");

        EntityLivingBase player = Minecraft.getMinecraft().renderViewEntity;
        EntityFX.interpPosX = player.lastTickPosX + (player.posX - player.lastTickPosX) * partialTicks;
        EntityFX.interpPosY = player.lastTickPosY + (player.posY - player.lastTickPosY) * partialTicks;
        EntityFX.interpPosZ = player.lastTickPosZ + (player.posZ - player.lastTickPosZ) * partialTicks;

        TextureUtils.bindAtlas(1);

        renderStandardParticles(partialTicks);

        Minecraft.getMinecraft().mcProfiler.endSection();
    }

    private void renderStandardParticles(float partialTicks)
    {
        float rotationX = ActiveRenderInfo.rotationX;
        float rotationZ = ActiveRenderInfo.rotationZ;
        float rotationYZ = ActiveRenderInfo.rotationYZ;
        float rotationXY = ActiveRenderInfo.rotationXY;
        float rotationXZ = ActiveRenderInfo.rotationXZ;

        GL11.glPushAttrib(16640);

        GL11.glColor4f(1.0F, 1.0F, 1.0F, 1.0F);
        GL11.glDepthMask(false);
        GL11.glEnable(GL11.GL_BLEND);
        GL11.glBlendFunc(GL11.GL_SRC_ALPHA, GL11.GL_ONE_MINUS_SRC_ALPHA);
        GL11.glAlphaFunc(GL11.GL_GREATER, 0.003921569F);
        GL11.glDisable(32826);

        Tessellator tessellator = Tessellator.instance;
        tessellator.startDrawingQuads();

        for (CoreParticle particle : particles)
        {
            tessellator.setBrightness(particle.getBrightnessForRender(partialTicks));
            particle.renderParticle(tessellator, partialTicks, rotationX, rotationXZ, rotationZ, rotationYZ, rotationXY);
        }

        tessellator.draw();

        GL11.glPopAttrib();
    }
}
