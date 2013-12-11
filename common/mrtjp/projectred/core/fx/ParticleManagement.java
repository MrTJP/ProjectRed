package mrtjp.projectred.core.fx;

import java.util.ArrayList;
import java.util.EnumSet;
import java.util.Iterator;

import net.minecraft.client.Minecraft;
import net.minecraft.client.particle.EntityFX;
import net.minecraft.client.renderer.ActiveRenderInfo;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.client.renderer.texture.TextureMap;
import net.minecraft.entity.EntityLivingBase;
import net.minecraft.world.World;
import net.minecraftforge.client.event.RenderWorldLastEvent;
import net.minecraftforge.event.ForgeSubscribe;
import net.minecraftforge.event.world.WorldEvent;

import org.lwjgl.opengl.GL11;
import org.lwjgl.opengl.GL12;

import cpw.mods.fml.common.ITickHandler;
import cpw.mods.fml.common.TickType;

public class ParticleManagement implements ITickHandler
{
    public static String name = "pr-fx";

    private final ArrayList<EntityFX> particles = new ArrayList<EntityFX>();
    private final ArrayList<EntityFX> particleQueue = new ArrayList<EntityFX>();

    public static final ParticleManagement instance = new ParticleManagement();

    public CoreParticle spawn(World world, String name, double x, double y, double z)
    {
        if (!world.isRemote)
            return null;

        CoreParticle particle = new CoreParticle(world, x, y, z);
        particle.setTextureByName(name);

        addEffect(particle);

        return particle;
    }

    public void addEffect(EntityFX effect)
    {
        particleQueue.add(effect);
    }

    @ForgeSubscribe
    public void onRenderWorldLast(RenderWorldLastEvent event)
    {
        render(event.partialTicks);
    }

    @ForgeSubscribe
    public void onWorldUnload(WorldEvent.Unload event)
    {
        particles.clear();
        particleQueue.clear();
    }

    @Override
    public void tickStart(EnumSet<TickType> type, Object... tickData)
    {
    }

    @Override
    public void tickEnd(EnumSet<TickType> type, Object... tickData)
    {
        updateParticles();
    }

    @Override
    public EnumSet<TickType> ticks()
    {
        return EnumSet.of(TickType.CLIENT);
    }

    @Override
    public String getLabel()
    {
        return name;
    }

    private void updateParticles()
    {
        Minecraft.getMinecraft().mcProfiler.startSection(name + "-update");

        particles.addAll(particleQueue);
        particleQueue.clear();

        for (Iterator<EntityFX> it = particles.iterator(); it.hasNext();)
        {
            EntityFX particle = it.next();

            particle.onUpdate();

            if (particle.isDead)
                it.remove();
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

        Minecraft.getMinecraft().renderEngine.bindTexture(TextureMap.locationItemsTexture);

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

        for (EntityFX particle : particles)
        {
            tessellator.setBrightness(particle.getBrightnessForRender(partialTicks));
            particle.renderParticle(tessellator, partialTicks, rotationX, rotationXZ, rotationZ, rotationYZ, rotationXY);
        }

        tessellator.draw();

        GL11.glPopAttrib();
    }
}
