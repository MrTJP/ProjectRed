package mrtjp.projectred.core.fx;

import codechicken.lib.vec.BlockCoord;
import codechicken.lib.vec.Vector3;
import mrtjp.projectred.core.PRColors;
import net.minecraft.client.particle.EntityFX;
import net.minecraft.client.renderer.Tessellator;
import net.minecraft.world.World;

import java.util.*;

public class CoreParticle extends EntityFX
{
    private boolean ignoreMaxAge;
    private final List<ParticleLogic> logics = new ArrayList<ParticleLogic>();
    private float r;
    private float g;
    private float b;
    private float a;
    private float scaleX = 0.2F;
    private float scaleY = 0.2F;
    private float scaleZ = 0.2F;
    private int particleMaxAge;
    private int particleAge;
    private int particleFrameCount;
    private final int maxFrames;
    private float uStep;
    private float vStep;
    private String particleName;
    private boolean doRender;
    private boolean isAffectedByGravity = false;
    private boolean ignoreNoLogics = false;
    private boolean doVelocityUpdates = true;
    public static String[] particleTypes;

    public void setParticleAge(int age)
    {
        particleAge = age;
    }

    public CoreParticle(World par1World, double par2, double par4, double par6)
    {
        super(par1World, 0.0D, 0.0D, 0.0D, 0.0D, 0.0D, 0.0D);
        motionX = 0.0D;
        motionY = 0.0D;
        motionZ = 0.0D;
        setPosition(par2, par4, par6);
        par2 += (rand.nextFloat() - rand.nextFloat()) * 0.05F;
        par4 += (rand.nextFloat() - rand.nextFloat()) * 0.05F;
        par6 += (rand.nextFloat() - rand.nextFloat()) * 0.05F;
        r = g = b = a = 1.0F;
        noClip = true;
        isDead = false;
        ignoreMaxAge = false;
        particleMaxAge = 20 + rand.nextInt(20);
        doRender = true;
        maxFrames = 1;

        particleGravity = 1.0F;

        setRandomScale(0.1F, 0.3F);
    }

    public void setDoRender(boolean doRender)
    {
        this.doRender = doRender;
    }

    public CoreParticle setAffectedByGravity()
    {
        isAffectedByGravity = true;
        return this;
    }

    public CoreParticle setNoLogicRequired()
    {
        ignoreNoLogics = true;
        return this;
    }

    @Override
    public boolean isBurning()
    {
        return false;
    }

    @Override
    protected boolean canTriggerWalking()
    {
        return false;
    }

    public void setNoVelocityUpdates()
    {
        doVelocityUpdates = false;
    }

    @Override
    public boolean canAttackWithItem()
    {
        return false;
    }

    public void setTextureByName(String name)
    {
        particleName = name;
        particleIcon = ParticleIconRegistry.instance.getIcon(name);
    }

    public void addRandomOffset(double maxX, double maxY, double maxZ)
    {
        double newX = posX + rand.nextDouble() * maxX - maxX / 2.0D;
        double newY = posY + rand.nextDouble() * maxY - maxY / 2.0D;
        double newZ = posZ + rand.nextDouble() * maxZ - maxZ / 2.0D;

        setPosition(newX, newY, newZ);
    }

    public float getScaleX()
    {
        return scaleX;
    }

    public float getScaleY()
    {
        return scaleY;
    }

    public float getScaleZ()
    {
        return scaleZ;
    }

    public CoreParticle setRandomScale(float min, float max)
    {
        setScale(rand.nextFloat() * (max - min) + min);
        return this;
    }

    public void setScale(float scale)
    {
        scaleX = scale;
        scaleY = scale;
        scaleZ = scale;
    }

    public void setScale(float scaleX, float scaleY, float scaleZ)
    {
        this.scaleX = scaleX;
        this.scaleY = scaleY;
        this.scaleZ = scaleZ;
    }

    public int getAge()
    {
        return particleAge;
    }

    public int getMaxAge()
    {
        return particleMaxAge;
    }

    public void setMaxAge(int age)
    {
        particleMaxAge = age;
    }

    public void setIgnoreMaxAge(boolean ignore)
    {
        ignoreMaxAge = ignore;
        particleAge = 0;
    }

    public void setRGBColorF(float r, float g, float b)
    {
        this.r = r;
        this.g = g;
        this.b = b;
    }

    public void setPRColor(PRColors color)
    {
        setRGBColorF((color.c.r & 0xFF) / 255F, (color.c.g & 0xFF) / 255F, (color.c.b & 0xFF) / 255F);
    }

    public void setAlpha(float alpha)
    {
        a = alpha;
    }

    public float getRed()
    {
        return r;
    }

    public float getGreen()
    {
        return g;
    }

    public float getBlue()
    {
        return b;
    }

    public float getAlpha()
    {
        return a;
    }

    public CoreParticle addLogic(ParticleLogic logic)
    {
        logics.add(logic);
        Collections.sort(logics, LogicComparator.instance);
        return this;
    }

    public void removeLogic(ParticleLogic logic)
    {
        logics.remove(logic);
    }

    public void clearLogic()
    {
        logics.clear();
    }

    @Override
    public int getBrightnessForRender(float par1)
    {
        float f = (particleAge + par1) / particleMaxAge;

        if (f < 0.0F)
            f = 0.0F;

        if (f > 1.0F)
            f = 1.0F;

        int i = super.getBrightnessForRender(par1);
        int j = i & 0xFF;
        int k = i >> 16 & 0xFF;
        j += (int) (f * 15.0F * 16.0F);

        if (j > 240)
            j = 240;

        return j | k << 16;
    }

    @Override
    public float getBrightness(float par1)
    {
        float f = (particleAge + par1) / particleMaxAge;

        if (f < 0.0F)
            f = 0.0F;

        if (f > 1.0F)
            f = 1.0F;

        float f1 = super.getBrightness(par1);
        return f1 * f + (1.0F - f);
    }

    @Override
    public void onUpdate()
    {
        ticksExisted += 1;
        prevDistanceWalkedModified = distanceWalkedModified;
        prevPosX = posX;
        prevPosY = posY;
        prevPosZ = posZ;
        prevRotationPitch = rotationPitch;
        prevRotationYaw = rotationYaw;

        if (isAffectedByGravity)
            motionY -= 0.04D * particleGravity;

        if (doVelocityUpdates)
            moveEntity(motionX, motionY, motionZ);

        Iterator<ParticleLogic> it = logics.iterator();
        while (it.hasNext())
        {
            ParticleLogic l = it.next();
            if (l.getFinished())
            {
                it.remove();
                continue;
            }
            l.onUpdate(worldObj, this);
            if (l.isFinalLogic())
                break;
        }

        if (particleAge++ > particleMaxAge && !ignoreMaxAge || !ignoreNoLogics && logics.size() == 0)
            setDead();
    }

    public Vector3 position()
    {
        return new Vector3(posX, posY, posZ);
    }

    public BlockCoord blockPosition()
    {
        return new BlockCoord((int)(Math.floor(posX)), (int)(Math.floor(posY)), (int)(Math.floor(posZ)));
    }

    @Override
    protected void entityInit()
    {
    }

    @Override
    public int getFXLayer()
    {
        return 2;
    }

    @Override
    public void renderParticle(Tessellator tessellator, float partialframe, float cosyaw, float cospitch, float sinyaw, float sinsinpitch, float cossinpitch)
    {
        if (!worldObj.isRemote)
            return;

        float f11 = (float) (prevPosX + (posX - prevPosX) * partialframe - interpPosX);
        float f12 = (float) (prevPosY + (posY - prevPosY) * partialframe - interpPosY);
        float f13 = (float) (prevPosZ + (posZ - prevPosZ) * partialframe - interpPosZ);

        if (particleIcon == null)
            return;

        tessellator.setBrightness(251658480);
        tessellator.setColorRGBA_F(getRed(), getGreen(), getBlue(), getAlpha());

        float scaleFactorX = getScaleX();
        float scaleFactorY = getScaleY();
        float scaleFactorZ = getScaleZ();

        float min_u = particleIcon.getMinU();
        float min_v = particleIcon.getMinV();
        float max_u = particleIcon.getMaxU();
        float max_v = particleIcon.getMaxV();

        tessellator.addVertexWithUV(f11 - cosyaw * scaleFactorX - sinsinpitch * scaleFactorX, f12 - cospitch * scaleFactorY, f13 - sinyaw * scaleFactorZ - cossinpitch * scaleFactorZ, max_u, max_v);
        tessellator.addVertexWithUV(f11 - cosyaw * scaleFactorX + sinsinpitch * scaleFactorX, f12 + cospitch * scaleFactorY, f13 - sinyaw * scaleFactorZ + cossinpitch * scaleFactorZ, max_u, min_v);
        tessellator.addVertexWithUV(f11 + cosyaw * scaleFactorX + sinsinpitch * scaleFactorX, f12 + cospitch * scaleFactorY, f13 + sinyaw * scaleFactorZ + cossinpitch * scaleFactorZ, min_u, min_v);
        tessellator.addVertexWithUV(f11 + cosyaw * scaleFactorX - sinsinpitch * scaleFactorX, f12 - cospitch * scaleFactorY, f13 + sinyaw * scaleFactorZ - cossinpitch * scaleFactorZ, min_u, max_v);
    }

    public static class LogicComparator implements Comparator<ParticleLogic>
    {
        public static final LogicComparator instance = new LogicComparator();

        @Override
        public int compare(ParticleLogic o1, ParticleLogic o2)
        {
            return o1 == o2 ? 0 : o1.getPriority() > o2.getPriority() ? 1 : -1;
        }
    }
}