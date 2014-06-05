package mrtjp.projectred.core.libmc.fx

import net.minecraft.client.particle.EntityFX
import net.minecraft.world.World
import codechicken.lib.vec.{BlockCoord, Vector3}
import mrtjp.projectred.core.libmc.PRColors
import net.minecraft.client.renderer.Tessellator

class CoreParticle(w:World, px:Double, py:Double, pz:Double) extends EntityFX(w, 0.0D, 0.0D, 0.0D, 0.0D, 0.0D, 0.0D)
{
    setPosition(px, py, pz)
    noClip = true

    motionX = 0.0D
    motionY = 0.0D
    motionZ = 0.0D

    var r = 1.0F
    var g = 1.0F
    var b = 1.0F
    var a = 1.0F

    var scaleX = 0.2F
    var scaleY = 0.2F
    var scaleZ = 0.2F

    //var particleMaxAge = 0
    //var particleAge = 0
    private var ignoreMaxAge = false

    var ignoreNoLogics = false
    var ignoreVelocity = false

    override def isBurning = false
    override def canTriggerWalking = false
    override def canAttackWithItem = false

    var logics = Vector[ParticleLogic]()

    def setTextureByName(name:String)
    {
        particleIcon = ParticleIconRegistry.instance.getIcon(name)
    }

    def setRGBColorF(r:Float, g:Float, b:Float)
    {
        this.r = r
        this.g = g
        this.b = b
    }

    def setPRColor(color:PRColors)
    {
        setRGBColorF((color.c.r&0xFF)/255F, (color.c.g&0xFF)/255F, (color.c.b&0xFF)/255F)
    }

    def setScale(scale:Float)
    {
        scaleX = scale
        scaleY = scale
        scaleZ = scale
    }

    def setIgnoreMaxAge(ignore:Boolean)
    {
        ignoreMaxAge = ignore
        particleAge = 0
    }

    def +=(logic:ParticleLogic):this.type  =
    {
        (logics :+ logic).sorted(LogicComparator)
        this
    }

    def ++=(it:Iterable[ParticleLogic]):this.type =
    {
        (logics ++ it).sorted(LogicComparator)
        this
    }

    override def getBrightnessForRender(par1:Float) =
    {
        var f = (particleAge+par1)/particleMaxAge
        if (f < 0.0F) f = 0.0F
        if (f > 1.0F) f = 1.0F

        val i = super.getBrightnessForRender(par1)
        var j = i&0xFF
        val k = i>>16&0xFF
        j += (f*15.0F*16.0F).asInstanceOf[Int]
        if (j > 240) j = 240

        j|k<<16
    }

    override def getBrightness(par1:Float) =
    {
        var f = (particleAge+par1)/particleMaxAge
        if (f < 0.0F) f = 0.0F
        if (f > 1.0F) f = 1.0F
        val f1 = super.getBrightness(par1)
        f1*f+(1.0F-f)
    }

    override def onUpdate()
    {
        ticksExisted += 1
        prevDistanceWalkedModified = distanceWalkedModified
        prevPosX = posX
        prevPosY = posY
        prevPosZ = posZ
        prevRotationPitch = rotationPitch
        prevRotationYaw = rotationYaw

        if (!ignoreVelocity) moveEntity(motionX, motionY, motionZ)

        val rem = Vector.newBuilder[ParticleLogic]
        def iterate()
        {
            for (l <- logics)
            {
                if (l.getFinished) rem += l
                else
                {
                    l.onUpdate(worldObj, this)
                    if (l.isFinalLogic) return
                }
            }
        }
        iterate()
        val toRem = rem.result()
        logics = logics.filterNot(l => toRem.contains(l))

        if ({particleAge+=1; particleAge-1} > particleMaxAge && !ignoreMaxAge ||
            !ignoreNoLogics && logics.size == 0) setDead()
    }

    def position = new Vector3(posX, posY, posZ)

    def blockPosition = new BlockCoord(Math.floor(posX).asInstanceOf[Int],
        Math.floor(posY).asInstanceOf[Int], Math.floor(posZ).asInstanceOf[Int])

    override def entityInit(){}

    override def getFXLayer = 2

    override def renderParticle(tessellator:Tessellator, partialframe:Float, cosyaw:Float, cospitch:Float, sinyaw:Float, sinsinpitch:Float, cossinpitch:Float)
    {
        if (!worldObj.isRemote) return

        val f11 = (prevPosX+(posX-prevPosX)*partialframe-EntityFX.interpPosX).asInstanceOf[Float]
        val f12 = (prevPosY+(posY-prevPosY)*partialframe-EntityFX.interpPosY).asInstanceOf[Float]
        val f13 = (prevPosZ+(posZ-prevPosZ)*partialframe-EntityFX.interpPosZ).asInstanceOf[Float]

        if (particleIcon == null) return

        tessellator.setBrightness(251658480)
        tessellator.setColorRGBA_F(r, g, b, a)

        val min_u = particleIcon.getMinU
        val min_v = particleIcon.getMinV
        val max_u = particleIcon.getMaxU
        val max_v = particleIcon.getMaxV

        tessellator.addVertexWithUV(f11-cosyaw*scaleX-sinsinpitch*scaleX, f12-cospitch*scaleY, f13-sinyaw*scaleZ-cossinpitch*scaleZ, max_u, max_v)
        tessellator.addVertexWithUV(f11-cosyaw*scaleX+sinsinpitch*scaleX, f12+cospitch*scaleY, f13-sinyaw*scaleZ+cossinpitch*scaleZ, max_u, min_v)
        tessellator.addVertexWithUV(f11+cosyaw*scaleX+sinsinpitch*scaleX, f12+cospitch*scaleY, f13+sinyaw*scaleZ+cossinpitch*scaleZ, min_u, min_v)
        tessellator.addVertexWithUV(f11+cosyaw*scaleX-sinsinpitch*scaleX, f12-cospitch*scaleY, f13+sinyaw*scaleZ-cossinpitch*scaleZ, min_u, max_v)
    }
}

object LogicComparator extends Ordering[ParticleLogic]
{
    def compare(o1:ParticleLogic, o2:ParticleLogic) =
        if (o1 eq o2) 0 else if (o1.getPriority > o2.getPriority) 1 else -1
}

