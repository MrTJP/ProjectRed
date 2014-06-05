package mrtjp.projectred.core.libmc

import org.lwjgl.util.vector.{Vector3f, Matrix4f}
import codechicken.lib.vec.Vector3
import net.minecraft.entity.Entity
import java.util.Random

object MathLib
{
    def createEntityRotateMatrix(entity:Entity):Matrix4f =
    {
        val yaw = Math.toRadians(entity.rotationYaw - 180)
        val pitch = Math.toRadians(entity.rotationPitch)
        val initial = new Matrix4f
        initial.rotate(pitch.asInstanceOf[Float], new Vector3f(1, 0, 0))
        initial.rotate(yaw.asInstanceOf[Float], new Vector3f(0, 1, 0))
        initial
    }

    def bezier(s:Vector3, c1:Vector3, c2:Vector3, e:Vector3, t:Float):Vector3 =
    {
        if ((t < 0.0F) || (t > 1.0F)) return s
        val one_minus_t = 1.0F - t
        val retValue = new Vector3(0.0D, 0.0D, 0.0D)
        val terms = new Array[Vector3](4)
        terms(0) = calcNewVector(one_minus_t * one_minus_t * one_minus_t, s)
        terms(1) = calcNewVector(3.0F * one_minus_t * one_minus_t * t, c1)
        terms(2) = calcNewVector(3.0F * one_minus_t * t * t, c2)
        terms(3) = calcNewVector(t * t * t, e)

        for (i <- 0 until 4) retValue.add(terms(i))
        retValue
    }

    private def calcNewVector(scaler:Float, base:Vector3):Vector3 =
    {
        val retValue = new Vector3(base.x, base.y, base.z)
        retValue.multiply(scaler)
        retValue
    }

    private val random = new Random
    def randomFromIntRange(az:Range) = az(random.nextInt(az.size))

    def leastSignificant(mask:Int) =
    {
        var bit = 0
        var m = mask
        while ((m&1) == 0 && m != 0)
        {
            bit += 1
            m <<= 1
        }
        bit
    }
}
