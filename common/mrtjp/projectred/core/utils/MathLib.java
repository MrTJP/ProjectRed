package mrtjp.projectred.core.utils;

import java.security.InvalidParameterException;

import net.minecraft.entity.Entity;
import net.minecraft.util.MathHelper;

import org.lwjgl.util.vector.Matrix4f;
import org.lwjgl.util.vector.Vector3f;

import codechicken.lib.vec.Vector3;

public class MathLib
{
    public static Matrix4f createEntityRotateMatrix(Entity entity)
    {
        double yaw = Math.toRadians(entity.rotationYaw - 180);
        double pitch = Math.toRadians(entity.rotationPitch);

        Matrix4f initial = new Matrix4f();
        initial.rotate((float) pitch, new Vector3f(1, 0, 0));
        initial.rotate((float) yaw, new Vector3f(0, 1, 0));
        return initial;
    }

    public static Vector3 getLook(Entity source, float f)
    {
        if (f == 1.0F)
        {
            float var2 = MathHelper.cos(-source.rotationYaw * 0.01745329F - 3.141593F);
            float var3 = MathHelper.sin(-source.rotationYaw * 0.01745329F - 3.141593F);
            float var4 = -MathHelper.cos(-source.rotationPitch * 0.01745329F);
            float var5 = MathHelper.sin(-source.rotationPitch * 0.01745329F);
            return new Vector3(var3 * var4, var5, var2 * var4);
        }

        float var2 = source.prevRotationPitch + (source.rotationPitch - source.prevRotationPitch) * f;
        float var3 = source.prevRotationYaw + (source.rotationYaw - source.prevRotationYaw) * f;
        float var4 = MathHelper.cos(-var3 * 0.01745329F - 3.141593F);
        float var5 = MathHelper.sin(-var3 * 0.01745329F - 3.141593F);
        float var6 = -MathHelper.cos(-var2 * 0.01745329F);
        float var7 = MathHelper.sin(-var2 * 0.01745329F);
        return new Vector3(var5 * var6, var7, var4 * var6);
    }

    public static Vector3 bezier(Vector3 s, Vector3 c1, Vector3 c2, Vector3 e, float t)
    {
        if ((t < 0.0F) || (t > 1.0F))
            return s;

        float one_minus_t = 1.0F - t;

        Vector3 retValue = new Vector3(0.0D, 0.0D, 0.0D);
        Vector3[] terms = new Vector3[4];
        terms[0] = calcNewVector(one_minus_t * one_minus_t * one_minus_t, s);
        terms[1] = calcNewVector(3.0F * one_minus_t * one_minus_t * t, c1);
        terms[2] = calcNewVector(3.0F * one_minus_t * t * t, c2);
        terms[3] = calcNewVector(t * t * t, e);

        for (int i = 0; i < 4; i++)
            retValue.add(terms[i]);

        return retValue;
    }

    private static Vector3 calcNewVector(float scaler, Vector3 base)
    {
        Vector3 retValue = new Vector3(base.x, base.y, base.z);
        retValue.multiply(scaler);
        return retValue;
    }
}
