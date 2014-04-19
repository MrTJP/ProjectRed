package mrtjp.projectred.core.libmc;

import java.nio.FloatBuffer;
import java.nio.IntBuffer;

import net.minecraft.client.renderer.GLAllocation;
import net.minecraft.util.Vec3;

import org.lwjgl.BufferUtils;
import org.lwjgl.opengl.GL11;
import org.lwjgl.util.glu.GLU;
import org.lwjgl.util.vector.Matrix4f;

public class OpenGLLib
{
    private static IntBuffer viewport = GLAllocation.createDirectIntBuffer(16);
    private static FloatBuffer modelview = GLAllocation.createDirectFloatBuffer(16);
    private static FloatBuffer projection = GLAllocation.createDirectFloatBuffer(16);
    private static FloatBuffer objectCoords = GLAllocation.createDirectFloatBuffer(3);

    public static void updateMatrices()
    {
        GL11.glGetFloat(GL11.GL_MODELVIEW_MATRIX, modelview);
        GL11.glGetFloat(GL11.GL_PROJECTION_MATRIX, projection);
        GL11.glGetInteger(GL11.GL_VIEWPORT, viewport);
    }

    public static Vec3 unproject(float winX, float winY, float winZ)
    {
        GLU.gluUnProject(winX, winY, winZ, modelview, projection, viewport, objectCoords);

        float objectX = objectCoords.get(0);
        float objectY = objectCoords.get(1);
        float objectZ = objectCoords.get(2);

        return Vec3.createVectorHelper(objectX, objectY, objectZ);
    }

    private static FloatBuffer matrixBuffer = BufferUtils.createFloatBuffer(16);

    public static synchronized void loadMatrix(Matrix4f transform)
    {
        transform.store(matrixBuffer);
        matrixBuffer.flip();
        GL11.glMultMatrix(matrixBuffer);
    }
}
