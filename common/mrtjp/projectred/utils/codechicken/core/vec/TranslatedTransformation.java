package mrtjp.projectred.utils.codechicken.core.vec;

import org.lwjgl.opengl.GL11;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class TranslatedTransformation extends Transformation
{
    public Vector3 point;
    public Transformation wrapped;
    
    public TranslatedTransformation(Transformation t, Vector3 vec)
    {
        point = vec;
        wrapped = t;
    }
    
    @Override
    public void apply(Vector3 vec)
    {
        vec.subtract(point).apply(wrapped).add(point);
    }

    @Override
    public void applyN(Vector3 normal)
    {
        wrapped.applyN(normal);
    }

    @Override
    public void apply(Matrix4 mat)
    {
        mat.translate(point).apply(wrapped)
            .translate(new Vector3(-point.x, -point.y, -point.z));
    }
    
    @Override
    @SideOnly(Side.CLIENT)
    public void glApply()
    {
        GL11.glTranslated(point.x, point.y, point.z);
        wrapped.glApply();
        GL11.glTranslated(-point.x, -point.y, -point.z);
    }
    
    @Override
    public Transformation inverse()
    {
        return new TranslatedTransformation(wrapped.inverse(), point);
    }
    
    @Override
    public String toString()
    {
        return point.translation()+"\n"+
                wrapped.toString()+"\n"+
                new Translation(-point.x, -point.y, -point.z);
    }
}
