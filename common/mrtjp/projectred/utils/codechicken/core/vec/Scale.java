package mrtjp.projectred.utils.codechicken.core.vec;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

import org.lwjgl.opengl.GL11;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class Scale extends Transformation
{
    private Vector3 factor;
    
    public Scale(Vector3 factor)
    {
        this.factor = factor;
    }

    public Scale(double factor)
    {
        this(new Vector3(factor, factor, factor));
    }
    
    public Scale(double x, double y, double z)
    {
        this(new Vector3(x, y, z));
    }

    @Override
    public void apply(Vector3 vec)
    {
        vec.multiply(factor);
    }
    
    @Override
    public void applyN(Vector3 normal)
    {
    }
    
    @Override
    public void apply(Matrix4 mat)
    {
        mat.scale(factor);
    }
    
    @Override
    @SideOnly(Side.CLIENT)
    public void glApply()
    {
        GL11.glScaled(factor.x, factor.y, factor.z);
    }
    
    @Override
    public Transformation inverse()
    {
        return new Scale(1/factor.x, 1/factor.y, 1/factor.z);
    }
    
    @Override
    public String toString()
    {
        MathContext cont = new MathContext(4, RoundingMode.HALF_UP);
        return "Scale("+new BigDecimal(factor.x, cont)+", "+new BigDecimal(factor.y, cont)+", "+new BigDecimal(factor.z, cont)+")";
    }
}
