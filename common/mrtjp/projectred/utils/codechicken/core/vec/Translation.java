package mrtjp.projectred.utils.codechicken.core.vec;

import java.math.BigDecimal;
import java.math.MathContext;
import java.math.RoundingMode;

import org.lwjgl.opengl.GL11;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class Translation extends Transformation
{
    private Vector3 vec;
    
    public Translation(Vector3 vec)
    {
        this.vec = vec;
    }

    public Translation(double x, double y, double z)
    {
        this(new Vector3(x, y, z));
    }

    @Override
    public void apply(Vector3 vec)
    {
        vec.add(this.vec);
    }
    
    @Override
    public void applyN(Vector3 normal)
    {
    }

    @Override
    public void apply(Matrix4 mat)
    {
        mat.translate(vec);
    }
    
    @Override
    public Transformation at(Vector3 point)
    {
        return this;
    }
    
    @Override
    @SideOnly(Side.CLIENT)
    public void glApply()
    {
        GL11.glTranslated(vec.x, vec.y, vec.z);
    }
    
    @Override
    public Transformation inverse()
    {
        return new Translation(-vec.x, -vec.y, -vec.z);
    }
    
    @Override
    public String toString()
    {
        MathContext cont = new MathContext(4, RoundingMode.HALF_UP);
        return "Translation("+new BigDecimal(vec.x, cont)+", "+new BigDecimal(vec.y, cont)+", "+new BigDecimal(vec.z, cont)+")";
    }
}
