package mrtjp.projectred.utils.codechicken.core.vec;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

/**
 * Interface for any 3D vector transformation
 */
public abstract class Transformation
{   
    /**
     * Applies this transformation to a position vector
     * @param vec The vector to transform
     */
    public abstract void apply(Vector3 vec);
    
    /**
     * Applies this transformation to a normal (doesn't translate)
     * @param normal The normal to transform
     */
    public abstract void applyN(Vector3 normal);
    
    /**
     * Applies this transformation to a matrix as a multiplication on the right hand side.
     * @param mat The matrix to combine this transformation with
     */
    public abstract void apply(Matrix4 mat);
    
    /**
     * @param point The point in OBJECT space to apply this transformation around
     * @return Wraps this transformation in a translation to point and then back from point
     */
    public Transformation at(Vector3 point)
    {
        return new TranslatedTransformation(this, point);
    }
    
    /**
     * Creates a transformation list composed of this transformation followed by t
     * If this is a TransformationList, the transformation will be appended and this returned
     */
    public TransformationList with(Transformation t)
    {
        return new TransformationList(this, t);
    }
    
    @SideOnly(Side.CLIENT)
    public abstract void glApply();

    public abstract Transformation inverse();
}
