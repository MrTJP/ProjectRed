package mrtjp.projectred.utils.codechicken.core.vec;

public class SwapYZ extends CoordinateSystem
{
    @Override
    public void convert(Vector3 vec)
    {
        double vz = vec.z;
        vec.z = vec.y;
        vec.y = vz;
    }
}
