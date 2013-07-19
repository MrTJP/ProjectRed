package mrtjp.projectred.utils.codechicken.core.render;

public class UVScale implements IUVTransformation
{
    double su;
    double sv;
    
    public UVScale(double scaleu, double scalev)
    {
        su = scaleu;
        sv = scalev;
    }
    
    @Override
    public void transform(UV uv)
    {
        uv.u*=su;
        uv.v*=sv;
    }
}
