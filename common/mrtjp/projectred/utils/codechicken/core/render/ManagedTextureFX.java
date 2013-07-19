package mrtjp.projectred.utils.codechicken.core.render;

public class ManagedTextureFX extends TextureFX
{
    public boolean changed;
    
    public ManagedTextureFX(int size, String name)
    {
        super(size, name);
        imageData = new byte[size*size*4];
    }
    
    @Override
    public void setup()
    {
    }
    
    public void setData(byte[] b)
    {
        System.arraycopy(b, 0, imageData, 0, imageData.length);
        changed = true;
    }
    
    @Override
    public boolean changed()
    {
        boolean r = changed;
        changed = false;
        return r;
    }
}
