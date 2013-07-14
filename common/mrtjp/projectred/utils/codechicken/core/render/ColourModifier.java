package mrtjp.projectred.utils.codechicken.core.render;

import mrtjp.projectred.utils.codechicken.core.vec.Vector3;
import net.minecraft.client.renderer.Tessellator;

public class ColourModifier implements IVertexModifier
{
    public static final ColourModifier instance = new ColourModifier();

    @Override
    public void applyModifiers(CCModel m, Tessellator tess, Vector3 vec, UV uv, Vector3 normal, int i)
    {
        if(CCRenderState.useModelColours() && m != null && m.colours != null)
            CCRenderState.vertexColour(m.colours[i]);
    }

    @Override
    public boolean needsNormals()
    {
        return false;
    }
}
