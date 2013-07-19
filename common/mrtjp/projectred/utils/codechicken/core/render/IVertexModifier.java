package mrtjp.projectred.utils.codechicken.core.render;

import mrtjp.projectred.utils.codechicken.core.vec.Vector3;
import net.minecraft.client.renderer.Tessellator;

public interface IVertexModifier
{
    public void applyModifiers(CCModel m, Tessellator tess, Vector3 vec, UV uv, Vector3 normal, int i);

    public boolean needsNormals();
}
