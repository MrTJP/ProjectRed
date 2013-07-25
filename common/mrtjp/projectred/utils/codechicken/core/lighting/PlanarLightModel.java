package mrtjp.projectred.utils.codechicken.core.lighting;

import mrtjp.projectred.utils.codechicken.core.colour.Colour;
import mrtjp.projectred.utils.codechicken.core.colour.ColourRGBA;
import mrtjp.projectred.utils.codechicken.core.render.CCModel;
import mrtjp.projectred.utils.codechicken.core.render.CCRenderState;
import mrtjp.projectred.utils.codechicken.core.render.IVertexModifier;
import mrtjp.projectred.utils.codechicken.core.render.UV;
import mrtjp.projectred.utils.codechicken.core.vec.Vector3;
import net.minecraft.client.renderer.Tessellator;

/**
 * Faster precomputed version of LightModel that only works for axis planar sides
 */
public class PlanarLightModel implements IVertexModifier
{
    public ColourRGBA[] colours;
    
    public PlanarLightModel(int[] colours)
    {
        this.colours = new ColourRGBA[6];
        for(int i = 0; i < 6; i++)
            this.colours[i] = new ColourRGBA(colours[i]);
    }

    @Override
    public void applyModifiers(CCModel m, Tessellator tess, Vector3 vec, UV uv, Vector3 normal, int i)
    {
        ColourRGBA light = colours[CCModel.findSide(normal)];
        int colour = (m == null || m.colours == null) ? -1 : m.colours[i];
        Colour res = new ColourRGBA(colour).multiply(light);
        CCRenderState.vertexColour(res.r&0xFF, res.g&0xFF, res.b&0xFF, res.a&0xFF) ;
    }

    @Override
    public boolean needsNormals()
    {
        return true;
    }
}
