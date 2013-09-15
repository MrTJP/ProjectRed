package mrtjp.projectred.illumination;

import mrtjp.projectred.ProjectRedIllumination;
import net.minecraft.client.renderer.RenderBlocks;
import net.minecraft.item.ItemStack;
import codechicken.lib.lighting.LazyLightMatrix;
import codechicken.lib.vec.Cuboid6;
import codechicken.lib.vec.Vector3;
import codechicken.multipart.PartMap;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class LanternPart extends BaseLightPart {
    
    static Cuboid6 bounds = new Cuboid6(.34f, .25f, .34f, .66f, .75f, .66f);
    
    @Override
    @SideOnly(Side.CLIENT)
    public void renderStatic(Vector3 pos, LazyLightMatrix olm, int pass) {
        if (pass == 0)
            RenderLantern.instance.renderLantern(this);
    }

    @Override
    @SideOnly(Side.CLIENT) 
    public void drawBreaking(RenderBlocks r){
        RenderLantern.instance.renderBreaking(x(), y(), z(), r.overrideBlockTexture);
    }

    @Override
    public ItemStack getItem() {
        return new ItemStack(isInverted ? ProjectRedIllumination.itemPartInvLantern : ProjectRedIllumination.itemPartLantern, 1, type);
    }

    @Override
    public Cuboid6 getBounds() {
        return bounds;
    }
    
    @Override
    public int getSlotMask() {
        return 1 << PartMap.CENTER.i;
    }
    
    @Override
    public String getType() {
        return "pr_lantern";
    }
}
