package mrtjp.projectred.transportation;

import mrtjp.projectred.ProjectRedTransportation;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public enum EnumPipe
{
    BASIC("pr_ptube", "basic"),
    ROUTEDJUNCTION("pr_rbasic", "routed", "unrouted"),
    ROUTEDINTERFACE("pr_rinterface", "routedconn", "unroutedconn"),
    ROUTEDCRAFTING("pr_rcrafting", "routedcrafting"),
    ROUTEDREQUEST("pr_rrequest", "routedrequest"),
    ROUTEDEXTENSION("pr_rextension", "routedextension")
    ;

    private EnumPipe(String type, String... textures)
    {
        this.type = type;
        spritePaths = textures;
        sprites = new Icon[textures.length];
    }

    public static EnumPipe[] VALID_PIPE = values();

    public final String type;

    // Rendering info
    public Icon[] sprites;
    public final String[] spritePaths;
    public int meta = this.ordinal();

    @SideOnly(Side.CLIENT)
    public void loadTextures(IconRegister reg)
    {
        if (spritePaths.length > 0)
        {
            sprites = new Icon[spritePaths.length];
            for (int i = 0; i < spritePaths.length; i++)
                sprites[i] = reg.registerIcon("projectred:pipes/" + spritePaths[i]);
        }
    }

    public ItemStack getItemStack()
    {
        return getItemStack(1);
    }

    public ItemStack getItemStack(int size)
    {
        return new ItemStack(ProjectRedTransportation.itemPartPipe(), size, meta);
    }
}
