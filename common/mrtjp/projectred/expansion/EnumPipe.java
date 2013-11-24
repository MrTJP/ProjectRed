package mrtjp.projectred.expansion;

import mrtjp.projectred.ProjectRedExpansion;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public enum EnumPipe {

    BASIC("Basic Pipe", "pr_ptube", "basic"),
    ROUTEDJUNCTION("Routed Junction Pipe", "pr_rbasic", "routed", "unrouted"),
    ROUTEDINTERFACE("Routed Interface Pipe", "pr_rinterface", "routedconn", "unroutedconn"),
    ROUTEDCRAFTING("Routed Crafting Pipe", "pr_rcrafting", "routedcrafting"),
    ROUTEDREQUEST("Routed Request Pipe", "pr_rrequest", "routedrequest"),
    ;

    private EnumPipe(String name, String type, String... textures) {
        this.name = name;
        this.type = type;
        spritePaths = textures;
        sprites = new Icon[textures.length];
    }

    public static EnumPipe[] VALID_PIPE = values();

    public final String name;
    public final String type;

    // Rendering info
    public Icon[] sprites;
    public final String[] spritePaths;
    public int meta = this.ordinal();

    @SideOnly(Side.CLIENT)
    public void loadTextures(IconRegister reg) {
        if (spritePaths.length > 0) {
            sprites = new Icon[spritePaths.length];
            for (int i = 0; i < spritePaths.length; i++)
                sprites[i] = reg.registerIcon("projectred:pipes/" + spritePaths[i]);
        }
    }

    public ItemStack getItemStack() {
        return new ItemStack(ProjectRedExpansion.itemPartPipe, 1, meta);
    }
}
