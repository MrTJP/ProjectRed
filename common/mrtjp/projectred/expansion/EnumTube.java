package mrtjp.projectred.expansion;

import mrtjp.projectred.ProjectRedExpansion;
import net.minecraft.item.ItemStack;

public enum EnumTube {

    PRESSURIZEDTUBE("Pressurized Tube", "pr_ptube");
    
    public static final EnumTube VALID_TUBE[] = values();
    
    public final String name;
    public final String part;
    public final int meta = ordinal();

    private EnumTube(String name, String part) {
        this.name = name;
        this.part = part;
    }
    
    public ItemStack getItemStack() {
        return new ItemStack(ProjectRedExpansion.itemPartTube, 1, meta);
    }
}
