package mrtjp.projectred.transmission;

import net.minecraft.item.ItemStack;
import net.minecraft.util.Icon;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;


public class RedAlloyWirePart extends RedwirePart {
    public RedAlloyWirePart(int side) {
        super(side);
    }

    @Override
    public int getColour() {
        return ((strength&0xFF)/2 + 60) << 24 | 0xFF;
    }
    
    @Override
    public EnumWire getWireType()
    {
        return EnumWire.RED_ALLOY;
    }
    
    @Override
    public String getType() {
        return "pr_redwire";
    }
}
