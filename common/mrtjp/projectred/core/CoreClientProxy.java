package mrtjp.projectred.core;

import static mrtjp.projectred.ProjectRed.itemComponent;
import static mrtjp.projectred.ProjectRed.itemDrawPlate;
import mrtjp.projectred.core.ItemBackpack.EnumBackpack;
import mrtjp.projectred.core.ItemPart.EnumPart;
import net.minecraft.item.ItemStack;
import net.minecraftforge.common.MinecraftForge;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class CoreClientProxy implements IProxy {

    @Override
    public void preinit() {}
    
    @Override
    public void init() {
        for (EnumPart part : EnumPart.VALID_PARTS) {
            LanguageRegistry.addName(new ItemStack(itemComponent, 1, part.meta), part.fullName);
        }
        
        LanguageRegistry.addName(itemDrawPlate, "Draw Plate");
        
        for (EnumBackpack b : EnumBackpack.VALID_BP) {
            LanguageRegistry.addName(b.getItemStack(), b.fullname);
        }
        
        MinecraftForge.EVENT_BUS.register(new Messenger());
    }

    @Override
    public void postinit() {}
}
