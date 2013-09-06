package mrtjp.projectred.core;

import static mrtjp.projectred.ProjectRedCore.blockMachines;
import static mrtjp.projectred.ProjectRedCore.itemComponent;
import static mrtjp.projectred.ProjectRedCore.itemDrawPlate;
import static mrtjp.projectred.ProjectRedCore.itemScrewdriver;
import static mrtjp.projectred.ProjectRedCore.itemWireDebugger;
import mrtjp.projectred.core.BlockBasics.EnumBasics;
import mrtjp.projectred.core.ItemPart.EnumPart;
import net.minecraft.item.ItemStack;
import net.minecraftforge.common.MinecraftForge;
import codechicken.lib.packet.PacketCustom;
import cpw.mods.fml.common.registry.LanguageRegistry;

public class CoreClientProxy extends CoreProxy {

    @Override
    public void init() {
        super.init();
        MinecraftForge.EVENT_BUS.register(new Messenger());
        PacketCustom.assignHandler(CoreCPH.channel, new CoreCPH());
    }
}
