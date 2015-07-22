package mrtjp.projectred.core.libmc

import mrtjp.core.resource.ResourceAction
import net.minecraft.util.ResourceLocation

object PRResources
{
    val guiBag = registerPR("textures/gui/backpack.png")
    val guiChipContainer = registerPR("textures/gui/chip_settings.png")
    val guiPipeInterface = registerPR("textures/gui/interface_pipe.png")
    val guiPipeRequest = registerPR("textures/gui/request_pipe.png")
    val guiPipeFirewall = registerPR("textures/gui/firewall_pipe.png")
    val guiFurnace = registerPR("textures/gui/furnace.png")
    val guiPrototyper = registerPR("textures/gui/ic_workbench.png")
    val guiProjectbench = registerPR("textures/gui/project_bench.png")
    val guiAutoCrafter = registerPR("textures/gui/auto_bench.png")
    val panelCraftExtension = registerPR("textures/gui/craft_chip_ext_panel.png")

    val icmaptex = registerPR("textures/gui/map_background.png")
    val guiBlockPlacer = registerPR("textures/gui/placer.png")
    val guiFilteredImporter = registerPR("textures/gui/filtered_importer.png")
    val guiICPrinter = registerPR("textures/gui/ic_printer.png")
    val guiBatteryBox = registerPR("textures/gui/battery_box.png")
    val guiElectrotineGenerator = registerPR("textures/gui/electrotine_generator.png")
    val guiCharger = registerPR("textures/gui/charger.png")

    def register(path:String) = new ResourceAction(new ResourceLocation(path))
    def registerPR(path:String) = new ResourceAction(new ResourceLocation("projectred", path))
}