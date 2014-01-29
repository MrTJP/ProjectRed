package mrtjp.projectred.expansion

import net.minecraft.client.gui.inventory.GuiContainer

object MachineGuiFactory
{
    val id_furnace = 0

    def createGui(id:Int, tile:TileMachineIO):GuiContainer = id match
    {
        case MachineGuiFactory.id_furnace => null
        case _ => null
    }

    def apply(id:Int, tile:TileMachineIO) = createGui(id, tile)
}