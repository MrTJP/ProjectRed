package mrtjp.projectred.expansion

import mrtjp.core.gui.{WidgetGui, WidgetTabControl}
import mrtjp.core.resource.ResourceLib
import mrtjp.core.vec.Point
import mrtjp.projectred.core.libmc.PRResources
import net.minecraft.client.Minecraft
import net.minecraft.client.gui.inventory.GuiContainer
import net.minecraft.inventory.Container

object MachineGuiFactory
{
    val mc = Minecraft.getMinecraft

    val id_furnace = 1

    def createGui(id:Int, tile:TileGuiMachine):GuiContainer = id match
    {
        case this.id_furnace => new GuiFurnace(tile.asInstanceOf[TileFurnace], tile.createContainer(mc.thePlayer))
        case _ => null
    }

    def apply(id:Int, tile:TileGuiMachine) = createGui(id, tile)
}

abstract class GuiMachineWorking(tile:TileMachineWorking, cont:Container) extends WidgetGui(cont)
{
    val controller = new WidgetTabControl(xSize, 0)
    override def runInit_Impl()
    {
        add(controller)
    }

    def drawChargeTank()
    {
        ResourceLib.guiExtras.bind()
        drawTexturedModalRect(7, 13, 1, 150, 7, 57) //chargebar
        if (tile.cond.canWork) drawTexturedModalRect(9, 13, 20, 150, 3, 6)

        val c = tile.cond.getChargeScaled(48)
        drawTexturedModalRect(8, 69-c, 19, 206-c, 5, c)
    }
    def drawFlowTank()
    {
        ResourceLib.guiExtras.bind()
        drawTexturedModalRect(14, 13, 10, 150, 7, 57) //flowbar
        if (tile.cond.flow == -1) drawTexturedModalRect(16, 13, 27, 150, 3, 6)

        val f = tile.cond.getFlowScaled(48)
        drawTexturedModalRect(15, 69-f, 26, 206-f, 5, f)
    }
}

class GuiFurnace(tile:TileFurnace, cont:Container) extends GuiMachineWorking(tile, cont)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        PRResources.guiFurnace.bind()
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize)

        val s = tile.progressScaled(24)
        drawTexturedModalRect(67, 37, 176, 0, s+1, 16)

        drawChargeTank()
        drawFlowTank()
    }
}

object GuiFurnace
{
}