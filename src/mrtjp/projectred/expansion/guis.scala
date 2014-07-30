package mrtjp.projectred.expansion

import codechicken.lib.data.MCDataInput
import cpw.mods.fml.relauncher.{Side, SideOnly}
import mrtjp.projectred.core.libmc.{PRLib, ResourceLib}
import mrtjp.projectred.core.libmc.gui._
import mrtjp.projectred.core.{GuiIDs, TGuiBuilder}
import net.minecraft.client.Minecraft
import net.minecraft.client.gui.inventory.GuiContainer
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.Container

object MachineGuiFactory
{
    val mc = Minecraft.getMinecraft

    val id_controller = 0
    val id_furnace = 1

    def createGui(id:Int, tile:TileGuiMachine):GuiContainer = id match
    {
        case this.id_controller => new GuiRouterController(tile.asInstanceOf[TileRouterController], tile.createContainer(mc.thePlayer))
        case this.id_furnace => new GuiFurnace(tile.asInstanceOf[TileFurnace], tile.createContainer(mc.thePlayer))
        case _ => null
    }

    def apply(id:Int, tile:TileGuiMachine) = createGui(id, tile)
}

class GuiRouterController(tile:TileRouterController, container:Container) extends WidgetGui(container)
{
    override def drawBack_Impl(mouse:Point, frame:Float)
    {
        ResourceLib.guiRouterControl.bind()
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize)
    }
}

object GuiRouterController extends TGuiBuilder
{
    override def getID = GuiIDs.routingController

    @SideOnly(Side.CLIENT)
    override def buildGui(player:EntityPlayer, data:MCDataInput) =
    {
        val tile = PRLib.getTileEntity(player.worldObj, data.readCoord(), classOf[TileRouterController])
        if (tile != null) new GuiRouterController(tile, tile.createContainer(player))
        else null
    }
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
        ResourceLib.guiFurnace.bind()
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