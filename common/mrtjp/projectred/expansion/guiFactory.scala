package mrtjp.projectred.expansion

import codechicken.lib.render._
import codechicken.lib.vec.{Rotation, Translation, Cuboid6}
import mrtjp.projectred.core.PRColors
import mrtjp.projectred.core.inventory._
import net.minecraft.client.Minecraft
import net.minecraft.client.gui.inventory.GuiContainer
import net.minecraft.inventory.Container
import net.minecraft.util.ResourceLocation
import org.lwjgl.opengl.GL11
import scala.collection.mutable.ListBuffer

object MachineGuiFactory
{
    val mc = Minecraft.getMinecraft

    val id_furnace = 0

    def createGui(id:Int, tile:TileGuiMachine):GuiContainer = id match
    {
        case this.id_furnace => new GuiFurnace(tile.asInstanceOf[TileFurnace], tile.createContainer(mc.thePlayer))
        case _ => null
    }

    def apply(id:Int, tile:TileGuiMachine) = createGui(id, tile)
}

abstract class TabSideConfig(c:Int, tile:TileMachineSideConfig) extends WidgetTab(c, 20, 20, 100, 100)
{
    object SideConfigSS
    {
        val sides = CCModel.quadModel(6*4).generateBlock(0, Cuboid6.full.copy().expand(0.0001D))
        sides.computeNormals()
    }

    class SideConfigSS extends WidgetSideSelect(30, 25, 40, 40, 40) with TWidgetSideTE
    {
        val stile = TabSideConfig.this.tile

        override def buildToolTip(list:ListBuffer[String], hoverSide:Int) = if (hoverSide > -1)
        {
            if (hoverSide != Rotation.rotateSide(stile.side, stile.rotation))
                list += stile.sideInfo(stile.sideConfig(hoverSide))
        }

        override def draw()
        {
            super.draw()
            PRColors.WHITE.setGL11Color(1)
            TextureUtils.bindAtlas(0)

            CCRenderState.reset()

            CCRenderState.startDrawing(7)
            tessellateTile(stile.side, stile.rotation)
            CCRenderState.draw()

            for (s <- 0 until 6) if (s != Rotation.rotateSide(stile.side, stile.rotation))
            {
                CCRenderState.setColourOpaque(PRColors.get(stile.sideConfig(s)).rgb)
                CCRenderState.startDrawing(7)
                val s2 = SideConfigSS.sides.copy()
                s2.shrinkUVs(0.0005D)
                s2.apply(new Translation(-0.5, -0.5, -0.5))
                s2.apply(new IconTransformation(BlockMachine.iconIO))
                s2.render(s*4, 4, null, null, ColourModifier.instance)
                CCRenderState.draw()
            }
        }

        override def onSideClicked(s:Int, button:Int) = button match
        {
            case 0 => stile.sideUp(s)
            case 1 => stile.sideDown(s)
        }
    }

    val ss = new SideConfigSS
    ss.setTile(tile)
    add(ss)

    override def drawTab()
    {
    }

    override def drawIcon()
    {
        CCRenderState.changeTexture(GhostWidget.guiExtras)
        GL11.glColor4d(1, 1, 1, 1)
        drawTexturedModalRect(2, 2, 21, 1, 16, 16)
    }

    override def buildToolTip(list:ListBuffer[String])
    {
        super.buildToolTip(list)
        list += "Configure IO"
    }

    def tessellateTile(s:Int, r:Int)
}

abstract class GuiMachineWorking(tile:TileMachineWorking, cont:Container) extends GhostGuiContainer(cont, null)
{
    val controller = new WidgetTabController(xSize, 0)
    override def addWidgets()
    {
        controller.add(new TabSideConfig(PRColors.LIGHT_BLUE.rgba, tile)
        {
            override def tessellateTile(s:Int, r:Int)
            {
                drawTileForTab(s, r)
            }
        })
        add(controller)
    }

    def drawTileForTab(s:Int, r:Int)

    def drawChargeTank()
    {
        CCRenderState.changeTexture(GhostWidget.guiExtras)
        drawTexturedModalRect(7, 13, 1, 150, 7, 57) //chargebar
        if (tile.cond.canWork) drawTexturedModalRect(9, 13, 20, 150, 3, 6)

        val c = tile.cond.getChargeScaled(48)
        drawTexturedModalRect(8, 69-c, 19, 206-c, 5, c)
    }
    def drawFlowTank()
    {
        CCRenderState.changeTexture(GhostWidget.guiExtras)
        drawTexturedModalRect(14, 13, 10, 150, 7, 57) //flowbar
        if (tile.cond.flow == -1) drawTexturedModalRect(16, 13, 27, 150, 3, 6)

        val f = tile.cond.getFlowScaled(48)
        drawTexturedModalRect(15, 69-f, 26, 206-f, 5, f)
    }
}

class GuiFurnace(tile:TileFurnace, cont:Container) extends GuiMachineWorking(tile, cont)
{

    override def drawTileForTab(s:Int, r:Int)
    {
        RenderFurnace.renderForSideSelect(r)
    }

    override def drawBackground()
    {
        super.drawBackground()
        CCRenderState.changeTexture(GuiFurnace.resource)
        drawTexturedModalRect(0, 0, 0, 0, xSize, ySize)

        val s = tile.progressScaled(24)
        drawTexturedModalRect(67, 37, 176, 0, s+1, 16)

        drawChargeTank()
        drawFlowTank()
    }
}

object GuiFurnace
{
    val resource = new ResourceLocation("projectred:textures/gui/furnace.png")
}