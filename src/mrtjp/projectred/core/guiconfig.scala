package mrtjp.projectred.core

import java.util.{ArrayList => JAList}

import cpw.mods.fml.client.IModGuiFactory
import cpw.mods.fml.client.IModGuiFactory.RuntimeOptionCategoryElement
import cpw.mods.fml.client.config.DummyConfigElement.DummyCategoryElement
import cpw.mods.fml.client.config.{GuiConfig, IConfigElement}
import net.minecraft.client.Minecraft
import net.minecraft.client.gui.GuiScreen
import net.minecraftforge.common.config.ConfigElement

class GuiConfigFactory extends IModGuiFactory
{
    override def initialize(minecraftInstance: Minecraft){}

    override def mainConfigGuiClass() = classOf[ProjectRedConfigGUI]

    override def runtimeGuiCategories() = null

    override def getHandlerFor(element: RuntimeOptionCategoryElement) = null
}

class ProjectRedConfigGUI(parent: GuiScreen) extends GuiConfig(parent, ProjectRedConfigGUI.getElements,
    "ProjRed|Core", false, false, GuiConfig.getAbridgedConfigPath(Configurator.config.toString))
{
}

object ProjectRedConfigGUI
{
    // Modified DummyCategoryElement that uses comments from config
    private class PRDummyCategoryElement(catName: String) extends DummyCategoryElement(catName, "", new ConfigElement(Configurator.config.getCategory(catName.toLowerCase)).getChildElements)
    {
        override def getComment = Configurator.config.getCategory(catName.toLowerCase).getComment
    }

    def getElements =
    {
        val list = new JAList[IConfigElement[_]]
        list.add(new PRDummyCategoryElement("General"))
        list.add(new PRDummyCategoryElement("World Generation"))
        list
    }
}