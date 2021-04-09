/*
/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.gui

import codechicken.lib.data.{MCDataInput, MCDataOutput}
import codechicken.lib.packet.PacketCustom
import mrtjp.core.handler.MrTJPCoreSPH
import net.minecraft.client.Minecraft
import net.minecraft.client.gui.GuiScreen
import net.minecraft.entity.player.{EntityPlayer, EntityPlayerMP}
import net.minecraft.inventory.Container
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

/**
  * Alternate way to handle GUIs instead of the built in IGuiHandler system. Advantages include simplicity and being
  * able to send custom data along with the gui open request.
  *
  * == Creating a Factory ==
  * To use the Gui system, you first need a Gui Factory. These are implementations of the [[TGuiFactory]] trait. This
  * factory needs to be registered with the [[GuiHandler.register()]] method during game initialization.
  *
  * {{{
  * import mrtjp.core.gui._
  *
  * object MyGuiFactory extends TGuiFactory { ... }
  *
  * GuiHandler.register(MyGuiFactory) //register the factory
  * }}}
  *
  * == Opening a GUI ==
  * Once you create and register a GUI factory, opening it is straightforward. You simply use one of the the available
  * [[TGuiFactory.open()]] functions in the factory server-side. You can optionally pass in a data writer function with
  * which you can write custom data.
  *
  * Server-side call to [[TGuiFactory.open() factory open function]] by you:
  * {{{
  * import mrtjp.core.gui._
  *
  * val dataWrite:MCDataOutput => Unit = { out:MCDataOutput =>
  *     out.writeBoolean(machine.isPowered)
  *     out.writeInt(machine.progress)
  * }
  *
  * MyGuiFactory.open(player, container, dataWrite) //server-side open call
  * }}}
  *
  *
  * The handler backend will send this open request to the client. The [[TGuiFactory.buildGui()]] function will
  * be called client-side with `data` parameter containing data written during the [[TGuiFactory.open()]] call on
  * the server.
  *
  * Client-side call to [[TGuiFactory.buildGui() GUI build function]] by the handler backend:
  * {{{
  * import mrtjp.core.gui._
  *
  * object MyGuiFactory extends TGuiFactory {
  *
  *     //...
  *
  *     def buildGui(player:EntityPlayer, data:MCDataInput):GuiScreen = {
  *         val myGui = new MyGui()
  *
  *         //construct your gui using the custom data
  *         myGui.isPowered = data.readBoolean()
  *         myGui.progress = data.readInt()
  *
  *         return myGui //This gui will be opened by the handler
  *     }
  * }
  *
  * }}}
  *
  */
object GuiHandler
{
    private var guiMap = Map[Int, TGuiFactory]()

    /**
      * Called server-side to open a synchronized container GUI
      *
      * @param player1 Server-side player
      * @param cont Server-side container
      * @param guiID Gui ID defined in GuiIDs object
      * @param dataWrite Partial function that adds data to a packet
      */
    private[gui] def openSMPContainer(player1:EntityPlayer, cont:Container, guiID:Int, dataWrite:MCDataOutput => Unit)
    {
        if (!player1.isInstanceOf[EntityPlayerMP]) return
        val player = player1.asInstanceOf[EntityPlayerMP]
        player.getNextWindowId()
        player.closeContainer()
        val packet = new PacketCustom(MrTJPCoreSPH.channel, MrTJPCoreSPH.guiPacket)
        dataWrite(packet.writeByte(player.currentWindowId).writeShort(guiID))
        packet.sendToPlayer(player)
        if (cont != null)
        {
            player.openContainer = cont
            player.openContainer.windowId = player.currentWindowId
            player.openContainer.addListener(player)
        }
    }

    /**
      * Called client side upon receiving a request to open a gui. Server requests client
      * to do so with the above method.
      *
      * @param windowID The window ID the server defined to client
      * @param gui The gui created by the client
      */
    @SideOnly(Side.CLIENT)
    private[gui] def openSMPContainer(windowID:Int, gui:GuiScreen)
    {
        val mc = Minecraft.getMinecraft
        mc.displayGuiScreen(gui)
        if (windowID != 0) mc.player.openContainer.windowId = windowID
    }

    /**
      * Internally called by the client when it receives a gui request from the
      * server with openSMPContainer.
      *
      * @param data Raw data built by the server, includes windowID as byte and
      *             guiID as short, as well as custom gui data.
      */
    @SideOnly(Side.CLIENT)
    private[core] def receiveGuiPacket(data:MCDataInput)
    {
        val win = data.readUByte()
        val id = data.readUShort()
        val gui = guiMap.get(id) match
        {
            case Some(e) => e.buildGui(Minecraft.getMinecraft.player, data)
            case None => null
        }
        if (gui != null) openSMPContainer(win, gui)
    }

    @deprecated("register(factory:TGuiFactory)")
    def register(factory:TGuiFactory, id:Int) {
        assert(factory.getID == id)
        register(factory)
    }

    /**
      * Called to register a [[TGuiFactory GUI factory]] to the handler. The factory must be registered on both
      * server and client during game initialization.
      *
      * @param factory The factory to register
      * @throws RuntimeException If a factory with the same [[TGuiFactory.getID ID]] is already registered.
      */
    def register(factory:TGuiFactory) {

        if (guiMap.contains(factory.getID))
            throw new RuntimeException(s"There is a factory already registered with ID ${factory.getID}.")
        guiMap += factory.getID -> factory
    }
}

/**
  * The base trait for a GUI factory. Each GUI will have its own factory. Factories are responsible for sending
  * open requests from the server to the client. The factory then creates the GUI based on the data recieved.
  * Register this with the [[GuiHandler.register() handler register function]].
  */
trait TGuiFactory
{
    /**
      * Globally unique ID for this GUI.
      *
      * @todo This needs to be a string to make it easier for mods to not have conflicting IDs.
      */
    def getID:Int

    /**
      * Called client side after the server sends an open request for this specific GUI.
      *
      * @param player The local player.
      * @param data Data that was written on the server as part of the open request.
      * @return The GUI constructed with the provided data. The handler will present it.
      */
    @SideOnly(Side.CLIENT)
    def buildGui(player:EntityPlayer, data:MCDataInput):GuiScreen

    /**
      * Server-side function that will send an open request to the client. This will trigger a call to this factory's
      * [[buildGui() gui build function]] on `player`'s client.
      *
      * Use [[open(player:EntityPlayer, cont:Container, dataWrite:MCDataOutput => Unit)]] if you need to add custom
      * data to the open request.
      *
      * @param player The player to send the open request to.
      * @param cont The container for this GUI, if it has one. Can be null if none.
      */
    final def open(player:EntityPlayer, cont:Container) { open(player, cont, {_ => }) }

    /**
      * Server-side function that will send an open request to the client. This will trigger a call to this factory's
      * [[buildGui() gui build function]] on `player`'s client.
      *
      * @param player The player to send the open request to.
      * @param cont The container for this GUI, if it has one. Can be null if none.
      * @param dataWrite A function to write custom data into this open request. It will be passed to the client's
      *                  [[buildGui() GUI build function]].
      */
    final def open(player:EntityPlayer, cont:Container, dataWrite:MCDataOutput => Unit)
    {
        GuiHandler.openSMPContainer(player, cont, getID, dataWrite)
    }
}*/
