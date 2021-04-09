//package mrtjp.projectred.expansion
//
//import codechicken.lib.data.{MCDataInput, MCDataOutput}
//import codechicken.lib.packet.ICustomPacketHandler.{IClientPacketHandler, IServerPacketHandler}
//import codechicken.lib.packet.{PacketCustom, PacketCustomChannelBuilder}
//import codechicken.lib.util.CrashLock
//import codechicken.multipart.api.part.TMultiPart
//import codechicken.multipart.block.BlockMultiPart
//import mrtjp.projectred.ProjectRedExpansion
//import mrtjp.projectred.core.CoreTile
//import mrtjp.projectred.integration.{GatePart, GuiCounter, GuiTimer, ICounterGuiLogic, ITimerGuiLogic}
//import net.minecraft.client.Minecraft
//import net.minecraft.client.network.play.IClientPlayNetHandler
//import net.minecraft.entity.player.ServerPlayerEntity
//import net.minecraft.network.play.IServerPlayNetHandler
//import net.minecraft.util.ResourceLocation
//import net.minecraft.world.World
//import ExpansionNetwork._

//class ExpansionPH
//{
//    val channel = ProjectRedExpansion
//    val jetpack_state = 1
//}
//
//object ExpansionCPH extends ExpansionPH with IClientPacketHandler
//{
//    def handlePacket(packet:PacketCustom, mc:Minecraft, nethandler:INetHandlerPlayClient)
//    {
//        packet.getType match
//        {
//            case `jetpack_state` =>
//                ItemJetpack.setStateOfEntity(packet.readInt(), packet.readBoolean(), false)
//        }
//    }
//
//    def openMachineGui(packet:PacketCustom, mc:Minecraft)
//    {
//    }
//}
//
//object ExpansionSPH extends ExpansionPH with IServerPacketHandler
//{
//    def handlePacket(packet:PacketCustom, sender:EntityPlayerMP, nethandler:INetHandlerPlayServer)
//    {
//    }
//}

//object ExpansionNetwork
//{
//    private val LOCK = new CrashLock("Already initialized.")
//    val NET_CHANNEL = new ResourceLocation(ProjectRedExpansion.MOD_ID, "network")
//
//
//
//    //    val C_TILE_UPDATE = 1
//    //    val C_ADD_MESSAGE = 2
//    //    val C_GUI_PACKET = 3
//    //
//    //    val S_TILE_UPDATE = 1
//    //    val S_KEY_UPDATE = 2
//
//    // Server to client messages
//     val TILE_UPDATE_FROM_SERVER = 1
////    val OPEN_TIMER_GUI_FROM_SERVER = 1
////    val OPEN_COUNTER_GUI_FROM_SERVER = 2
//
//    // Client to server messages
//    val TILE_UPDATE_FROM_CLIENT = 1
////    val INCR_TIMER_FROM_CLIENT = 3
////    val INCR_COUNTER_FROM_CLIENT = 4
//
//    def init() {
//        LOCK.lock()
//        PacketCustomChannelBuilder.named(NET_CHANNEL)
//                .assignClientHandler(() => () => ClientHandler)
//                .assignServerHandler(() => () => ServerHandler)
//                .build()
//    }
//
//    def writePartIndex(out:MCDataOutput, part:TMultiPart) = {
//        out.writePos(part.pos).writeByte(part.tile.getPartList.indexOf(part))
//    }
//
//    def readPartIndex(world:World, in:MCDataInput) = {
//        val tile = BlockMultiPart.getTile(world, in.readPos())//PRLib.getMultipartTile(world, in.readCoord)
//        try {
//            tile.getPartList.get(in.readUByte)
//        }
//        catch {
//            case e:NullPointerException => null
//            case e:IndexOutOfBoundsException => null
//        }
//    }
//
//    def createUpdatePacket(tile:CoreTile):PacketCustom = {
//        val packet = new PacketCustom(NET_CHANNEL,
//            if (!tile.getWorld.isRemote) TILE_UPDATE_FROM_SERVER else TILE_UPDATE_FROM_CLIENT)
//        packet.writePos(tile.getPos)
//        packet
//    }
//
//    private[expansion] def handleTileUpdate(world:World, input:MCDataInput):Unit = {
//        val tile = world.getTileEntity(input.readPos())
//        if (tile != null && tile.isInstanceOf[CoreTile])
//            tile.asInstanceOf[CoreTile].handleRecievedPacket(input)
//    }
//
//    //    def openTimerGui(player:PlayerEntity, gate:GatePart):Unit = {
//    //        val packet = new PacketCustom(NET_CHANNEL, OPEN_TIMER_GUI_FROM_SERVER)
//    //        writePartIndex(packet, gate)
//    //        packet.sendToPlayer(player.asInstanceOf[ServerPlayerEntity])
//    //    }
//    //
//    //    def openCounterGui(player:PlayerEntity, gate:GatePart):Unit = {
//    //        val packet = new PacketCustom(NET_CHANNEL, OPEN_COUNTER_GUI_FROM_SERVER)
//    //        writePartIndex(packet, gate)
//    //        packet.sendToPlayer(player.asInstanceOf[ServerPlayerEntity])
//    //    }
//}
//
//private object ClientHandler extends IClientPacketHandler
//{
//    override def handlePacket(packet: PacketCustom, mc: Minecraft, handler: IClientPlayNetHandler):Unit = {
//        packet.getType match {
//            case TILE_UPDATE_FROM_SERVER => handleTileUpdate(mc.world, packet)
//            case _ =>
//        }
//    }
//}
//
//private object ServerHandler extends IServerPacketHandler
//{
//    override def handlePacket(packet: PacketCustom, sender: ServerPlayerEntity, handler: IServerPlayNetHandler):Unit = {
//        packet.getType match  {
//            case TILE_UPDATE_FROM_CLIENT => handleTileUpdate(sender.world, packet)
//            case _ =>
//        }
//    }
//}