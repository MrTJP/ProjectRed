/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.data

import codechicken.lib.packet.PacketCustom
import mrtjp.core.handler.MrTJPCoreNetwork
import mrtjp.core.handler.MrTJPCoreNetwork.{NET_CHANNEL, S_KEY_UPDATE}
import net.minecraft.client.Minecraft
import net.minecraft.client.settings.KeyBinding
import net.minecraft.entity.player.PlayerEntity
import net.minecraftforge.api.distmarker.{Dist, OnlyIn}
import net.minecraftforge.common.MinecraftForge
import net.minecraftforge.event.TickEvent.ClientTickEvent
import net.minecraftforge.eventbus.api.SubscribeEvent
import net.minecraftforge.fml.client.registry.ClientRegistry

import scala.collection.mutable.{HashMap => MHashMap, Map => MMap, WeakHashMap => MWeakHashMap}

object KeyTracking
{
    private var idPool = 0
    private val map = MHashMap[Int, MMap[PlayerEntity, Boolean]]()

    def updatePlayerKey(id:Int, player:PlayerEntity, state:Boolean)
    {
        map(id) += player -> state
    }

    def registerTracker(tracker:TServerKeyTracker)
    {
        tracker.id = idPool
        idPool += 1
        map.getOrElseUpdate(tracker.id,
            MWeakHashMap[PlayerEntity, Boolean]().withDefaultValue(false))
    }

    def isKeyDown(id:Int, player:PlayerEntity) = map(id)(player)
}

trait TServerKeyTracker
{
    var id = -1

    def isKeyDown(p:PlayerEntity) = KeyTracking.isKeyDown(id, p)

    def register()
    {
        KeyTracking.registerTracker(this)
    }
}

trait TClientKeyTracker
{
    private var wasDown = false

    def getTracker:TServerKeyTracker

    def getIsKeyDown:Boolean

    @SubscribeEvent
    @OnlyIn(Dist.CLIENT)
    def tick(event:ClientTickEvent) {
        val down = getIsKeyDown
        if (down != wasDown) {
            wasDown = down
            if (Minecraft.getInstance.getConnection != null) {
                KeyTracking.updatePlayerKey(getTracker.id, Minecraft.getInstance.player, down)
                val packet = new PacketCustom(NET_CHANNEL, S_KEY_UPDATE)
                packet.writeByte(getTracker.id)
                packet.writeBoolean(down)
                packet.sendToServer()
            }
        }
    }

    @OnlyIn(Dist.CLIENT)
    def register() {
        MinecraftForge.EVENT_BUS.register(this)
        this match {
            case kb:KeyBinding => ClientRegistry.registerKeyBinding(kb)
            case _ =>
        }
    }
}
