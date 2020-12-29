/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.relocation

import net.minecraftforge.client.event.RenderWorldLastEvent
import net.minecraftforge.event.world.{ChunkWatchEvent, WorldEvent}
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent
import net.minecraftforge.fml.common.gameevent.TickEvent
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

object RelocationEventHandler
{
    @SubscribeEvent
    def worldUnload(event:WorldEvent.Unload)
    {
        RelocationSPH.onWorldUnload(event.getWorld)
        MovementManager.onWorldUnload(event.getWorld)
    }

    @SubscribeEvent
    def chunkWatch(event:ChunkWatchEvent.Watch)
    {
        RelocationSPH.onChunkWatch(event.getPlayer, event.getChunk)
    }

    @SubscribeEvent
    def chunkUnwatch(event:ChunkWatchEvent.UnWatch)
    {
        RelocationSPH.onChunkUnWatch(event.getPlayer, event.getChunk)
    }

    @SubscribeEvent
    def serverTick(event:TickEvent.ServerTickEvent)
    {
        if (event.phase == TickEvent.Phase.END) {
            RelocationSPH.onTickEnd()
            MovementManager.onTick(false)
        }
    }

    @SubscribeEvent
    def worldTick(event:TickEvent.WorldTickEvent)
    {
        //    if (event.side.isServer && event.phase == TickEvent.Phase.END) {
        //      RelocationSPH.onTickEnd()
        //      MovementManager2.onTick(false)
        //    }
    }
}

object RelocationClientEventHandler
{
    @SubscribeEvent
    @SideOnly(Side.CLIENT)
    def onRenderTick(e:TickEvent.RenderTickEvent)
    {
        //    if (e.phase == TickEvent.Phase.START)
        //      MovingRenderer.onPreRenderTick(e.renderTickTime)
        //    else MovingRenderer.onPostRenderTick()
    }

    @SubscribeEvent
    @SideOnly(Side.CLIENT)
    def onRenderWorld(e:RenderWorldLastEvent)
    {
        MovingRenderer.onPreRenderTick(e.getPartialTicks)
        MovingRenderer.onRenderWorldEvent()
        MovingRenderer.onPostRenderTick()
    }

    @SubscribeEvent
    @SideOnly(Side.CLIENT)
    def clientTick(event:TickEvent.ClientTickEvent)
    {
        if (event.phase == TickEvent.Phase.END)
            MovementManager.onTick(true) //TODO call from better place. This moves blocks even when client is paused
    }

    @SubscribeEvent
    @SideOnly(Side.CLIENT)
    def worldTick(event:TickEvent.WorldTickEvent)
    {
        //    if (event.phase == TickEvent.Phase.END)
        //      MovementManager2.onTick(true)
    }
}