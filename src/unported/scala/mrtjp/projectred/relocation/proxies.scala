/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.relocation

import java.util.concurrent.Callable

import codechicken.lib.packet.PacketCustom
import codechicken.multipart.{MultiPartRegistry, MultipartGenerator}
import mrtjp.projectred.ProjectRedRelocation._
import mrtjp.projectred.api.ProjectRedAPI.{relocationAPI => API}
import mrtjp.projectred.api.{IConditionallyMovable, IFrame}
import net.minecraft.nbt.NBTBase
import net.minecraft.util.math.BlockPos
import net.minecraft.util.{EnumFacing, ResourceLocation}
import net.minecraft.world.World
import net.minecraftforge.common.MinecraftForge
import net.minecraftforge.common.capabilities.Capability.IStorage
import net.minecraftforge.common.capabilities.{Capability, CapabilityManager}
import net.minecraftforge.fml.common.registry.ForgeRegistries
import net.minecraftforge.fml.relauncher.{Side, SideOnly}

class RelocationProxy_server
{
    def preinit()
    {
        API.registerTileMover("saveload",
            "Saves the tile and then reloads it in the next position. Reliable but CPU intensive.",
            SaveLoadTileMover)

        API.registerTileMover("coordpush",
            "Physically changes the location of tiles. Works if tiles do not cache their position.",
            CoordPushTileMover)

        API.registerTileMover("static", "Setting this disables movement for the specified block.", StaticTileMover)

        API.registerTileMover("fmp", "Tile mover for Forge Multipart", FMPTileHandler)

        API.registerPreferredMover("default", "saveload")
        API.registerPreferredMover("mod:minecraft", "coordpush")
        API.registerPreferredMover("mod:computercraft", "coordpush")
        API.registerPreferredMover("mod:enderstorage", "coordpush")
        API.registerPreferredMover("mod:chickenchunks", "coordpush")
        API.registerPreferredMover("mod:projectred-compatibility", "coordpush")
        API.registerPreferredMover("mod:projectred-core", "coordpush")
        API.registerPreferredMover("mod:projectred-expansion", "coordpush")
        API.registerPreferredMover("mod:projectred-exploration", "coordpush")
        API.registerPreferredMover("mod:projectred-fabrication", "coordpush")
        API.registerPreferredMover("mod:projectred-illumination", "coordpush")
        API.registerPreferredMover("mod:projectred-integration", "coordpush")
        API.registerPreferredMover("mod:projectred-transmission", "coordpush")
        API.registerPreferredMover("mod:projectred-relocation", "coordpush")
        API.registerPreferredMover("mod:projectred-transportation", "coordpush")

        API.registerMandatoryMover("mod:forgemultipartcbe", "fmp")

        MultipartGenerator.registerPassThroughInterface("mrtjp.projectred.api.IFrame")

        /** Initialization **/
        blockMovingRow = new BlockMovingRow
        blockFrame = new BlockFrame

        /** Localiztion **/
        blockFrame.setUnlocalizedName("projectred.relocation.frame")

        /** Registration **/
        ForgeRegistries.BLOCKS.register(blockMovingRow.setRegistryName("blockMovingRow"))
        ForgeRegistries.BLOCKS.register(blockFrame.setRegistryName("frame"))
        ForgeRegistries.ITEMS.register(new ItemBlockFrame(blockFrame).setRegistryName(blockFrame.getRegistryName))

        blockMovingRow.addTile(classOf[TileMovingRow], 0)
    }

    def init()
    {
        MultiPartRegistry.registerParts((_:ResourceLocation, _:Boolean) => new FramePart, Array(FramePart.partType))
        MultiPartRegistry.registerConverter(FrameBlockConverter)

        CapabilityManager.INSTANCE.register(classOf[IFrame], new IStorage[IFrame] {
            override def writeNBT(capability:Capability[IFrame], instance:IFrame, side:EnumFacing):NBTBase = null

            override def readNBT(capability:Capability[IFrame], instance:IFrame, side:EnumFacing, nbt:NBTBase){}
        }, new Callable[IFrame]{
            override def call():IFrame = new IFrame {
                override def stickIn(w:World, pos:BlockPos, side:EnumFacing) = false
                override def stickOut(w:World, pos:BlockPos, side:EnumFacing) = false
            }
        })

        CapabilityManager.INSTANCE.register(classOf[IConditionallyMovable], new IStorage[IConditionallyMovable] {
            override def writeNBT(capability:Capability[IConditionallyMovable], instance:IConditionallyMovable, side:EnumFacing):NBTBase = null
            override def readNBT(capability:Capability[IConditionallyMovable], instance:IConditionallyMovable, side:EnumFacing, nbt:NBTBase){}
        }, new Callable[IConditionallyMovable]{
            override def call():IConditionallyMovable = new IConditionallyMovable {
                override def isMovable(w:World, pos:BlockPos) = true
            }
        })
    }

    def postinit()
    {
        MinecraftForge.EVENT_BUS.register(RelocationEventHandler)
        PacketCustom.assignHandler(RelocationSPH.channel, RelocationSPH)
    }
}

class RelocationProxy_client extends RelocationProxy_server
{
    @SideOnly(Side.CLIENT)
    override def preinit()
    {
        super.preinit()

        MinecraftForge.EVENT_BUS.register(this)
        FrameRenderer.init()
    }

    @SideOnly(Side.CLIENT)
    override def init()
    {
        super.init()
        MovingRenderer.init()
    }

    @SideOnly(Side.CLIENT)
    override def postinit()
    {
        super.postinit()

        MinecraftForge.EVENT_BUS.register(RelocationClientEventHandler)
        PacketCustom.assignHandler(RelocationCPH.channel, RelocationCPH)
    }
}

object RelocationProxy extends RelocationProxy_client