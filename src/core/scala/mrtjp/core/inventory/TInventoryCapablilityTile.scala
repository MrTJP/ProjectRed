package mrtjp.core.inventory

import net.minecraft.inventory.{IInventory, ISidedInventory}
import net.minecraft.tileentity.TileEntity
import net.minecraft.util.Direction
import net.minecraftforge.common.capabilities.Capability
import net.minecraftforge.common.util.LazyOptional
import net.minecraftforge.items.CapabilityItemHandler._
import net.minecraftforge.items.IItemHandler
import net.minecraftforge.items.wrapper.{EmptyHandler, SidedInvWrapper, InvWrapper => MCFInvWrapper}

import java.util.{HashMap => JHashMap}

trait TInventoryCapablilityTile extends TileEntity with IInventory {

    private val globalCap: LazyOptional[IItemHandler] = LazyOptional.of(() => new MCFInvWrapper(this))
    private val sideMap = new JHashMap[Direction, LazyOptional[_]]()

    override def getCapability[T](cap: Capability[T], side: Direction): LazyOptional[T] =
        if (cap == ITEM_HANDLER_CAPABILITY) {
            if (side == null) {
                return globalCap.cast()
            }
            sideMap.computeIfAbsent(side, s => {
                LazyOptional.of(() => this match {
                    case inv: ISidedInventory =>
                        new SidedInvWrapper(inv, side)
                    case _ =>
                        EmptyHandler.INSTANCE
                })
            }).cast()

        } else {
            super.getCapability(cap, side)
        }
}
