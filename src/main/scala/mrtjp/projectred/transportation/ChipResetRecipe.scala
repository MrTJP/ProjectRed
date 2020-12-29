package mrtjp.projectred.transportation

import java.lang.{Character => JC}

import mrtjp.projectred.transportation.RoutingChipDefs.ChipVal
import net.minecraft.inventory.InventoryCrafting
import net.minecraft.item.ItemStack
import net.minecraft.item.crafting.IRecipe
import net.minecraft.world.World
import net.minecraftforge.event.RegistryEvent
import net.minecraftforge.fml.common.eventhandler.SubscribeEvent
import net.minecraftforge.registries.IForgeRegistryEntry

class ChipResetRecipe extends IForgeRegistryEntry.Impl[IRecipe] with IRecipe
{
    override def matches(inv:InventoryCrafting, world:World) = !getCraftingResult(inv).isEmpty

    override def getCraftingResult(inv:InventoryCrafting):ItemStack =
    {
        val cdef = getType(inv)
        if (cdef != null) if (isTypeExclusive(cdef, inv)) return cdef.makeStack(countUnits(inv))
        ItemStack.EMPTY
    }

    def getType(inv:InventoryCrafting):ChipVal =
    {
        for (i <- 0 until inv.getSizeInventory)
        {
            val cdef = RoutingChipDefs.getForStack(inv.getStackInSlot(i))
            if (cdef != null) return cdef
        }
        null
    }

    def isTypeExclusive(cdef:ChipVal, inv:InventoryCrafting):Boolean =
    {
        for (i <- 0 until inv.getSizeInventory)
        {
            val stack = inv.getStackInSlot(i)
            if (!stack.isEmpty && !stack.getItem.isInstanceOf[ItemRoutingChip]) return false
            val type2 = RoutingChipDefs.getForStack(stack)
            if (type2 != null && !(type2 == cdef)) return false
        }
        true
    }

    def countUnits(inv:InventoryCrafting):Int =
    {
        var count = 0
        for (i <- 0 until inv.getSizeInventory)
            if (!inv.getStackInSlot(i).isEmpty) count += 1
        count
    }

    override def canFit(width: Int, height: Int) = width * height >= 2
    def getRecipeOutput = ItemStack.EMPTY
    override def isDynamic: Boolean = true
}

object ChipResetRecipe
{
    @SubscribeEvent
    def registerRecipes(event: RegistryEvent.Register[IRecipe])
    {
        event.getRegistry.register(new ChipResetRecipe().setRegistryName("chip_reset"))
    }
}
