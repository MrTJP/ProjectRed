package mrtjp.projectred.expansion

import mrtjp.projectred.ProjectRedExpansion
import mrtjp.projectred.core.inventory.SimpleInventory
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.inventory.Container
import net.minecraft.item.ItemStack

class TileElectricFurnace extends TileMachineIO
{
    override def onBlockPlaced(stack:ItemStack, side:Int, player:EntityPlayer)
    {
        super.onBlockPlaced(stack, side, player)
        rotation = resolveLook_4(player).asInstanceOf[Byte]
    }

    override def getBlockMetadata = 0
    def getBlockID = ProjectRedExpansion.machine1.blockID

    def createContainer(player:EntityPlayer):Container = null
    def getGuiID = MachineGuiFactory.id_furnace

    def getAccessibleSlotsFromSide(var1:Int) = var1 match
    {
        case _ if Seq(0,1) contains var1 => Array(0,1) // input
        case _ if Seq(2,3,4,5) contains var1 => Array(2,3) // output
        case _ => Array()
    }

    def canInsertItem(i:Int, itemstack:ItemStack, j:Int):Boolean = i==0||i==1
    def canExtractItem(i:Int, itemstack:ItemStack, j:Int):Boolean = i==2||i==3

    //0,1 input
    //2,3 output
    //4 buffer
    def createInv:SimpleInventory = new SimpleInventory(5, "smelt", 64)
}
