package mrtjp.projectred.core

import net.minecraft.block.Block
import net.minecraft.block.material.Material
import cpw.mods.fml.common.registry.GameRegistry
import net.minecraft.item.{ItemStack, ItemBlock, Item}
import net.minecraft.tileentity.TileEntity

class BlockCore(name:String, mat:Material) extends Block(mat)
{
    setBlockName(name)
    GameRegistry.registerBlock(this, getItemBlockClass, name)

    def getItemBlockClass = classOf[ItemBlockCore]

    def bindTile[A <: TileEntity](c:Class[A])
    {
        GameRegistry.registerTileEntity(c, getUnlocalizedName)
    }
}

class ItemBlockCore(b:Block) extends ItemBlock(b)
{
    setHasSubtypes(true)
    setMaxDamage(0)

    override def getMetadata(meta:Int) = meta

    override def getUnlocalizedName(stack:ItemStack) = super.getUnlocalizedName+"|"+stack.getItemDamage
}

abstract class BlockDefinition extends ItemDefinition
{
    override type EnumVal <: BlockDef

    override def getItem = Item.getItemFromBlock(getBlock)
    def getBlock:Block

    class BlockDef extends ItemDef
}