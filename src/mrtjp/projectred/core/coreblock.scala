package mrtjp.projectred.core

import net.minecraft.block.Block
import net.minecraft.block.material.Material
import cpw.mods.fml.common.registry.GameRegistry
import net.minecraft.item.{ItemStack, ItemBlock, Item}
import net.minecraft.tileentity.TileEntity
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.world.World
import mrtjp.projectred.core.libmc.MultiTileBlock
import codechicken.lib.vec.Vector3

class BlockCore(name:String, mat:Material) extends Block(mat)
{
    setBlockName(name)
    GameRegistry.registerBlock(this, getItemBlockClass, name)

    def getItemBlockClass:Class[_ <: ItemBlock] = classOf[ItemBlockCore]

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

    override def placeBlockAt(stack:ItemStack, player:EntityPlayer, w:World, x:Int, y:Int, z:Int, side:Int, hitX:Float, hitY:Float, hitZ:Float, meta:Int) =
    {
        val a = super.placeBlockAt(stack, player, w, x, y, z, side, hitX, hitY, hitZ, meta)
        field_150939_a match
        {
            case b:MultiTileBlock => b.postBlockSetup(w, x, y, z, side, meta, player, stack, new Vector3(hitX, hitY, hitZ))
            case _ =>
        }
        a
    }
}

abstract class BlockDefinition extends ItemDefinition
{
    override type EnumVal <: BlockDef

    override def getItem = Item.getItemFromBlock(getBlock)
    def getBlock:Block

    class BlockDef extends ItemDef
}