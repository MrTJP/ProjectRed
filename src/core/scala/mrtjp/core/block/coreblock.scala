/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.block

import codechicken.lib.vec.Vector3
import net.minecraft.block.AbstractBlock
import net.minecraft.item.BlockItem
//import mrtjp.core.item.ItemDefinition
import net.minecraft.block.Block
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.util.math.BlockPos
import net.minecraft.world.World

class BlockCore(properties: AbstractBlock.Properties) extends Block(properties) {
    def getItemBlockClass: Class[_ <: BlockItem] = classOf[ItemBlockCore]
}

class ItemBlockCore(b: Block, properties: Item.Properties) extends BlockItem(b, properties) {

//    override def placeBlockAt(stack: ItemStack, player: EntityPlayer, w: World, pos: BlockPos, side: EnumFacing, hitX: Float, hitY: Float, hitZ: Float, newState: IBlockState) = {
//        val a = super.placeBlockAt(stack, player, w, pos, side, hitX, hitY, hitZ, newState)
//        block match {
//            case b: MultiTileBlock =>
//                b.postBlockSetup(w, pos, side.ordinal, player, stack, new Vector3(hitX, hitY, hitZ))
//            case _ =>
//        }
//        a
//    }
}

//abstract class BlockDefinition extends ItemDefinition {
//    override type EnumVal <: BlockDef
//
//    override def getItem = Item.getItemFromBlock(getBlock)
//
//    def getBlock: Block
//
//    class BlockDef(variantName: String) extends ItemDef(variantName)
//
//}
