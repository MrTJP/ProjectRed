///*
// * Copyright (c) 2015.
// * Created by MrTJP.
// * All rights reserved.
// */
//package mrtjp.core.block
//
//import java.util.Random
//
//import codechicken.lib.vec.Cuboid6
//import net.minecraft.block.{Block, IGrowable}
//import net.minecraft.entity.player.EntityPlayer
//import net.minecraft.item.{ItemStack, Item}
//import net.minecraft.world.{IBlockAccess, World}
//import net.minecraftforge.common.util.ForgeDirection
//import net.minecraftforge.common.{EnumPlantType, IPlantable}
//
//trait TPlantBlock extends InstancedBlock with IPlantable with IGrowable
//{
//    setTickRandomly(true)
//    setStepSound(Block.soundTypeGrass)
//
//    override def canPlaceBlockAt(w:World, x:Int, y:Int, z:Int) =
//    {
//        super.canPlaceBlockAt(w, x, y, z) && canStay(w, x, y, z)
//    }
//
//    override def onNeighborBlockChange(w:World, x:Int, y:Int, z:Int, b:Block)
//    {
//        super.onNeighborBlockChange(w, x, y, z, b)
//        dropIfCantStay(w, x, y, z)
//    }
//
//    def dropIfCantStay(w:World, x:Int, y:Int, z:Int)
//    {
//        if (!canStay(w, x, y, z))
//        {
//            dropBlockAsItem(w, x, y, z, w.getBlockMetadata(x, y, z), 2)
//            w.setBlockToAir(x, y, z)
//        }
//    }
//
//    def canStay(w:World, x:Int, y:Int, z:Int) = w.getTileEntity(x, y, z) match
//    {
//        case p:TPlantTile => p.canBlockStay
//        case _ => initialCanStay(w, x, y, z)
//    }
//
//    def initialCanStay(w:World, x:Int, y:Int, z:Int):Boolean
//
//    override def getPlantType(w:IBlockAccess, x:Int, y:Int, z:Int):EnumPlantType
//
//    override def getPlant(w:IBlockAccess, x:Int, y:Int, z:Int) = this
//
//    override def getPlantMetadata(w:IBlockAccess, x:Int, y:Int, z:Int) = 0
//
//    //can apply bonemeal
//    override def func_149851_a(w:World, x:Int, y:Int, z:Int, isClient:Boolean) = w.getTileEntity(x, y, z) match
//    {
//        case p:TPlantTile => p.applyBonemeal()
//        case _ => false
//    }
//
//    //2 useless callbacks for IGrowable, easily handled with just the above method.
//    override def func_149852_a(w:World, rand:Random, x:Int, y:Int, z:Int) = false
//    override def func_149853_b(w:World, rand:Random, x:Int, y:Int, z:Int){}
//}
//
//trait TPlantTile extends InstancedBlockTile
//{
//    override def getCollisionBounds = null
//
//    def canBlockStay = world.getBlock(x, y, z) match
//    {
//        case p:TPlantBlock => p.initialCanStay(world, x, y, z)
//        case _ => false
//    }
//
//    def applyBonemeal() = false
//
//    override def onNeighborChange(b:Block)
//    {
//        super.onNeighborChange(b)
//        dropIfCantStay
//    }
//
//    def dropIfCantStay =
//    {
//        if (!canBlockStay)
//        {
//            breakBlock_do()
//            true
//        }
//        else false
//    }
//
//    override def isSolid(side:Int) = false
//}
//
//trait TItemSeed extends Item with IPlantable
//{
//    override def onItemUse(item:ItemStack, player:EntityPlayer, w:World, x:Int, y:Int, z:Int, side:Int, hitX:Float, hitY:Float, hitZ:Float):Boolean =
//    {
//        if (super.onItemUse(item, player, w, x, y, z, side, hitX, hitY, hitZ)) return true
//        if (side != 1) return false
//
//        if (player.canPlayerEdit(x, y, z, side, item) && player.canPlayerEdit(x, y+1, z, side, item))
//            if (w.getBlock(x, y, z).canSustainPlant(w, x, y, z, ForgeDirection.UP, this) && w.isAirBlock(x, y+1, z))
//            {
//                val (b, m) = getPlantBlock
//                w.setBlock(x, y+1, z, b, m, 3)
//                item.stackSize -= 1
//                onPlanted(item, w, x, y+1, z)
//                return true
//            }
//        false
//    }
//
//    def getPlantBlock:(Block, Int)
//    def onPlanted(item:ItemStack, w:World, x:Int, y:Int, z:Int){}
//
//    override def getPlant(w:IBlockAccess, x:Int, y:Int, z:Int) = getPlantBlock._1
//    override def getPlantMetadata(w:IBlockAccess, x:Int, y:Int, z:Int) = getPlantBlock._2
//
//    override def getPlantType(w:IBlockAccess, x:Int, y:Int, z:Int):EnumPlantType
//}
