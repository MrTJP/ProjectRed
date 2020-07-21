/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.compatibility.storagedrawers

import com.jaquadro.minecraft.storagedrawers.block.tile.TileEntityDrawers
import mrtjp.core.inventory.{IInvWrapperRegister, InvWrapper}
import mrtjp.core.item.ItemKey
import mrtjp.projectred.compatibility.IPRPlugin
import mrtjp.projectred.core.Configurator
import net.minecraft.inventory.IInventory

object PluginStorageDrawers extends IPRPlugin
{
  override def getModIDs = Array("ProjRed|Transportation")

  override def isEnabled = Configurator.compat_StorageDrawers

  override def preInit(){}

  override def init(){}

  override def postInit()
  {
    InvWrapper.register(StorageDrawersInvWrapperRegister)
  }

  override def desc() = "Storage Drawers pipe interactions"
}

object StorageDrawersInvWrapperRegister extends IInvWrapperRegister
{
  override def wrapperID = "storagedrawers"
  override def matches(inv:IInventory) = inv.isInstanceOf[TileEntityDrawers]
  override def create(inv:IInventory) = new StorageDrawersInvWrapper(inv)
}

class StorageDrawersInvWrapper(inv:IInventory) extends InvWrapper(inv)
{
  def getDrawers = inv.asInstanceOf[TileEntityDrawers]

  override def getSpaceForItem(item:ItemKey) =
  {
    var freeSpace = 0
    for(i <- 0 until getDrawers.getDrawerCount){
      val drawer = getDrawers.getDrawer(i)

      if(drawer.canItemBeStored(item.testStack)){
        freeSpace += drawer.getMaxCapacity(item.testStack) - drawer.getStoredItemCount
      }
    }
    freeSpace
  }

  override def hasSpaceForItem(item:ItemKey) = getSpaceForItem(item) > 0

  override def getItemCount(item:ItemKey) =
  {
    var stored = 0
    for(i <- 0 until getDrawers.getDrawerCount){
      val drawer = getDrawers.getDrawer(i)

      if(drawer.canItemBeExtracted(item.testStack)){
        stored += drawer.getStoredItemCount
      }
    }
    stored
  }

  override def hasItem(item:ItemKey) = getItemCount(item) > 0

  override def injectItem(item:ItemKey, toAdd:Int):Int =
  {
    var added = 0

    for(mergePass <- Array(true, false)){
      for(i <- 0 until getDrawers.getDrawerCount){
        val drawer = getDrawers.getDrawer(i)

          if(drawer.canItemBeStored(item.testStack)) {
          val spaceLeft = drawer.getMaxCapacity(item.testStack) - drawer.getStoredItemCount
          val toAddToDrawer = if (mergePass && drawer.isEmpty) 0 else math.min(spaceLeft, toAdd - added)

          if(toAddToDrawer > 0){
            if (drawer.isEmpty) {
              drawer.setStoredItemRedir(item.testStack, toAddToDrawer)
            } else {
              drawer.setStoredItemCount(drawer.getStoredItemCount + toAddToDrawer)
            }
          }

          added += toAddToDrawer
        }
      }
    }

    added
  }

  override def extractItem(item:ItemKey, toExtract:Int) =
  {
    var extracted = 0

    for(i <- 0 until getDrawers.getDrawerCount){
      val drawer = getDrawers.getDrawer(i)

      if(drawer.canItemBeExtracted(item.testStack)){
        val stored = drawer.getStoredItemCount
        val toExtractFromDrawer = math.min(stored, toExtract - extracted)
        drawer.setStoredItemCount(drawer.getStoredItemCount - toExtractFromDrawer)
        extracted += toExtractFromDrawer
      }
    }
    extracted
  }

  override def getAllItemStacks =
  {
    var items = Map[ItemKey, Int]()

    for(i <- 0 until getDrawers.getDrawerCount){
      val drawer = getDrawers.getDrawer(i)

      val key = ItemKey.get(drawer.getStoredItemPrototype)
      if(key != null)
        items += key -> (drawer.getStoredItemCount + items.getOrElse(key, 0))
    }
    items
  }
}