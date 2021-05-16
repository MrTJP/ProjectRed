/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.item

import net.minecraft.item.{Item, ItemStack}
import net.minecraft.nbt.CompoundNBT

import scala.collection.immutable.HashMap

object ItemKey
{
    def get(stack:ItemStack):ItemKey = new ItemKey(stack.getItem, stack.getTag)

    @deprecated("Use nonnull standard")
    def getOrNull(stack:ItemStack):ItemKey =
    {
        if (stack.isEmpty) null
        else get(stack)
    }
}

class ItemKey(val item:Item, val tag:CompoundNBT) extends Ordered[ItemKey]
{
    lazy val testStack = makeStack(1)
    lazy val itemID = Item.getId(item)

    private val hash = itemID*1000001+(if (tag != null) tag.hashCode else 0)
    override def hashCode = hash

    override def equals(other:Any) = other match
    {
        case that:ItemKey =>
            item == that.item &&
                tag == that.tag
        case _ => false
    }

    override def toString = getName.toString

    def compare(that:ItemKey) =
    {
        itemID-that.itemID
    }

    def makeStack(size:Int):ItemStack =
    {
        val stack = new ItemStack(item, size)
        if (tag != null) stack.setTag(tag.copy())
        stack
    }

    def copy = new ItemKey(item, tag)

    def isEmpty = testStack.isEmpty

    /** Interactions **/
    def getItem = item
    def getMaxStackSize = testStack.getMaxStackSize
    def getName = testStack.getDisplayName
}

object ItemKeyStack
{
    def get(key:ItemKey, size:Int) = new ItemKeyStack(key, size)

    def get(stack:ItemStack):ItemKeyStack = new ItemKeyStack(ItemKey.get(stack), stack.getCount)

    @deprecated("Use nonnull standard")
    def getOrNull(stack:ItemStack):ItemKeyStack =
    {
        if (stack.isEmpty) null
        else get(stack)
    }
}

class ItemKeyStack(val key:ItemKey, var stackSize:Int) extends Ordered[ItemKeyStack]
{
    override def hashCode = key.hashCode

    override def equals(other:Any) = other match
    {
        case that:ItemKeyStack =>
            key == that.key && stackSize == that.stackSize
        case _ => false
    }

    override def toString = "["+key.toString+", "+stackSize+"]"

    def makeStack = key.makeStack(stackSize)

    def copy = new ItemKeyStack(key.copy, stackSize)

    def isEmpty = key.isEmpty || stackSize <= 0

    def compare(that:ItemKeyStack) =
    {
        val c = key.compare(that.key)
        if (c == 0) stackSize-that.stackSize
        else c
    }
}

class ItemQueue
{
    private var collection = HashMap[ItemKey, Int]()

    def +=(elem:(ItemKey, Int)) =
    {
        val current = collection.getOrElse(elem._1, 0)
        collection += elem._1 -> (current+elem._2)
        this
    }

    def ++=(xs:IterableOnce[(ItemKey, Int)]) = {xs foreach +=; this}

    def ++=(that:ItemQueue) = {that.result.foreach(+=); this}

    def add(item:ItemKey, amount:Int)
    {
        this += item -> amount
    }

    def -=(elem:(ItemKey, Int)) =
    {
        val remaining = apply(elem._1)-elem._2
        if (remaining > 0) collection += elem._1 -> remaining
        else collection -= elem._1
    }

    def --=(xs:IterableOnce[(ItemKey, Int)]) = {xs foreach -=; this}

    def --=(that:ItemQueue) = {that.result.foreach(-=); this}

    def remove(item:ItemKey, amount:Int)
    {
        this -= item -> amount
    }

    def apply(item:ItemKey):Int = collection.getOrElse(item, 0)

    def clear(){collection = HashMap[ItemKey, Int]()}

    def isEmpty = collection.isEmpty

    def nonEmpty = collection.nonEmpty

    def keySet = collection.keySet

    def count(p:ItemKey => Boolean) =
        collection.foldLeft(0){(i, pair) =>
            if (p(pair._1)) i+pair._2 else i
        }

    def countItems(p:ItemKey => Boolean) =
        collection.count(pair => p(pair._1))

    def result =
    {
        val b = HashMap.newBuilder[ItemKey, Int]
        b ++= collection
        b.result()
    }
}

object ItemKeyConversions
{
    implicit def itemToIK(item:Item):ItemKey = ItemKey.get(new ItemStack(item))
    implicit def IKToItem(i:ItemKey):Item = i.getItem

    implicit def stackToIK(stack:ItemStack):ItemKey = ItemKey.get(stack)
    implicit def IKToStack(key:ItemKey):ItemStack = key.makeStack(0)

    implicit def stackToIKS(stack:ItemStack):ItemKeyStack = ItemKeyStack.get(stack)
    implicit def IKSToStack(key:ItemKeyStack):ItemStack = key.makeStack

    implicit def KToKS(key:ItemKey):ItemKeyStack = ItemKeyStack.get(key, 0)
    implicit def KSToK(key:ItemKeyStack):ItemKey = key.key
}
