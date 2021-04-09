/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.item

import scala.jdk.CollectionConverters._


class ItemEquality
{
    var matchNBT = true
    var matchTags = false

    def apply(key:ItemKey) =
    {
        val c = new AppliedItemEquality(key)
        c.setFlags(matchNBT, matchTags)
        c
    }

    def setFlags(nbt:Boolean, tags:Boolean)
    {
        matchNBT = nbt
        matchTags = tags
    }

    def matches(key1:ItemKey, key2:ItemKey):Boolean =
    {
        if (key1.isEmpty || key2.isEmpty) return key1 == key2

        val stack1 = key1.makeStack(1)
        val stack2 = key2.makeStack(1)

        if (matchTags)
        {
            val a = stack1.getItem.getTags
            val b = stack2.getItem.getTags
            if (a.asScala.exists(b.contains)) return true
        }

        if (key1.item == key2.item)
        {
            if (matchNBT && key1.tag != key2.tag) return false
            return true
        }
        false
    }
}

object ItemEquality
{
    val standard = new ItemEquality
}

class AppliedItemEquality(val key:ItemKey) extends ItemEquality
{
    def matches(key2:ItemKey):Boolean = matches(key, key2)
}
