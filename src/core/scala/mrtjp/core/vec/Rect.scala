/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.vec

import mrtjp.core.vec.Rect.zeroRect

case class Rect(origin:Point, size:Size)
{
    def this(x:Int, y:Int, width:Int, height:Int) = this(Point(x, y), Size(width, height))
    def this(min:Point, max:Point) = this(min, Size(max.x-min.x, max.y-min.y))
    def this(r:Rect) = this(r.origin, r.size)

    override def equals(obj:scala.Any) = obj match
    {
        case that:Rect => origin == that.origin && size == that.size
        case _ => false
    }

    def copy = new Rect(origin, size)

    def x = origin.x
    def y = origin.y
    def width = size.width
    def height = size.height

    def maxX = x+width
    def maxY = y+height
    def maxPoint = Point(maxX, maxY)

    def midX = x+width/2
    def midY = y+height/2
    def midPoint = Point(midX, midY)

    def contains(p:Point):Boolean = p.x >= x && p.y >= y && p.x <= maxX && p.y <= maxY
    def contains(rect:Rect):Boolean = contains(rect.origin) && contains(rect.maxPoint)
    def intersects(r:Rect) = contains(r.origin) || contains(r.maxPoint) || r.contains(origin) || r.contains(maxPoint)

    def enclose(p:Point) = new Rect(Point(math.min(x, p.x), math.min(y, p.y)), Point(math.max(maxX, p.x), math.max(maxY, p.y)))

    def union(r:Rect):Rect = {
        if (this == zeroRect) return r
        if (r == zeroRect) return this
        new Rect(Point(math.min(x, r.x), math.min(y, r.y)), Point(math.max(maxX, r.maxX), math.max(maxY, r.maxY)))
    }

    def trap(r:Rect) = {
        val dx = (if (r.x < x) x-r.x else 0) + (if (r.maxX > maxX) maxX-r.maxX else 0)
        val dy = (if (r.y < y) y-r.y else 0) + (if (r.maxY > maxY) maxY-r.maxY else 0)
        new Rect(r.x + dx, r.y + dy, r.width, r.height)
    }
}

object Rect
{
    val zeroRect = Rect(Point.zeroPoint, Size.zeroSize)
    val infiniteRect = Rect(-Point.infinitePoint/2, Size.infiniteSize)
}