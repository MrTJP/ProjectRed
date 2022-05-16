/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.vec

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
    def union(r:Rect) = new Rect(Point(math.min(x, r.x), math.min(y, r.y)), Point(math.max(maxX, r.maxX), math.max(maxY, r.maxY)))

    def ndc(p:Point):Vec2 = {
        val dx = ((p.x - origin.x) / width.toDouble * 2D) - 1
        val dy = ((p.y - origin.y) / height.toDouble * 2D) - 1
        Vec2(dx, -dy)
    }
}

object Rect
{
    val zeroRect = Rect(Point.zeroPoint, Size.zeroSize)
    val infiniteRect = Rect(-Point.infinitePoint/2, Size.infiniteSize)
}