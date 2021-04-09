/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.vec

case class Point(x:Int, y:Int)
{
    override def equals(obj:scala.Any) = obj match
    {
        case that:Point => x == that.x && y == that.y
        case _ => false
    }

    def copy = Point(x, y)

    def add(dx:Int, dy:Int) = Point(x+dx, y+dy)
    def subtract(dx:Int, dy:Int) = add(-dx, -dy)
    def multiply(i:Int, j:Int) = Point(x*i, y*j)
    def divide(i:Int, j:Int) = Point(x/i, y/j)

    def add(d:Int):Point = add(d, d)
    def subtract(d:Int):Point = subtract(d, d)
    def multiply(k:Int):Point = multiply(k, k)
    def divide(k:Int):Point = divide(k, k)

    def add(that:Point):Point = add(that.x, that.y)
    def subtract(that:Point):Point = subtract(that.x, that.y)
    def multiply(that:Point):Point = multiply(that.x, that.y)
    def divide(that:Point):Point = divide(that.x, that.y)

    def negate = Point(-x, -y)
    def invert = Point(y, x)
    def vectorize = Vec2(x, y)

    def max(that:Point) = Point(x max that.x, y max that.y)
    def min(that:Point) = Point(x min that.x, y min that.y)

    def clamp(rect:Rect) = this min rect.maxPoint max rect.origin
    def clamp(size:Size) = this min Point(size) max Point.zeroPoint

    def offset(r:Int):Point = offset(r, 1)
    def offset(r:Int, amount:Int):Point = this+(Point.dirOffsets(r)*amount)

    def unary_- = negate
    def unary_~ = invert

    def +(that:Point) = add(that)
    def -(that:Point) = subtract(that)
    def *(that:Point) = multiply(that)
    def /(that:Point) = divide(that)

    def +(that:Int) = add(that)
    def -(that:Int) = subtract(that)
    def *(that:Int) = multiply(that)
    def /(that:Int) = divide(that)

    override def toString = s"Point @[$x $y]"
}

object Point
{
    val infinitePoint = Point(Int.MaxValue, Int.MaxValue)
    val zeroPoint = Point(0, 0)

    def apply(size:Size):Point = Point(size.width, size.height)

    def apply(vec2:Vec2):Point = Point(vec2.dx.toInt, vec2.dy.toInt)

    private val dirOffsets = Seq(
        Point(0, -1),
        Point(1, 0),
        Point(0, 1),
        Point(-1, 0)
    )
}
