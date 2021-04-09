/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.vec

case class Size(width:Int, height:Int)
{
    override def equals(obj:scala.Any) = obj match
    {
        case that:Size => that.width == width && that.height == height
        case _ => false
    }

    def negate = Size(-width, -height)
    def invert = Size(height, width)

    def add(dw:Int, dh:Int) = Size(width+dw, height+dh)
    def subtract(dw:Int, dh:Int) = add(-dw, -dh)
    def multiply(i:Int, j:Int) = Size(width*i, height*j)
    def divide(i:Int, j:Int) = Size(width/i, height/j)

    def add(d:Int):Size = add(d, d)
    def subtract(d:Int):Size = subtract(d, d)
    def multiply(k:Int):Size = multiply(k, k)
    def divide(k:Int):Size = divide(k, k)

    def add(that:Size):Size = add(that.width, that.height)
    def subtract(that:Size):Size = subtract(that.width, that.height)
    def multiply(that:Size):Size = multiply(that.width, that.height)
    def divide(that:Size):Size = divide(that.width, that.height)

    def unary_- = negate
    def unary_~ = invert

    def +(that:Size) = add(that)
    def -(that:Size) = subtract(that)
    def *(that:Size) = multiply(that)
    def /(that:Size) = divide(that)

    def +(that:Int) = add(that)
    def -(that:Int) = subtract(that)
    def *(that:Int) = multiply(that)
    def /(that:Int) = divide(that)

    def vectorize = Vec2(width, height)

    override def toString = s"Size @[$width $height]"
}

object Size
{
    val zeroSize = Size(0, 0)

    val infiniteSize = Size(Int.MaxValue, Int.MaxValue)

    def apply(point:Point):Size = Size(point.x, point.y)
}