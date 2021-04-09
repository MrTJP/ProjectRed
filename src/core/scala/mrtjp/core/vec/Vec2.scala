/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.vec

case class Vec2(dx:Double, dy:Double)
{
    override def equals(obj:scala.Any) = obj match
    {
        case that:Vec2 => dx == that.dx && dy == that.dy
        case _ => false
    }

    def copy = Vec2(dx, dy)

    def add(dx0:Double, dy0:Double) = Vec2(dx+dx0, dy+dy0)
    def subtract(dx0:Double, dy0:Double) = add(-dx0, -dy0)
    def multiply(i:Double, j:Double) = Vec2(dx*i, dy*j)
    def divide(i:Double, j:Double) = multiply(1/i, 1/j)
    def dot(i:Double, j:Double) = dx*i+dy*j

    def add(d:Double):Vec2 = add(d, d)
    def subtract(d:Double):Vec2 = subtract(d, d)
    def multiply(k:Double):Vec2 = multiply(k, k)
    def divide(k:Double):Vec2 = divide(k, k)
    def dot(k:Double):Double = dot(k, k)

    def add(that:Vec2):Vec2 = add(that.dx, that.dy)
    def subtract(that:Vec2):Vec2 = subtract(that.dx, that.dy)
    def multiply(that:Vec2):Vec2 = multiply(that.dx, that.dy)
    def divide(that:Vec2):Vec2 = divide(that.dx, that.dy)
    def dot(that:Vec2):Double = dot(that.dx, that.dy)

    def magSquared = dx*dx+dy*dy
    def mag = math.sqrt(magSquared)

    def normalize = if (mag == 0) Vec2.zeroVec else this/mag
    def negate = Vec2(-dx, -dy)
    def invert = Vec2(dy, dx)

    def project(that:Vec2):Vec2 = that*((this dot that)/that.magSquared)
    def reject(that:Vec2):Vec2 = this-project(that)
    def axialProject:Vec2 =
        if (dx.abs > dy.abs) Vec2(dx, 0) else Vec2(0, dy)

    def unary_- = negate
    def unary_~ = invert

    def +(that:Vec2) = add(that)
    def -(that:Vec2) = subtract(that)
    def *(that:Vec2) = multiply(that)
    def /(that:Vec2) = divide(that)

    def +(that:Double) = add(that)
    def -(that:Double) = subtract(that)
    def *(that:Double) = multiply(that)
    def /(that:Double) = divide(that)

    override def toString = s"Vec2 @[$dx $dy]"
}

object Vec2
{
    val zeroVec = Vec2(0, 0)
    val up = Vec2(0, -1)
    val right = Vec2(1, 0)
    val down = Vec2(0, 1)
    val left = Vec2(-1, 0)
}