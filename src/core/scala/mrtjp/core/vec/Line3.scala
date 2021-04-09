/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.vec

case class Line3(p1:Point3, p2:Point3)
{
    override def equals(obj:scala.Any) = obj match
    {
        case that:Line3 => p1 == that.p1 && p2 == that.p2
        case _ => false
    }

    def copy = Line3(p1, p2)

    def length = (p2-p1).vectorize.mag

    def lineContains(p:Point3) =
    {
        val v1 = (p2-p1).vectorize
        val v2 = (p-p1).vectorize
        v1.normalize == v2.normalize
    }

    def segmentContains(p:Point3) =
    {
        val v1 = (p2-p1).vectorize
        val v2 = (p-p1).vectorize
        v1.normalize == v2.normalize && v2.magSquared <= v1.magSquared
    }

    def stretch(p3:Point3) =
        if (lineContains(p3))
        {
            val l1 = Line3(p1, p2)
            val l2 = Line3(p2, p3)
            val l3 = Line3(p1, p3)
            Seq(l1, l2, l3).sortBy(-1*_.length).head
        }
        else null
}

object Line3
{

}
