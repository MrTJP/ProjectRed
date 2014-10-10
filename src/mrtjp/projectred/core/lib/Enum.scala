package mrtjp.projectred.core.lib

import scala.collection.immutable.BitSet
import scala.collection.{mutable, SortedSetLike, immutable}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.{Builder => MBuilder, BitSet => MBitSet}
import scala.xml.Null

trait Enum
{
    thisenum =>

    type EnumVal <: Value

    private var vals = Vector[EnumVal]()
    def values = vals

    private final def addEnumVal(newVal:EnumVal):Int =
    {
        vals :+= newVal
        vals.indexOf(newVal)
    }

    def isDefinedAt(idx:Int) = vals.isDefinedAt(idx)

    def apply(ordinal:Int):EnumVal =
        if (vals.isDefinedAt(ordinal)) vals(ordinal)
        else null.asInstanceOf[EnumVal]

    protected trait Value extends Ordered[EnumVal]
    {
        //self:EnumVal =>
        def getThis = this.asInstanceOf[EnumVal]

        final val ordinal = addEnumVal(getThis)

        def name:String
        override def toString = name

        private[Enum] val outerEnum = thisenum

        override def compare(that:EnumVal) =
            if (this.ordinal < that.ordinal) -1
            else if (this.ordinal == that.ordinal) 0
            else 1

        override def equals(other:Any) = other match
        {
            case that:Value => (outerEnum == that.outerEnum) &&
                (ordinal == that.ordinal)
            case _ => false
        }

        override def hashCode = 31*(this.getClass.## +name.## +ordinal)

        def +(v:EnumVal) = ValSet(getThis, v)
        def ++(xs:TraversableOnce[EnumVal]) = (ValSet.newBuilder ++= xs).result()

        def until(v:EnumVal) = build(ordinal until v.ordinal)
        def to(v:EnumVal) = build(ordinal to v.ordinal)
        private def build(r:Range) =
        {
            val b = ValSet.newBuilder
            for (i <- r) b += apply(i)
            b.result()
        }
    }

    private[Enum] object ValOrdering extends Ordering[EnumVal]
    {
        override def compare(x:EnumVal, y:EnumVal) = x compare y
    }

    class ValSet(var set:BitSet) extends Set[EnumVal]
    with immutable.SortedSet[EnumVal]
    with SortedSetLike[EnumVal, ValSet]
    with Serializable
    {
        implicit def ordering = ValOrdering
        override def empty = ValSet.empty

        override def rangeImpl(from:Option[EnumVal], until:Option[EnumVal]) =
            new ValSet(set.rangeImpl(from.map(_.ordinal), until.map(_.ordinal)))

        override def contains(elem:EnumVal) = set contains elem.ordinal
        override def +(elem:EnumVal) = new ValSet(set + elem.ordinal)
        override def -(elem:EnumVal) = new ValSet(set - elem.ordinal)
        override def iterator = set.iterator map (id => thisenum(id))
        override def keysIteratorFrom(start: EnumVal) =
            throw new NotImplementedError("Please report this crash")
    }

    object ValSet
    {
        val empty = new ValSet(BitSet.empty)

        def apply(elems:EnumVal*) = (newBuilder ++= elems).result()

        def newBuilder = new MBuilder[EnumVal, ValSet]
        {
            private[this] val b = new MBitSet
            def +=(x:EnumVal) = {b += x.ordinal; this}
            def clear() = b.clear()
            def result() = new ValSet(b.toImmutable)
        }

        implicit def canBuildFrom = new CanBuildFrom[ValSet, EnumVal, ValSet]
        {
            def apply(from:ValSet) = newBuilder
            def apply() = newBuilder
        }
    }
}