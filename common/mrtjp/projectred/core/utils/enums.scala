package mrtjp.projectred.core.utils


abstract class LiteEnumCollector
{
    var values = Seq[LiteEnumVal]()

    def addEnumVal(newVal:LiteEnumVal) =
    {
        values :+= newVal
        values.length-1
    }
}

trait LiteEnumVal extends Ordered[LiteEnumVal]
{
    final val ordinal = getCollector.addEnumVal(this)

    def name = classOf[LiteEnumVal].getName

    override def toString = name

    override def compare(that: LiteEnumVal) =
    {
        if (this.ordinal < that.ordinal) -1
        else if (this.ordinal == that.ordinal) 0
        else 1
    }

    override def equals(other: Any) = this eq other.asInstanceOf[AnyRef]

    override def hashCode = 31*(getClass.## +name.## +ordinal)

    def +(v: LiteEnumVal) = Array(this, v)

    def getCollector:LiteEnumCollector
}
