package mrtjp.projectred.core.lib

import scala.util.control.ControlThrowable

class LabelBreaks
{
    def label(tag:String)(op: => Any):Unit =
    {
        try
        {
            op
        }
        catch
        {
            case ex:LabelThrowable if ex.ident.equals(tag) => /** broken **/
            case ex:Throwable => throw ex
        }
    }

    def break(tag:String):Unit = throw new LabelThrowable()(tag)

    //Shorthand single break
    def label(op: => Any):Unit = label("$1")(op)
    def break():Unit = break("$1")
}

class LabelThrowable extends ControlThrowable
{
    var ident = ""

    def apply(tag:String) =
    {
        ident = tag
        this
    }

    override def toString() =
    {
        super.toString() + ": " + ident
    }
}

object LabelBreaks extends LabelBreaks
