package mrtjp.projectred.core.utils

import scala.util.control.ControlThrowable

class LabelBreaks
{
    val throwable = new LabelThrowable()

    def label(tag:String)(op: => Unit)
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

    def break(tag:String) = throw throwable(tag)
}

class LabelThrowable extends ControlThrowable
{
    var ident = ""

    def apply(tag:String) =
    {
        ident = tag
        this
    }
}

object LabelBreaks extends LabelBreaks