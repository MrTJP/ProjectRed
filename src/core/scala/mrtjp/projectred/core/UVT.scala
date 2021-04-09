package mrtjp.projectred.core

import codechicken.lib.vec.uv.{UV, UVTransformation}
import codechicken.lib.vec.{Transformation, Vector3}

class UVT(t:Transformation) extends UVTransformation
{
    private val vec = new Vector3

    def transform(uv:UV)
    {
        vec.set(uv.u, 0, uv.v).apply(t)
        uv.set(vec.x, vec.z)
    }

    override def apply(uv:UV) =
    {
        vec.set(uv.u, 0, uv.v).apply(t)
        uv.set(vec.x, vec.z)
    }

    override def inverse() = new UVT(t.inverse())
}
