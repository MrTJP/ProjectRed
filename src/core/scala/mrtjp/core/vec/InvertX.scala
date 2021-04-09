/*
 * Copyright (c) 2014.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.vec

import codechicken.lib.vec.{Matrix4, VariableTransformation, Vector3}

object InvertX extends VariableTransformation(new Matrix4)
{
    override def apply(vec:Vector3){vec.x *= -1}
    override def inverse() = this
}