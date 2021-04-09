///*
// * Copyright (c) 2015.
// * Created by MrTJP.
// * All rights reserved.
// */
//package mrtjp.core.render
//
//import org.lwjgl.opengl.GL11
//import GL11._
//
//object RenderLib
//{
//    var savedBits = Seq.empty[(Int, Boolean)]
//
//    def pushBits(bits:Int*)
//    {
//        savedBits = bits.map(b => b -> glIsEnabled(b))
//    }
//
//    def popBits()
//    {
//        for ((bit, state) <- savedBits)
//            if (state) glEnable(bit)
//            else glDisable(bit)
//    }
//}
