/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.core.data

import codechicken.lib.config.{ConfigTag, StandardConfigFile}

import java.nio.file.Paths
import scala.jdk.CollectionConverters._

abstract class ModConfig(modID:String)
{
    var config:ConfigTag = null
    private var loaded = false;

    protected case class BaseCategory(key:String, comment:String = "")
    {
        def cat = config.getTag(key)
        cat.setComment(comment)

        def put[T](key:String, value:T, force:Boolean):T =
            put(key, value, "", force)

        def put[T](key:String, value:T, comment:String = ""):T =
            put(key, value, comment, false)

        def put[T](key:String, value:T, comment:String, force:Boolean):T =
        {
            import codechicken.lib.config.ConfigTag.TagType._
            def getType(value:Any):ConfigTag.TagType = value match
            {
                case xs:Array[_] => getType(xs.head)
                case b:Boolean   => BOOLEAN
                case i:Int       => INT
                case s:String    => STRING
                case d:Double    => DOUBLE
                case _           => STRING
            }

            val propType = getType(value)
            val prop = cat.getTag(key)

            if (!loaded) {
                prop.setComment(comment)
                value match {
                    case xs: Array[Boolean] => prop.setDefaultBooleanList(xs.map(Boolean.box).toList.asJava)
                    case xs: Array[Int] => prop.setDefaultIntList(xs.map(Int.box).toList.asJava)
                    case xs: Array[String] => prop.setDefaultStringList(xs.toList.asJava)
                    case xs: Array[Double] => prop.setDefaultDoubleList(xs.map(Double.box).toList.asJava)
                    case xs: Array[_] => prop.setDefaultStringList(xs.map(_.toString).toList.asJava)
                    case b: Boolean => prop.setDefaultBoolean(b)
                    case i: Int => prop.setDefaultInt(i)
                    case s: String => prop.setDefaultString(s)
                    case d: Double => prop.setDefaultDouble(d)
                    case s => prop.setDefaultString(s.toString)
                }
                if (force) prop.resetToDefault()
            }

            val reslult = value match
            {
                case xs:Array[_]    => propType match
                {
                    case BOOLEAN    => prop.getBooleanList
                    case INT    => prop.getIntList
                    case STRING     => prop.getStringList
                    case DOUBLE     => prop.getDoubleList
                    case _          => prop.getStringList
                }
                case b:Boolean      => prop.getBoolean
                case i:Int          => prop.getInt
                case s:String       => prop.getString
                case d:Double       => prop.getDouble
                case _              => prop.getString
            }
            reslult.asInstanceOf[T]
        }

        def containsKey(key:Any) = cat.hasTag(key.toString)
    }

    def getFileName = modID

    def loadConfig()
    {
        config = new StandardConfigFile(Paths.get("./config/", getFileName + ".cfg")).load()
        initValues()
        loaded = true
        config.save()
    }

    protected def initValues()
}
