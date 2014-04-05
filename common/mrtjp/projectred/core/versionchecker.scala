package mrtjp.projectred.core

import cpw.mods.fml.common.{TickType, ITickHandler}
import java.io.{BufferedReader, InputStreamReader}
import java.net.URL
import java.sql.Timestamp
import java.text.SimpleDateFormat
import java.util
import java.util.Date
import net.minecraft.entity.player.EntityPlayer
import scala.util.parsing.json.JSON

class PRVersioningThread extends Thread("PR Version Check") with ITickHandler
{
    var parser = new PRBuildsParser(Configurator.version)
    var outdated = false

    setDaemon(true)
    start()

    override def run()
    {
        if (parser.current.contains("@")) return
        if (!Configurator.versionChecking) return
        parser.parseAll()
        outdated = parser.isOutdated
    }

    override def getLabel = "PR Version Checker"

    override def ticks() = util.EnumSet.of(TickType.PLAYER)
    override def tickStart(tick:util.EnumSet[TickType], tickData:AnyRef*){}

    var displayed = false
    override def tickEnd(tick:util.EnumSet[TickType], tickData:AnyRef*)
    {
        if (!outdated || displayed) return

        val p = tickData(0).asInstanceOf[EntityPlayer]
        val target = parser.getTargetBuild
        p.addChatMessage("Version "+target.version+" of ProjectRed was released on "+target.buildDate+".")
        for (s <- parser.getChangelogSince) p.addChatMessage(s)

        displayed = true
    }
}

class PRBuildsParser(val current:String)
{
    //maps are indexed with version. (ie, "4.0.0")
    var builds:Map[String, BuildDef] = null

    val checkUnstables = Configurator.versionCheckDevBuilds

    def reader(urlIn:String) =
    {
        val url = new URL(urlIn)
        val is = url.openStream()
        val isr = new InputStreamReader(is)
        new BufferedReader(isr)
    }

    def JSONfrom(from:String) =
    {
        val stream = reader(from)
        val builder = new StringBuilder
        var in:String = null
        while ({in = stream.readLine(); in} != null) builder.append(in)
        val jtext = builder.result()
        stream.close()
        JSON.parseFull(jtext).get.asInstanceOf[Map[String, Any]]
    }

    def parseBuilds =
    {
        val parse = JSONfrom("1=htped?nosj/ipa/deR02%tcejorP/boj/0808:moc.sikiweidni.ic//:ptth".reverse)
        val rawBuilds = parse.get("builds").get.asInstanceOf[List[Map[String, Any]]]
        val buildB = Map.newBuilder[String, BuildDef]
        for (m <- rawBuilds)
        {
            def bdef = new BuildDef(m, false)
            buildB += bdef.version -> bdef
        }
        buildB.result().filter(_._2.isValidBuild)
    }

    def parseStableBuilds =
    {
        val parse = JSONfrom("2=htped?nosj/ipa/dednemmoceR/ssecorp/noitomorp/deR02%tcejorP/boj/0808:moc.sikiweidni.ic//:ptth".reverse)
        val rawBuilds = parse.get("builds").get.asInstanceOf[List[Map[String, Any]]]
        val versionB = Set.newBuilder[Int]
        for (b <- rawBuilds) versionB += b.get("target").get.asInstanceOf[Map[String, Double]].get("number").get.toInt
        versionB.result()
    }

    def parseChanges =
    {
        val stream2 = reader("https://raw.github.com/MrTJP/ProjectRed/master/resources/Changelog")
        val changeB = Map.newBuilder[String, Vector[String]]
        var next:String = null
        def poll() = {next = stream2.readLine(); next}
        while (poll() != null)
        {
            if (next.startsWith("v") || next.startsWith("[dev]"))
            {
                val key = next.stripPrefix("[dev]").stripPrefix("v")
                val changesB = Vector.newBuilder[String]
                while (poll() != null && next.startsWith("- ")) changesB += next
                changeB += key -> changesB.result()
            }
        }
        stream2.close()
        changeB.result()
    }

    def parseAll()
    {
        builds = parseBuilds
        val stables = parseStableBuilds
        for (b <- builds) if (stables.contains(b._2.buildNumber)) b._2.isRecommended = true

        for ((k, v) <- parseChanges)
        {
            val build = builds.getOrElse(k, null)
            if (build != null) build.changes = v
        }
    }

    def isOutdated = getTargetBuild > getCurrentBuild

    def getCurrentBuild = builds.getOrElse(current, null)

    def getTargetBuild:BuildDef =
    {
        for (b <- builds) if (b._2.isRecommended || checkUnstables) return b._2
        null
    }

    def getChangelogSince =
    {
        val from = getCurrentBuild
        val target = getTargetBuild

        val builder = Vector.newBuilder[String]
        def build()
        {
            val it = builds.iterator
            while (it.hasNext)
            {
                val b = it.next()._2
                if (b <= target)
                {
                    if (b > from)
                    {
                        builder += "(since v"+b.version+")"
                        builder ++= b.changes
                        builder += "\n"
                    }
                    else return
                }
            }
        }
        build()
        builder.result()
    }
}

class BuildDef(val data:Map[String, Any], var isRecommended:Boolean) extends Ordered[BuildDef]
{
    private def get[T](key:String) = data.get(key).get.asInstanceOf[T]
    private val coreJarName =
    {
        val one = get[List[Map[String, Any]]]("artifacts")
        if (!one.isEmpty) one(1).asInstanceOf[Map[String, String]]("fileName")
        else "#PR-#version-#build"
    }

    val isSuccessful = get[String]("result") == "SUCCESS"
    val isPublic = get[String]("description") == "public"

    val buildNumber = get[Double]("number").toInt
    val version = coreJarName.split("-")(2).replace("."+buildNumber+".jar", "")
    val mcVersion = coreJarName.split("-")(1)

    private[BuildDef] val date = new Date(new Timestamp(get[Double]("timestamp").toLong).getTime)
    val buildDate = new SimpleDateFormat("MM/dd/yyyy").format(date)

    var changes:Vector[String] = Vector[String]()

    def isValidBuild = isSuccessful && isPublic

    override def equals(other:Any) = other match
    {
        case that:BuildDef =>
            isSuccessful == that.isSuccessful &&
                isPublic == that.isPublic &&
                buildNumber == that.buildNumber &&
                version == that.version &&
                buildDate == that.buildDate
        case _ => false
    }

    override def compare(that:BuildDef) = date.compareTo(that.date)
}