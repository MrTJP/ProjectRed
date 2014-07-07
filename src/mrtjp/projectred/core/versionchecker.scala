package mrtjp.projectred.core

import java.io.{BufferedReader, InputStreamReader}
import java.net.URL
import java.sql.Timestamp
import java.text.SimpleDateFormat
import java.util.Date
import cpw.mods.fml.common.eventhandler.SubscribeEvent
import cpw.mods.fml.common.gameevent.TickEvent
import cpw.mods.fml.common.gameevent.TickEvent.PlayerTickEvent
import net.minecraft.client.Minecraft
import net.minecraft.util.ChatComponentText
import scala.collection.immutable.ListMap
import com.google.gson.{JsonParser, JsonObject, JsonArray, JsonPrimitive}

class PRVersioningThread extends Thread("PR Version Check")
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

    var displayed = false

    @SubscribeEvent
    def tickEnd(event:PlayerTickEvent)
    {
        if (event.phase == TickEvent.Phase.END)
        {
            if (!outdated || displayed) return

            val p = Minecraft.getMinecraft.thePlayer
            val target = parser.getTargetBuild
            p.addChatMessage(new ChatComponentText("Version "+target.version+" of ProjectRed was released on "+target.buildDate+"."))
            for (s <- parser.getChangelogSince) p.addChatMessage(new ChatComponentText(s))

            displayed = true
        }
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
        new JsonParser().parse(jtext).getAsJsonObject()
    }

    def parseBuilds =
    {
        val parse = JSONfrom("1=htped?nosj/ipa/deR02%tcejorP/boj/0808:moc.sikiweidni.ic//:ptth".reverse)
        val rawBuildsIter = parse.getAsJsonArray("builds").iterator()
        val buildB = Vector.newBuilder[BuildDef]
        while (rawBuildsIter.hasNext())
        {
            def bdef = new BuildDef(rawBuildsIter.next().getAsJsonObject(), false)
            buildB += bdef
        }
        ListMap(buildB.result().filter(_.isValidBuild).sorted.reverse.map(b => b.version -> b):_*)
    }

    def parseStableBuilds =
    {
        val parse = JSONfrom("2=htped?nosj/ipa/dednemmoceR/ssecorp/noitomorp/deR02%tcejorP/boj/0808:moc.sikiweidni.ic//:ptth".reverse)
        val rawBuildsIter = parse.getAsJsonArray("builds").iterator()
        val versionB = Set.newBuilder[Int]
        while(rawBuildsIter.hasNext())
        {
            var e = rawBuildsIter.next().getAsJsonObject().get("target")
            if (e.isInstanceOf[JsonObject])
                versionB += e.asInstanceOf[JsonObject].getAsJsonPrimitive("number").getAsInt()
        }
        versionB.result()
    }

    def parseChanges =
    {
        val stream2 = reader("https://raw.github.com/MrTJP/ProjectRed/master/resources/Changelog")
        val changeB = ListMap.newBuilder[String, Vector[String]]
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
        for ((v, b) <- builds) if (stables.contains(b.buildNumber)) b.isRecommended = true

        var lastBuild:BuildDef = null
        for ((k, v) <- parseChanges)
        {
            val build = builds.getOrElse(k, null)
            if (build != null)
            {
                lastBuild = build
                build.changes = v
            }
            else if (lastBuild != null) //for builds that are no longer valid
                lastBuild.changes = (Vector.newBuilder[String]
                        ++= lastBuild.changes
                        += "(since v"+k+")" ++= v).result()
        }
    }

    def isOutdated = getCurrentBuild == null || getTargetBuild > getCurrentBuild

    def getCurrentBuild = builds.getOrElse(current, null)

    def getTargetBuild:BuildDef =
    {
        for ((v, b) <- builds) if (b.isRecommended || checkUnstables) return b
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
                    if (b > from) builder += "(since v"+b.version+")" ++= b.changes
                    else return
                }
            }
        }
        build()
        builder.result()
    }
}

class BuildDef(val data:JsonObject, var isRecommended:Boolean) extends Ordered[BuildDef]
{
    private def get[T](key:String) = data.get(key).asInstanceOf[T]
    private val coreJarName =
    {
        val one = get[JsonArray]("artifacts")
        if (one.size() > 0) one.get(1).getAsJsonObject().getAsJsonPrimitive("fileName").getAsString().replace("ProjectRed-", "ProjectRed")
        else "#PR-#version-#build"
    }

    val isSuccessful = get[JsonPrimitive]("result").getAsString() contains "SUCCESS"
    val isPublic = get[JsonPrimitive]("description").getAsString() contains "public"

    val buildNumber = get[JsonPrimitive]("number").getAsInt()
    val version = coreJarName.split("-")(2).replace("."+buildNumber+".jar", "")
    val mcVersion = coreJarName.split("-")(1)

    private val date = new Date(new Timestamp(get[JsonPrimitive]("timestamp").getAsLong()).getTime)
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

    override def compare(that:BuildDef) = buildNumber-that.buildNumber
}