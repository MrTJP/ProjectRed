package mrtjp.projectred.archive

import java.io.{File, FileWriter}
import java.lang.{Character => JChar}
import java.util.Collections

import codechicken.lib.reflect.ObfMapping
import codechicken.lib.util.ResourceUtils
import com.google.gson.internal.Streams
import com.google.gson.stream.JsonWriter
import com.google.gson.{JsonArray, JsonElement, JsonObject}
import mezz.jei.JustEnoughItems
import mezz.jei.startup.ProxyCommonClient
import mrtjp.projectred.archive.Conversions._
import net.minecraft.block.Block
import net.minecraft.command.{ICommand, ICommandSender}
import net.minecraft.item.crafting.CraftingManager
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.server.MinecraftServer
import net.minecraft.util.ResourceLocation
import net.minecraft.util.math.BlockPos
import net.minecraftforge.client.ClientCommandHandler
import net.minecraftforge.common.crafting.CraftingHelper
import org.apache.commons.io.IOUtils
import org.apache.logging.log4j.LogManager


class RecipeDumper(val name: String) {

    final val logger = LogManager.getLogger("RecipeDumper")
    final val dump_dir = System.getProperty("pr_recipe_dump")
    var recipes: Array[Recipe] = Array()

    def addRecipe(recipe: Recipe): Recipe = {
        recipes :+= recipe
        recipe
    }

    def addRecipe(in: ItemStack, objects: Any*): Recipe = {
        addRecipe(new ShapedRecipe(in, objects: _*))
    }


    def dump() {
        for (r <- recipes) {
            val file = s"$dump_dir\\$name\\${r.json_name}.json"
            logger.info(s"Dumping recipe ${r.json_name} to $file")
            val writer = new JsonWriter(new FileWriter(ResourceUtils.ensureExists(new File(file))))
            writer.setIndent("\t")
            val obj = new JsonObject
            r.serialize(obj)
            Streams.write(obj, writer)
            IOUtils.closeQuietly(writer)
        }
    }
}

object RecipeDumper {
    def load() {
        if (!ObfMapping.obfuscated) {
            ClientCommandHandler.instance.registerCommand(DumpCommand)
            ClientCommandHandler.instance.registerCommand(ReloadRecipesCommand)
        }
    }
}

object DumpCommand extends ICommand {
    //@formatter:off
    override def getName = "dump_recipes"
    override def getUsage(sender: ICommandSender) = ""
    override def getAliases = Collections.emptyList()
    override def checkPermission(server: MinecraftServer, sender: ICommandSender) = true
    override def getTabCompletions(server: MinecraftServer, sender: ICommandSender, args: Array[String], targetPos: BlockPos) = Collections.emptyList()
    override def isUsernameIndex(args: Array[String], index: Int) = false
    override def compareTo(o: ICommand) = 0
    //@formatter:on

    override def execute(server: MinecraftServer, sender: ICommandSender, args: Array[String]) = {
        CoreRecipes.initCoreRecipes()
        ExpansionRecipes.initRecipes()
        ExplorationRecipes.initRecipes()
        IlluminationRecipes.initRecipes()
        IntegrationRecipes.initRecipes()
        TransmissionRecipes.initTransmissionRecipes()
        TransportationRecipes.initRecipes()
    }
}

object ReloadRecipesCommand extends ICommand {
    final val logger = LogManager.getLogger("RecipeDumper")

    //@formatter:off
    override def getName = "reload_recipes"
    override def getUsage(sender: ICommandSender) = ""
    override def getAliases = Collections.emptyList()
    override def checkPermission(server: MinecraftServer, sender: ICommandSender) = true
    override def getTabCompletions(server: MinecraftServer, sender: ICommandSender, args: Array[String], targetPos: BlockPos) = Collections.emptyList()
    override def isUsernameIndex(args: Array[String], index: Int) = false
    override def compareTo(o: ICommand) = 0
    //@formatter:on

    override def execute(server: MinecraftServer, sender: ICommandSender, args: Array[String]) = {
        logger.info("Forcing recipe reload..")
        CraftingManager.REGISTRY.registryObjects.clear()
        CraftingManager.REGISTRY.underlyingIntegerMap.clear()
        CraftingManager.REGISTRY.inverseObjectRegistry.clear()
        CraftingHelper.loadRecipes(true)
        val proxy = JustEnoughItems.getProxy.asInstanceOf[ProxyCommonClient]
        logger.info("Forcing JEI reload.")
        proxy.starter.start(proxy.plugins)
        logger.info("Recipe reload complete.")
    }
}

abstract class Recipe(val output: ItemStack, val objects: Any*) {

    var json_name: String = _
    var conditions: JsonArray = _

    def serialize(obj: JsonObject) {
        if (conditions != null) {
            obj.add("conditions", conditions)
        }
        obj.addProperty("type", type_)
        obj.add("result", new JsonObject().add("item", output.registryName).add("data", output.getMetadata).add("count", output.getCount))
    }

    def createObject(item: ItemStack) = new JsonObject().add("type", "minecraft:item").add("item", item.registryName).add("data", item.getMetadata)

    def createObject(o: Any): JsonObject = {
        o match {
            case s: String =>
                val i = new JsonObject
                i.addProperty("type", "forge:ore_dict")
                i.addProperty("ore", s)
                i
            case s: ItemStack => createObject(s)
            case s: Item => createObject(new ItemStack(s))
            case s: Block => createObject(new ItemStack(s))
            case s: JsonObject => s
            case _ => throw new RuntimeException("Unhandled recipe class" + o.getClass)
        }
    }

    def type_ : String

    def setJsonName(name: String): Recipe = {
        json_name = name
        this
    }

    def setConditions(obj: JsonArray): Recipe = {
        conditions = obj
        this
    }
}

class ShapelessOreRecipe(output: ItemStack, objects: Any*) extends Recipe(output, objects) {

    override def serialize(obj: JsonObject) = {
        super.serialize(obj)
        val ingredients = new JsonArray
        for (o <- objects) {
            ingredients.add(createObject(o))
        }
        obj.add("ingredients", ingredients)
    }

    override def type_ = "forge:ore_shapeless"
}

class ShapedOreRecipe(output: ItemStack, objects: Any*) extends Recipe(output, objects) {

    override def serialize(obj: JsonObject) = {
        super.serialize(obj)
        var pattern = Array[String]()
        var ings = Map[JChar, Any]()
        var i = 0
        while (objects(i).isInstanceOf[String]) {
            pattern :+= objects(i).asInstanceOf[String]
            i += 1
        }
        while ((0 until objects.length contains i) && objects(i).isInstanceOf[JChar]) {
            val ch = objects(i).asInstanceOf[JChar]
            val o = objects(i + 1)
            ings += ch -> o
            i += 2
        }

        val p = new JsonArray
        val keys = new JsonObject
        pattern.foreach(p.add)
        ings.filter({ case (k, _) => pattern.exists(p => p.contains(k)) }).foreach { case (k, v) => keys.add(k, createObject(v)) }
        obj.add("pattern", p)
        obj.add("key", keys)
    }

    override def type_ = "forge:ore_shaped"
}

class ShapelessOreNBTCopyRecipe(output: ItemStack, objects: Any*) extends ShapelessOreRecipe(output, objects: _*) {

    override def type_ = "projectred-core:shapeless_ore_nbt_copy"
}

class ShapelessRecipe(i: ItemStack, o: Any*) extends ShapelessOreRecipe(i, o: _*) {

    override def type_ = "minecraft:crafting_shapeless"
}

class ShapedRecipe(i: ItemStack, o: Any*) extends ShapedOreRecipe(i, o: _*) {

    override def type_ = "minecraft:crafting_shaped"
}

object Conversions {

    implicit class JObj(val obj: JsonObject) {
        def add(name: String, value: String): JsonObject = {
            obj.addProperty(name, value)
            obj
        }

        def add(name: String, value: Int): JsonObject = {
            obj.addProperty(name, value)
            obj
        }

        def add(name: JChar, value: JsonElement): JsonObject = {
            obj.add(name.toString, value)
            obj
        }

        def add_(name: String, value: JsonObject): JsonObject = {
            obj.add(name.toString, value)
            obj
        }
    }

    implicit class JArr(val arr: JsonArray) {
        def add(value: String): JsonArray = {
            arr.add(value)
            arr
        }

        def add(value: Int): JsonArray = {
            arr.add(value)
            arr
        }

        def add_(value: JsonElement): JsonArray = {
            arr.add(value)
            arr
        }
    }

    implicit class RegName(val obj: ItemStack) {
        def getRegistryName: ResourceLocation = obj.getItem.getRegistryName

        def registryName: String = obj.getItem.getRegistryName.toString
    }

}
