package mrtjp.projectred.core

import mrtjp.core.item.{ItemDefinition, ItemCore}
import net.minecraft.client.renderer.texture.IIconRegister
import net.minecraft.item.{Item, ItemStack}
import mrtjp.projectred.ProjectRedCore
import net.minecraft.util.{EnumChatFormatting, IIcon}
import net.minecraft.creativetab.CreativeTabs
import java.util.{List => JList, Set => JSet}
import cpw.mods.fml.relauncher.{SideOnly, Side}
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.world.World
import mrtjp.projectred.api.IScrewdriver
import net.minecraft.nbt.NBTTagCompound
import net.minecraftforge.oredict.OreDictionary
import org.lwjgl.input.Keyboard
import scala.collection.JavaConversions

abstract class ItemCraftingDamage(name:String) extends ItemCore(name)
{
    setMaxStackSize(1)
    setNoRepair()

    override def hasContainerItem(itemStack:ItemStack) = true

    override def getContainerItem(stack:ItemStack) =
        if (stack.getItem == this)
        {
            stack.setItemDamage(stack.getItemDamage+1)
            stack
        }
        else
        {
            val newStack = new ItemStack(this)
            newStack.setItemDamage(newStack.getMaxDamage)
            newStack
        }

    override def doesContainerItemLeaveCraftingGrid(stack:ItemStack) = false
}

class ItemDrawPlate extends ItemCraftingDamage("projectred.core.drawplate")
{
    setMaxDamage(512)
    setCreativeTab(ProjectRedCore.tabCore)

    override def registerIcons(par1IconRegister:IIconRegister)
    {
        itemIcon = par1IconRegister.registerIcon("projectred:drawplate")
    }
}

class ItemPart extends ItemCore("projectred.core.part")
{
    setCreativeTab(ProjectRedCore.tabCore)
    setHasSubtypes(true)

    override def getSubItems(item:Item, tab:CreativeTabs, list:JList[_])
    {
        for (i <- PartDefs.values)
            list.asInstanceOf[JList[ItemStack]].add(i.makeStack)
    }

    @SideOnly(Side.CLIENT)
    override def registerIcons(reg:IIconRegister)
    {
        for (i <- PartDefs.values) i.registerIcon(reg)
    }

    override def getIconFromDamage(meta:Int) =
    {
        val col = PartDefs(meta)
        if (col != null) col.icon
        else null
    }

    override def getUnlocalizedName(stack: ItemStack):String = {
        val col = PartDefs(stack.getItemDamage())
        if (col != null) getUnlocalizedName() + "." + col.name
        else super.getUnlocalizedName(stack)
    }
}

object PartDefs extends ItemDefinition
{
    override type EnumVal = PartVal
    override def getItem = ProjectRedCore.itemPart

    val PLATE = new PartVal("plate")
    val CONDUCTIVEPLATE = new PartVal("conductive_plate")
    val WIREDPLATE = new PartVal("wired_plate")
    val BUNDLEDPLATE = new PartVal("bundled_plate")
    val ANODE = new PartVal("anode")
    val CATHODE = new PartVal("cathode")
    val POINTER = new PartVal("pointer")
    val SILICONCHIP = new PartVal("silicon_chip")
    val ENERGIZEDSILICONCHIP = new PartVal("energized_silicon_chip")
    val PLATFORMEDPLATE = new PartVal("platformed_plate")

    val REDINGOT = new PartVal("red_ingot")
    val SILICONBOULE = new PartVal("boule")
    val SILICON = new PartVal("silicon")
    val INFUSEDSILICON = new PartVal("infused_silicon")
    val ENERGIZEDSILICON = new PartVal("energized_silicon")
    val MOTOR = new PartVal("motor")
    val COPPERCOIL = new PartVal("copper_coil")
    val IRONCOIL = new PartVal("iron_coil")
    val GOLDCOIL = new PartVal("gold_coil")

    val WHITEILLUMAR = new PartVal("illumar0")
    val ORANGEILLUMAR = new PartVal("illumar1")
    val MAGENTAILLUMAR = new PartVal("illumar2")
    val LIGHTBLUEILLUMAR = new PartVal("illumar3")
    val YELLOWILLUMAR = new PartVal("illumar4")
    val LIMEILLUMAR = new PartVal("illumar5")
    val PINKILLUMAR = new PartVal("illumar6")
    val GREYILLUMAR = new PartVal("illumar7")
    val LIGHTGREYILLUMAR = new PartVal("illumar8")
    val CYANILLUMAR = new PartVal("illumar9")
    val PURPLEILLUMAR = new PartVal("illumar10")
    val BLUEILLUMAR = new PartVal("illumar11")
    val BROWNILLUMAR = new PartVal("illumar12")
    val GREENILLUMAR = new PartVal("illumar13")
    val REDILLUMAR = new PartVal("illumar14")
    val BLACKILLUMAR = new PartVal("illumar15")

    val WOVENCLOTH = new PartVal("cloth")
    val SAIL = new PartVal("sail")

    val RUBY = new PartVal("gemruby")
    val SAPPHIRE = new PartVal("gemsapphire")
    val PERIDOT = new PartVal("gemperidot")

    val REDIRONCOMPOUND = new PartVal("red_iron_comp")
    val SANDYCOALCOMPOUND = new PartVal("sand_coal_comp")
    val REDSILICONCOMPOUND = new PartVal("red_silicon_comp")
    val GLOWINGSILICONCOMPOUND = new PartVal("glow_silicon_comp")

    val NULLROUTINGCHIP = new PartVal("null_chip")
    val NULLUPGRADECHIP = new PartVal("null_upgrd")
    val CHIPUPGRADE_LX = new PartVal("upgrd_lx")
    val CHIPUPGRADE_LY = new PartVal("upgrd_ly")
    val CHIPUPGRADE_LZ = new PartVal("upgrd_lz")
    val CHIPUPGRADE_RX = new PartVal("upgrd_rx")
    val CHIPUPGRADE_RY = new PartVal("upgrd_ry")
    val CHIPUPGRADE_RZ = new PartVal("upgrd_rz")

    val COPPERINGOT = new PartVal("copper_ingot")
    val TININGOT = new PartVal("tin_ingot")
    val SILVERINGOT = new PartVal("silver_ingot")
    val ELECTROTINEINGOT = new PartVal("electrotine_ingot")

    val ELECTROTINE = new PartVal("electrotine_dust")
    val ELECTROTINEIRONCOMPOUND = new PartVal("electrotine_iron_comp")
    val ELECTROTINESILICONCOMPOUND = new PartVal("electrotine_silicon_comp")
    val ELECTROSILICON = new PartVal("electro_silicon")

    //Groups
    val ILLUMARS = WHITEILLUMAR to BLACKILLUMAR toArray

    val oreDictDefinitionIllumar = "projredIllumar"
    val oreDictDefinitionRedIngot = "ingotRedAlloy"

    class PartVal(iconName:String) extends ItemDef
    {
        var icon:IIcon = null

        def registerIcon(reg:IIconRegister)
        {
            icon = reg.registerIcon("projectred:parts/"+iconName)
        }

        override def name = iconName
    }
}

class ItemScrewdriver extends ItemCore("projectred.core.screwdriver") with IScrewdriver
{
    setMaxStackSize(1)
    setMaxDamage(128)
    setNoRepair()
    setCreativeTab(ProjectRedCore.tabCore)

    override def onItemUse(stack:ItemStack, player:EntityPlayer, w:World,
                           x:Int, y:Int, z:Int, side:Int,
                           par8:Float, par9:Float, par10:Float) = false

    override def doesSneakBypassUse(world:World, x:Int, y:Int, z:Int,
                                    player:EntityPlayer) = true

    @SideOnly(Side.CLIENT)
    override def registerIcons(reg:IIconRegister)
    {
        itemIcon = reg.registerIcon("projectred:screwdriver")
    }

    def damageScrewdriver(world:World, player:EntityPlayer)
    {
        player.getHeldItem.damageItem(1, player)
    }
}

class ItemWireDebugger extends ItemCore("projectred.core.wiredebugger")
{
    setMaxStackSize(1)
    setMaxDamage(256)
    setNoRepair()
    setCreativeTab(ProjectRedCore.tabCore)

    override def onItemUse(stack:ItemStack, player:EntityPlayer, w:World,
                           x:Int, y:Int, z:Int, side:Int,
                           par8:Float, par9:Float, par10:Float) = false

    override def doesSneakBypassUse(world:World, x:Int, y:Int, z:Int,
                                    player:EntityPlayer) = true

    override def onItemUseFirst(par1ItemStack:ItemStack, player:EntityPlayer,
                                par3World:World, par4:Int, par5:Int, par6:Int,
                                par7:Int, par8:Float, par9:Float, par10:Float) = false

    @SideOnly(Side.CLIENT)
    override def registerIcons(reg:IIconRegister)
    {
        itemIcon = reg.registerIcon("projectred:debugger")
    }
}

class ItemDataCard extends ItemCore("projectred.core.datacard")
{
    setCreativeTab(ProjectRedCore.tabCore)
    setMaxStackSize(1)

    private val icons = new Array[IIcon](2)

    @SideOnly(Side.CLIENT)
    override def registerIcons(reg:IIconRegister)
    {
        for (i <- Seq(0, 1)) icons(i) = reg.registerIcon("projectred:datacard"+i)
    }

    import ItemDataCard._

    override def getIcon(stack:ItemStack, pass:Int) = if (hasData(stack)) icons(1) else icons(0)

    override def getIcon(stack:ItemStack, renderPass:Int, player:EntityPlayer, usingItem:ItemStack, useRemaining:Int) =
        if (hasData(stack)) icons(1) else icons(0)

    override def getIconIndex(stack:ItemStack) = if (hasData(stack)) icons(1) else icons(0)

    override def addInformation(stack:ItemStack, player:EntityPlayer, list:JList[_], par4:Boolean)
    {
        val l2 = list.asInstanceOf[JList[String]]
        if ((Keyboard.isKeyDown(Keyboard.KEY_LSHIFT)||Keyboard.isKeyDown(Keyboard.KEY_RSHIFT)) && stack.hasTagCompound)
        {
            val tag = stack.getTagCompound
            import JavaConversions._
            var hasData = false
            for (key <- tag.func_150296_c().asInstanceOf[JSet[String]])
            {
                l2.add(EnumChatFormatting.GRAY+
                    key+"->"+tag.getString(key).substring(0, 10)+(if (key.length>16) "..." else ""))
                hasData = true
            }
            if (!hasData) l2.add(EnumChatFormatting.GRAY + "no data")
        }
    }
}

object ItemDataCard
{
    private def assertStack(stack:ItemStack) =
    {
        if (stack != null) stack.getItem match
        {
            case c:ItemDataCard =>
                if (!stack.hasTagCompound) stack.setTagCompound(new NBTTagCompound)
                true
            case _ => false
        }
        else false
    }

    def setData(stack:ItemStack, dir:String, data:String)
    {
        if (assertStack(stack))
        {
            stack.getTagCompound.setString(dir, data)
        }
    }

    def removeData(stack:ItemStack, dir:String)
    {
        if (assertStack(stack))
        {
            stack.getTagCompound.removeTag(dir)
        }
    }

    def hasData(stack:ItemStack) =
    {
        if (assertStack(stack))
        {
            !stack.getTagCompound.hasNoTags
        }
        else false
    }

    def getData(stack:ItemStack, dir:String):String =
    {
        if (assertStack(stack))
        {
            stack.getTagCompound.getString(dir)
        }
        else null
    }
}