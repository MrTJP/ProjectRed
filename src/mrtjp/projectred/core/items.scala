package mrtjp.projectred.core

import java.util.{List => JList}

import mrtjp.core.item.{ItemCore, ItemDefinition}
import mrtjp.projectred.ProjectRedCore
import mrtjp.projectred.api.IScrewdriver
import net.minecraft.client.renderer.block.model.ModelResourceLocation
import net.minecraft.creativetab.CreativeTabs
import net.minecraft.entity.player.EntityPlayer
import net.minecraft.item.{Item, ItemStack}
import net.minecraft.util.math.BlockPos
import net.minecraft.world.IBlockAccess
import net.minecraftforge.client.model.ModelLoader

abstract class ItemCraftingDamage(registryName:String) extends ItemCore(registryName)
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
}

class ItemDrawPlate extends ItemCraftingDamage("projectred:drawPlate")
{
    setMaxDamage(512)
    setCreativeTab(ProjectRedCore.tabCore)
}

class ItemPart extends ItemCore("projectred:itemResource")
{
    setCreativeTab(ProjectRedCore.tabCore)
    setHasSubtypes(true)

    override def getSubItems(itemIn:Item, tab:CreativeTabs, subItems:JList[ItemStack])
    {
        for (i <- PartDefs.values)
            subItems.asInstanceOf[JList[ItemStack]].add(i.makeStack)
    }

    override def getUnlocalizedName(stack: ItemStack):String = {
        val col = PartDefs.getEnumFromMeta(stack.getItemDamage)
        if (col != null) getUnlocalizedName() + "." + col.name
        else super.getUnlocalizedName(stack)
    }
}

object PartDefs extends ItemDefinition
{
    override type EnumVal = PartVal
    override def getItem = ProjectRedCore.itemPart

    /** Gate Parts **/
    val PLATE = new PartVal(0, "plate")
    val CONDUCTIVEPLATE = new PartVal(1, "conductive_plate")
    val WIREDPLATE = new PartVal(2, "wired_plate")
    val BUNDLEDPLATE = new PartVal(3, "bundled_plate")
    val PLATFORMEDPLATE = new PartVal(4, "platformed_plate")
    //
    val ANODE = new PartVal(10, "anode")
    val CATHODE = new PartVal(11, "cathode")
    val POINTER = new PartVal(12, "pointer")
    //
    val SILICONCHIP = new PartVal(20, "silicon_chip")
    val ENERGIZEDSILICONCHIP = new PartVal(21, "energized_silicon_chip")

    /** Ingots/Dust **/

    val COPPERINGOT = new PartVal(100, "copper_ingot")
    val TININGOT = new PartVal(101, "tin_ingot")
    val SILVERINGOT = new PartVal(102, "silver_ingot")
    val REDINGOT = new PartVal(103, "red_ingot")
    val ELECTROTINEINGOT = new PartVal(104, "electrotine_ingot")
    val ELECTROTINE = new PartVal(105, "electrotine_dust")

    /** Gems **/

    val RUBY = new PartVal(200, "ruby")
    val SAPPHIRE = new PartVal(201, "sapphire")
    val PERIDOT = new PartVal(202, "peridot")

    /** Go-betweens, To be removed w/ Alloy Smeltery **/

    val SANDYCOALCOMPOUND = new PartVal(250, "sand_coal_comp")
    val REDIRONCOMPOUND = new PartVal(251, "red_iron_comp")
    val ELECTROTINEIRONCOMPOUND = new PartVal(252, "electrotine_iron_comp")

    /** Silicons **/

    val SILICONBOULE = new PartVal(300, "boule")
    val SILICON = new PartVal(301, "silicon")
    //
    val REDSILICONCOMPOUND = new PartVal(310, "red_silicon_comp")
    val GLOWINGSILICONCOMPOUND = new PartVal(311, "glow_silicon_comp")
    val ELECTROTINESILICONCOMPOUND = new PartVal(312, "electrotine_silicon_comp")
    //
    val INFUSEDSILICON = new PartVal(320,"infused_silicon")
    val ENERGIZEDSILICON = new PartVal(341, "energized_silicon")
    val ELECTROSILICON = new PartVal(342, "electro_silicon")

    /** Mechanical Peices **/

    val COPPERCOIL = new PartVal(400, "copper_coil")
    val IRONCOIL = new PartVal(401, "iron_coil")
    val GOLDCOIL = new PartVal(402, "gold_coil")
    //
    val MOTOR = new PartVal(410, "motor")
    //
    val WOVENCLOTH = new PartVal(420, "cloth")
    val SAIL = new PartVal(421, "sail")

    /** Illumars **/

    val WHITEILLUMAR = new PartVal(500, "illumar0")
    val ORANGEILLUMAR = new PartVal(501, "illumar1")
    val MAGENTAILLUMAR = new PartVal(502, "illumar2")
    val LIGHTBLUEILLUMAR = new PartVal(503, "illumar3")
    val YELLOWILLUMAR = new PartVal(504, "illumar4")
    val LIMEILLUMAR = new PartVal(505, "illumar5")
    val PINKILLUMAR = new PartVal(506, "illumar6")
    val GREYILLUMAR = new PartVal(507, "illumar7")
    val LIGHTGREYILLUMAR = new PartVal(508, "illumar8")
    val CYANILLUMAR = new PartVal(509, "illumar9")
    val PURPLEILLUMAR = new PartVal(510, "illumar10")
    val BLUEILLUMAR = new PartVal(511, "illumar11")
    val BROWNILLUMAR = new PartVal(512, "illumar12")
    val GREENILLUMAR = new PartVal(513, "illumar13")
    val REDILLUMAR = new PartVal(514, "illumar14")
    val BLACKILLUMAR = new PartVal(515, "illumar15")

    val NULLROUTINGCHIP = new PartVal(600, "null_chip")

    //Groups
    val ILLUMARS = WHITEILLUMAR to BLACKILLUMAR toArray

    val oreDictDefinitionIllumar = "projredIllumar"
    val oreDictDefinitionRedIngot = "ingotRedAlloy"

    class PartVal(override val meta:Int, iconName:String) extends ItemDef(iconName)
    {
        def setCustomModelResourceLocations()
        {
            ModelLoader.setCustomModelResourceLocation(getItem, meta,
                new ModelResourceLocation("projectred:base/itemResource", "type="+name))
        }

        override def name = iconName
    }
}

class ItemScrewdriver extends ItemCore("projectred:screwdriver") with IScrewdriver
{
    setMaxStackSize(1)
    setMaxDamage(128)
    setNoRepair()
    setCreativeTab(ProjectRedCore.tabCore)

    override def doesSneakBypassUse(stack:ItemStack, world:IBlockAccess, pos:BlockPos, player:EntityPlayer) = true

    override def canUse(player:EntityPlayer, stack:ItemStack) = true

    override def damageScrewdriver(player:EntityPlayer, stack:ItemStack)
    {
        stack.damageItem(1, player)
    }
}

class ItemWireDebugger extends ItemCore("projectred:multimeter")
{
    setMaxStackSize(1)
    setMaxDamage(256)
    setNoRepair()
    setCreativeTab(ProjectRedCore.tabCore)

    override def doesSneakBypassUse(stack:ItemStack, world:IBlockAccess, pos:BlockPos, player:EntityPlayer) = true
}
