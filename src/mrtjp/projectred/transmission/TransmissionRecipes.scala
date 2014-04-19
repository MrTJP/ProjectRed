package mrtjp.projectred.transmission

import codechicken.microblock.ItemMicroPart
import cpw.mods.fml.common.registry.GameRegistry
import mrtjp.projectred.core.ItemPart.EnumPart
import mrtjp.projectred.core.ShapedOreNBTRecipe
import mrtjp.projectred.core.ShapelessOreNBTRecipe
import net.minecraft.block.Block
import net.minecraft.item.Item
import net.minecraft.item.ItemStack
import net.minecraftforge.oredict.ShapedOreRecipe
import net.minecraftforge.oredict.ShapelessOreRecipe
import mrtjp.projectred.core.libmc.PRColors

object TransmissionRecipes
{
    def initTransmissionRecipes()
    {
        initWireRecipes()
        initPartRecipes()
    }

    private def initWireRecipes()
    {
        GameRegistry.addRecipe(WireDef.RED_ALLOY.getItemStack(12),
            " r ", " r ", " r ",
            'r':Character, EnumPart.REDINGOT.getItemStack)

        for (i <- 0 until WireDef.INSULATED_WIRE.length)
        {
            val w:WireDef = WireDef.INSULATED_WIRE(i)
            GameRegistry.addRecipe(w.getItemStack(12),
                "WrW", "WrW", "WrW",
                'W':Character, new ItemStack(Block.cloth, 1, PRColors.get(i).woolId),
                'r':Character, EnumPart.REDINGOT.getItemStack)
        }

        for (w <- WireDef.INSULATED_WIRE)
            GameRegistry.addRecipe(new ShapelessOreRecipe(
                w.getItemStack, PRColors.get(w.meta-WireDef.INSULATED_0.meta).getOreDict,
                WireDef.oreDictDefinitionInsulated, Item.silk))

        for (w <- WireDef.INSULATED_WIRE)
            GameRegistry.addRecipe(new ShapelessOreNBTRecipe(
                w.getFramedItemStack, PRColors.get(w.meta - WireDef.INSULATED_0.meta).getOreDict,
                WireDef.oreDictDefinitionInsFramed, ItemMicroPart.create(769, Block.wood.getUnlocalizedName)).setCheckNBT())

        GameRegistry.addRecipe(new ShapedOreRecipe(WireDef.BUNDLED_N.getItemStack,
            "SWS", "WWW", "SWS",
            'S':Character, Item.silk,
            'W':Character, WireDef.oreDictDefinitionInsulated))

        for (w <- WireDef.BUNDLED_WIRE)
        {
            if (w != WireDef.BUNDLED_N)
            GameRegistry.addRecipe(new ShapelessOreRecipe(w.getItemStack, PRColors.get(w.meta-WireDef.BUNDLED_0.meta).getOreDict,
                WireDef.oreDictDefinitionBundled, Item.silk))
        }

        for (w <- WireDef.VALID_WIRE) if (w.hasFramedForm)
            GameRegistry.addRecipe(new ShapedOreNBTRecipe(w.getFramedItemStack,
                "sss", "sis", "sss",
                'i':Character, w.getItemStack,
                's':Character, ItemMicroPart.create(769, Block.wood.getUnlocalizedName)).setCheckNBT())
    }

    private def initPartRecipes()
    {
        GameRegistry.addRecipe(EnumPart.WIREDPLATE.getItemStack,
            "r", "p",
            'r':Character, WireDef.RED_ALLOY.getItemStack,
            'p':Character, EnumPart.PLATE.getItemStack)

        GameRegistry.addRecipe(EnumPart.BUNDLEDPLATE.getItemStack,
            "r", "p",
            'r':Character, WireDef.BUNDLED_N.getItemStack,
            'p':Character, EnumPart.PLATE.getItemStack)
    }
}