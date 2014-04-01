package mrtjp.projectred.transportation;

import cpw.mods.fml.common.registry.GameRegistry;
import mrtjp.projectred.ProjectRedTransportation;
import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.transportation.EnumRoutingChip.ChipVal;
import net.minecraft.block.Block;
import net.minecraft.inventory.InventoryCrafting;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraft.item.crafting.IRecipe;
import net.minecraft.world.World;
import net.minecraftforge.oredict.ShapedOreRecipe;


public class TransportationRecipes
{
    public static void initRecipes()
    {
        initPipeRecipes();
        initChipRecipes();
        initUpgradeRecipes();
    }

    private static void initPipeRecipes()
    {
        /** Item Transport Pipe **/
        GameRegistry.addRecipe(PipeDef.BASIC().getItemStack(16),
                "sgs",
                'g', Block.thinGlass,
                's', Block.stone
        );

        /** Routed Junction Pipe **/
        GameRegistry.addRecipe(PipeDef.ROUTEDJUNCTION().getItemStack(16),
                "RrG",
                "dgd",
                "GrR",
                'R', EnumPart.REDILLUMAR.getItemStack(),
                'r', EnumPart.INFUSEDSILICON.getItemStack(),
                'G', EnumPart.GREENILLUMAR.getItemStack(),
                'd', Item.diamond,
                'g', Block.thinGlass
        );

        /** Routed Interface Pipe **/
        GameRegistry.addRecipe(PipeDef.ROUTEDINTERFACE().getItemStack(),
                "rgr",
                "gjg",
                "rgr",
                'g', Item.goldNugget,
                'j', PipeDef.ROUTEDJUNCTION().getItemStack(),
                'r', Item.redstone
        );

        /** Routed Crafting Pipe **/
        GameRegistry.addRecipe(PipeDef.ROUTEDCRAFTING().getItemStack(),
                "rgr",
                "rjr",
                "rgr",
                'r', Item.redstone,
                'g', Item.glowstone,
                'j', PipeDef.ROUTEDJUNCTION().getItemStack()
        );

        /** Routed Request Pipe **/
        GameRegistry.addRecipe(PipeDef.ROUTEDREQUEST().getItemStack(),
                "rdr",
                "rjr",
                "rdr",
                'r', Item.redstone,
                'd', Item.diamond,
                'j', PipeDef.ROUTEDJUNCTION().getItemStack()
        );

        /** Routed Extension Pipe **/
        GameRegistry.addRecipe(PipeDef.ROUTEDEXTENSION().getItemStack(),
                " r ",
                "rjr",
                " r ",
                'r', Item.redstone,
                'j', PipeDef.ROUTEDJUNCTION().getItemStack()
        );

        /** Routed Firewall Pipe **/
        GameRegistry.addRecipe(PipeDef.ROUTEDFIREWALL().getItemStack(),
                "bcb",
                "cjc",
                "bcb",
                'b', Block.netherBrick,
                'c', Item.magmaCream,
                'j', PipeDef.ROUTEDJUNCTION().getItemStack()
        );
    }

    private static void initChipRecipes()
    {
        /** Chip reset **/
        for (ChipVal r : EnumRoutingChip.VALID_CHIPS())
            GameRegistry.addRecipe(new IRecipe()
            {

                @Override
                public boolean matches(InventoryCrafting inv, World world)
                {
                    return getCraftingResult(inv) != null;
                }

                @Override
                public ItemStack getCraftingResult(InventoryCrafting inv)
                {
                    ChipVal type = getType(inv);
                    if (type != null)
                        if (isTypeExclusive(type, inv))
                            return type.getItemStack(countUnits(inv));

                    return null;
                }

                public ChipVal getType(InventoryCrafting inv)
                {
                    for (int i = 0; i < inv.getSizeInventory(); i++)
                    {
                        ChipVal type = EnumRoutingChip.getForStack(inv.getStackInSlot(i));
                        if (type != null)
                            return type;
                    }
                    return null;
                }

                public boolean isTypeExclusive(ChipVal type, InventoryCrafting inv)
                {
                    for (int i = 0; i < inv.getSizeInventory(); i++)
                    {
                        ItemStack stack = inv.getStackInSlot(i);
                        if (stack != null && !(stack.getItem() instanceof ItemRoutingChip))
                            return false;

                        ChipVal type2 = EnumRoutingChip.getForStack(stack);
                        if (type2 != null && !type2.equals(type))
                            return false;
                    }
                    return true;
                }

                public int countUnits(InventoryCrafting inv)
                {
                    int count = 0;
                    for (int i = 0; i < inv.getSizeInventory(); i++)
                        if (inv.getStackInSlot(i) != null)
                            count++;

                    return count;
                }

                @Override
                public int getRecipeSize()
                {
                    return 2;
                }

                @Override
                public ItemStack getRecipeOutput()
                {
                    return null;
                }

            });

        /** Null chip **/
        GameRegistry.addRecipe(EnumPart.NULLROUTINGCHIP.getItemStack(),
                "gpp",
                "grr",
                "g  ",
                'g', Item.goldNugget,
                'p', Item.paper,
                'r', Item.redstone
        );

        /** Item Responder **/
        addChipRecipe(EnumRoutingChip.ITEMRESPONDER().getItemStack(),
                Item.ingotIron, Item.redstone, Item.redstone,
                EnumPart.ORANGEILLUMAR.getItemStack(),
                EnumPart.ORANGEILLUMAR.getItemStack()
        );

        /** Dynamic Item Responder **/
        addChipRecipe(EnumRoutingChip.DYNAMICITEMRESPONDER().getItemStack(),
                Item.ingotIron, Item.redstone, EnumPart.CYANILLUMAR.getItemStack(),
                EnumPart.ORANGEILLUMAR.getItemStack(),
                EnumPart.ORANGEILLUMAR.getItemStack()
        );

        /** Item Overflow Responder **/
        addChipRecipe(EnumRoutingChip.ITEMOVERFLOWRESPONDER().getItemStack(),
                Item.ingotIron, Item.redstone, Item.redstone,
                EnumPart.GREENILLUMAR.getItemStack(),
                EnumPart.GREENILLUMAR.getItemStack()
        );

        /** Item Terminator **/
        addChipRecipe(EnumRoutingChip.ITEMTERMINATOR().getItemStack(),
                Item.ingotIron, Item.redstone, Item.redstone,
                EnumPart.PURPLEILLUMAR.getItemStack(),
                EnumPart.GREYILLUMAR.getItemStack()
        );

        /** Item Extractor **/
        addChipRecipe(EnumRoutingChip.ITEMEXTRACTOR().getItemStack(),
                Item.ingotIron, Item.redstone, Item.redstone,
                EnumPart.CYANILLUMAR.getItemStack(),
                EnumPart.CYANILLUMAR.getItemStack()
        );

        /** Item Broadcaster **/
        addChipRecipe(EnumRoutingChip.ITEMBROADCASTER().getItemStack(),
                Item.ingotGold, Item.redstone, Item.redstone,
                EnumPart.MAGENTAILLUMAR.getItemStack(),
                EnumPart.MAGENTAILLUMAR.getItemStack()
        );

        /** Item Stock Keeper **/
        addChipRecipe(EnumRoutingChip.ITEMSTOCKKEEPER().getItemStack(),
                Item.diamond, Item.redstone, Item.redstone,
                EnumPart.BLUEILLUMAR.getItemStack(),
                EnumPart.BLUEILLUMAR.getItemStack()
        );

        /** Item Crafting **/
        addChipRecipe(EnumRoutingChip.ITEMCRAFTING().getItemStack(),
                Item.glowstone, Item.redstone, Item.glowstone,
                EnumPart.LIMEILLUMAR.getItemStack(),
                EnumPart.LIMEILLUMAR.getItemStack()
        );
    }

    private static void addChipRecipe(ItemStack result, Object bus, Object material2, Object material1, Object dyeLeft, Object dyeRight)
    {
        GameRegistry.addRecipe(new ShapedOreRecipe(result,
                "dMD",
                "bcm",
                "dMD",
                'd', dyeLeft,
                'M', material2,
                'D', dyeRight,
                'b', bus,
                'c', EnumPart.NULLROUTINGCHIP.getItemStack(),
                'm', material1
        ));
    }

    private static void initUpgradeRecipes()
    {
        /** Router Utility **/
        GameRegistry.addRecipe(new ItemStack(ProjectRedTransportation.itemRouterUtility()),
                "  r",
                "iei",
                "iii",
                'r', Item.redstone,
                'i', Item.ingotIron,
                'e', Item.emerald
        );

        /** Null Upgrade **/
        GameRegistry.addRecipe(EnumPart.NULLUPGRADECHIP.getItemStack(),
                "prp",
                "rrr",
                "prp",
                'p', Item.paper,
                'r', Item.redstone
        );

        /** LX **/
        GameRegistry.addRecipe(EnumPart.CHIPUPGRADE_LX.getItemStack(),
                "rrr",
                " ng",
                "r r",
                'r', Item.redstone,
                'n', EnumPart.NULLUPGRADECHIP.getItemStack(),
                'g', Item.goldNugget
        );

        /** RX **/
        GameRegistry.addRecipe(EnumPart.CHIPUPGRADE_RX.getItemStack(),
                "r r",
                "gn ",
                "rrr",
                'r', Item.redstone,
                'n', EnumPart.NULLUPGRADECHIP.getItemStack(),
                'g', Item.goldNugget
        );

        /** LY **/
        GameRegistry.addRecipe(EnumPart.CHIPUPGRADE_LY.getItemStack(),
                "l l",
                " n ",
                " l ",
                'l', PRColors.BLUE.getDye(),
                'n', EnumPart.CHIPUPGRADE_LX.getItemStack()
        );

        /** RY **/
        GameRegistry.addRecipe(EnumPart.CHIPUPGRADE_RY.getItemStack(),
                "l l",
                " n ",
                " l ",
                'l', PRColors.BLUE.getDye(),
                'n', EnumPart.CHIPUPGRADE_RX.getItemStack()
        );

        /** LZ **/
        GameRegistry.addRecipe(EnumPart.CHIPUPGRADE_LZ.getItemStack(),
                "r r",
                " n ",
                "rer",
                'r', Item.redstone,
                'n', EnumPart.CHIPUPGRADE_LY.getItemStack(),
                'e', Item.emerald
        );

        /** RZ **/
        GameRegistry.addRecipe(EnumPart.CHIPUPGRADE_RZ.getItemStack(),
                "r r",
                " n ",
                "rer",
                'r', Item.redstone,
                'n', EnumPart.CHIPUPGRADE_RY.getItemStack(),
                'e', Item.emerald
        );
    }
}

