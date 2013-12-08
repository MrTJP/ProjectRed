package mrtjp.projectred.transportation;

import mrtjp.projectred.core.PRColors;
import mrtjp.projectred.core.ItemPart.EnumPart;
import mrtjp.projectred.transportation.ItemRoutingChip.EnumRoutingChip;
import net.minecraft.block.Block;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraftforge.oredict.ShapedOreRecipe;
import cpw.mods.fml.common.registry.GameRegistry;


public class TransportationRecipes {

    public static void initRecipes() {
        initPipeRecipes();
        initChipRecipes();
    }

    private static void initPipeRecipes() {
        /** Item Transport Pipe **/
        GameRegistry.addRecipe(EnumPipe.BASIC.getItemStack(16),
                "sgs",
                'g', Block.thinGlass,
                's', Block.stone
                );

        /** Routed Junction Pipe **/
        GameRegistry.addRecipe(EnumPipe.ROUTEDJUNCTION.getItemStack(16),
                "RrG",
                "dgd",
                "GrR",
                'R', EnumPart.REDILLUMAR.getItemStack(),
                'r', Item.redstone,
                'G', EnumPart.GREENILLUMAR.getItemStack(),
                'd', Item.diamond,
                'g', Block.thinGlass
                );
        
        /** Routed Interface Pipe **/
        GameRegistry.addRecipe(EnumPipe.ROUTEDINTERFACE.getItemStack(),
                "rgr",
                "gjg",
                "rgr",
                'g', Item.goldNugget,
                'j', EnumPipe.ROUTEDJUNCTION.getItemStack(),
                'r', Item.redstone
                );
        
        /** Routed Crafting Pipe **/
        GameRegistry.addRecipe(EnumPipe.ROUTEDCRAFTING.getItemStack(),
                "rlr",
                "rjr",
                "rlr",
                'r', Item.redstone,
                'l', new ItemStack(Item.dyePowder, 1, PRColors.BLUE.dyeId()),
                'j', EnumPipe.ROUTEDJUNCTION.getItemStack()
                );
        
        /** Routed Request Pipe **/
        GameRegistry.addRecipe(EnumPipe.ROUTEDREQUEST.getItemStack(),
                "rdr",
                "rjr",
                "rdr",
                'r', Item.redstone,
                'd', Item.diamond,
                'j', EnumPipe.ROUTEDJUNCTION.getItemStack()
                );

    }
    
    private static void initChipRecipes() {
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
        addChipRecipe(EnumRoutingChip.ITEMRESPONDER.getItemStack(), 
                Item.ingotIron, Item.redstone, Item.redstone, 
                EnumPart.ORANGEILLUMAR.getItemStack(), 
                EnumPart.ORANGEILLUMAR.getItemStack());

        /** Dynamic Item Responder **/
        addChipRecipe(EnumRoutingChip.DYNAMICITEMRESPONDER.getItemStack(), 
                Item.ingotIron, Item.redstone, EnumPart.CYANILLUMAR.getItemStack(),
                EnumPart.ORANGEILLUMAR.getItemStack(),
                EnumPart.ORANGEILLUMAR.getItemStack());

        /** Item Overflow Responder **/
        addChipRecipe(EnumRoutingChip.ITEMOVERFLOWRESPONDER.getItemStack(), 
                Item.ingotIron, Item.redstone, Item.redstone,
                EnumPart.GREENILLUMAR.getItemStack(),
                EnumPart.GREENILLUMAR.getItemStack());

        /** Item Terminator **/
        addChipRecipe(EnumRoutingChip.ITEMTERMINATOR.getItemStack(), 
                Item.ingotIron, Item.redstone, Item.redstone,
                EnumPart.PURPLEILLUMAR.getItemStack(),
                EnumPart.GREYILLUMAR.getItemStack());
        
        /** Item Extractor **/
        addChipRecipe(EnumRoutingChip.ITEMEXTRACTOR.getItemStack(), 
                Item.ingotIron, Item.redstone, Item.redstone,
                EnumPart.CYANILLUMAR.getItemStack(),
                EnumPart.CYANILLUMAR.getItemStack());
        
        /** Item Broadcaster **/
        addChipRecipe(EnumRoutingChip.ITEMBROADCASTER.getItemStack(), 
                Item.ingotGold, Item.redstone, Item.redstone,
                EnumPart.MAGENTAILLUMAR.getItemStack(),
                EnumPart.MAGENTAILLUMAR.getItemStack());
        
        /** Item Stock Keeper **/
        addChipRecipe(EnumRoutingChip.ITEMSTOCKKEEPER.getItemStack(), 
                Item.diamond, Item.redstone, Item.redstone,
                EnumPart.BLUEILLUMAR.getItemStack(),
                EnumPart.BLUEILLUMAR.getItemStack());
    }
    
    private static void addChipRecipe(ItemStack result, Object bus, Object material2, Object material1, Object dyeLeft, Object dyeRight) {
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
}

