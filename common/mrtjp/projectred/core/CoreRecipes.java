package mrtjp.projectred.core;

import mrtjp.projectred.ProjectRedCore;
import mrtjp.projectred.ProjectRedExploration;
import mrtjp.projectred.core.BlockBasics.EnumBasics;
import mrtjp.projectred.core.ItemPart.EnumPart;
import net.minecraft.block.Block;
import net.minecraft.item.Item;
import net.minecraft.item.ItemStack;
import net.minecraftforge.oredict.ShapedOreRecipe;
import net.minecraftforge.oredict.ShapelessOreRecipe;
import codechicken.microblock.ItemMicroPart;
import codechicken.microblock.handler.MicroblockProxy;
import cpw.mods.fml.common.Loader;
import cpw.mods.fml.common.registry.GameRegistry;

public class CoreRecipes {
    public static void initCoreRecipes() {
        initApplianceRecipes();
        initPartRecipes();
        initToolRecipes();
    }

    private static void initApplianceRecipes() {
        /** Alloy Smelter **/
        GameRegistry.addRecipe(EnumBasics.ALLOYSMELTER.getItemStack(), 
                "CBC",
                "BBB",
                "CBC",
                'C', Block.blockClay,
                'B', Block.brick
        );
    }
    
    private static void initToolRecipes() {
        /** Draw Plate **/
        GameRegistry.addRecipe(new ShapedOreNBTRecipe(
                new ItemStack(ProjectRedCore.itemDrawPlate, 1),
                " i ",
                "idi",
                " i ",
                'i', ItemMicroPart.create(770, Block.blockIron.getUnlocalizedName()),
                'd', ItemMicroPart.create(2, Block.blockDiamond.getUnlocalizedName())
        ).setCheckNBT());  
        
        /** Screw Driver **/
        GameRegistry.addRecipe(new ItemStack(ProjectRedCore.itemScrewdriver),
                "i  ",
                " ib",
                " bi",
                'i', Item.ingotIron,
                'b', new ItemStack(Item.dyePowder, 1, PRColors.BLUE.dyeId())
        );
        
        /** Wire debugger **/
        GameRegistry.addRecipe(new ItemStack(ProjectRedCore.itemWireDebugger), 
                "a a",
                "ber",
                "bgr",
                'a', EnumPart.REDINGOT.getItemStack(),
                'b', new ItemStack(Item.dyePowder, 1, PRColors.BLACK.dyeId()),
                'e', Item.emerald,
                'l', new ItemStack(Item.dyePowder, 1, PRColors.BLUE.dyeId()),
                'r', new ItemStack(Item.dyePowder, 1, PRColors.RED.dyeId()),
                'g', Item.glowstone
        );

    }

    private static void initPartRecipes() {
        /** Circuit Plate **/
        GameRegistry.addSmelting(Block.stone.blockID, EnumPart.PLATE.getItemStack(2), 0f);
        
        /** Conductive Plate **/
        GameRegistry.addRecipe(EnumPart.CONDUCTIVEPLATE.getItemStack(), 
                "r",
                "p",
                'r', Item.redstone,
                'p', EnumPart.PLATE.getItemStack()
        );
                
        /** Anode **/
        GameRegistry.addRecipe(EnumPart.ANODE.getItemStack(3), 
                " r ",
                "rrr",
                "ppp",
                'r', Item.redstone,
                'p', EnumPart.PLATE.getItemStack()
        );
        
        /** Cathode **/
        GameRegistry.addRecipe(EnumPart.CATHODE.getItemStack(), 
                "t",
                "p",
                't', Block.torchRedstoneActive,
                'p', EnumPart.PLATE.getItemStack()
        );
        
        /** Pointer **/
        GameRegistry.addRecipe(EnumPart.POINTER.getItemStack(), 
                "b",
                "m",
                "c",
                'b', Block.stone,
                'm', EnumPart.MOTOR.getItemStack(),
                'c', EnumPart.CATHODE.getItemStack()
        );

        /** Silicon Chip **/
        GameRegistry.addRecipe(EnumPart.SILICONCHIP.getItemStack(), 
                " s ",
                "ppp",
                's', EnumPart.INFUSEDSILICON.getItemStack(),
                'p', EnumPart.PLATE.getItemStack()
        );
        
        /** Energized Silicon Chip **/
        GameRegistry.addRecipe(EnumPart.ENERGIZEDSILICONCHIP.getItemStack(), 
                " e ",
                "ppp",
                'e', EnumPart.ENERGIZEDSILICON.getItemStack(),
                'p', EnumPart.PLATE.getItemStack()
        );
        
        /** Platformed Plate **/
        GameRegistry.addRecipe(EnumPart.PLATFORMEDPLATE.getItemStack(), 
                " r ",
                "sps",
                "prp",
                'r', EnumPart.WIREDPLATE.getItemStack(),
                's', Item.stick,
                'p', EnumPart.PLATE.getItemStack()
        );

        
        /** Silicon Boule **/
        AlloySmelterRecipe.add(new AlloySmelterRecipe(new ItemStack[] {
                new ItemStack(Block.sand, 8),
                new ItemStack(Item.coal, 8),
        }, EnumPart.SILICONBOULE.getItemStack(), 500));
        
        /** Silicon **/
        GameRegistry.addRecipe(EnumPart.SILICON.getItemStack(8), 
                "s",
                "b",
                's', new ItemStack(MicroblockProxy.sawDiamond(), 1, Short.MAX_VALUE), 
                'b', EnumPart.SILICONBOULE.getItemStack()
        );
        
        /** Infused Silicon **/
        AlloySmelterRecipe.add(new AlloySmelterRecipe(new ItemStack[] {
                EnumPart.SILICON.getItemStack(),
                new ItemStack(Item.redstone, 4),
        }, EnumPart.INFUSEDSILICON.getItemStack(), 150));
        
        /** Energized Silicon **/
        AlloySmelterRecipe.add(new AlloySmelterRecipe(new ItemStack[] {
                EnumPart.SILICON.getItemStack(),
                new ItemStack(Item.glowstone, 4),
        }, EnumPart.ENERGIZEDSILICON.getItemStack(), 160));
        
        /** Motor **/
        GameRegistry.addRecipe(EnumPart.MOTOR.getItemStack(), 
                " i ",
                "scs",
                "rcr",
                'i', Item.ingotIron,
                's', Block.stone,
                'c', EnumPart.COPPERCOIL.getItemStack(),
                'r', Item.redstone
        );
        
        /** Copper Coil **/
        GameRegistry.addRecipe(new ShapedOreRecipe(EnumPart.COPPERCOIL.getItemStack(), 
                "cd",
                'c', "ingotCopper",
                'd', new ItemStack(ProjectRedCore.itemDrawPlate, 1, Short.MAX_VALUE)
        ));
        
        /** Iron Coil **/
        GameRegistry.addRecipe(EnumPart.IRONCOIL.getItemStack(), 
                "cd",
                'c', new ItemStack(Item.ingotIron),
                'd', new ItemStack(ProjectRedCore.itemDrawPlate, 1, Short.MAX_VALUE)
        );
        
        /** Gold Coil **/
        GameRegistry.addRecipe(EnumPart.GOLDCOIL.getItemStack(), 
                "cd",
                'c', new ItemStack(Item.ingotGold),
                'd', new ItemStack(ProjectRedCore.itemDrawPlate, 1, Short.MAX_VALUE)
        );
        
        /** Red Alloy Ingot **/
        AlloySmelterRecipe.add(new AlloySmelterRecipe(new ItemStack[] {
                new ItemStack(Item.ingotIron),
                new ItemStack(Item.redstone, 4),
        }, EnumPart.REDINGOT.getItemStack(), 125));
        
        /** Illumar **/
        for (int i = 0; i < EnumPart.ILLUMAR_PARTS.length; i++) {
            EnumPart p = EnumPart.ILLUMAR_PARTS[i];
            GameRegistry.addRecipe(new ShapelessOreRecipe(p.getItemStack(), 
                    new ItemStack(Item.glowstone),
                    new ItemStack(Item.glowstone),
                    PRColors.get(i).getOreDict(),
                    PRColors.get(i).getOreDict()
            ));
        }
        
        /** Woven Cloth **/
        GameRegistry.addRecipe(EnumPart.WOVENCLOTH.getItemStack(), 
                "sss",
                "sws",
                "sss",
                's', Item.silk,
                'w', Item.stick
        );
        
        /** Sail **/
        GameRegistry.addRecipe(EnumPart.SAIL.getItemStack(), 
                "ss",
                "ss",
                "ss",
                's', EnumPart.WOVENCLOTH.getItemStack()
        );    
    }

}
