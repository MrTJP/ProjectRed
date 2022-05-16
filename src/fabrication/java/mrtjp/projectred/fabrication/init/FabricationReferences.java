package mrtjp.projectred.fabrication.init;

import codechicken.multipart.api.MultiPartType;
import mrtjp.projectred.fabrication.block.FabricationBaseBlock;
import mrtjp.projectred.fabrication.item.ICBlueprintItem;
import mrtjp.projectred.fabrication.part.FabricatedGatePart;
import mrtjp.projectred.fabrication.tile.ICWorkbenchTile;
import mrtjp.projectred.integration.ItemPartGate;
import net.minecraft.tileentity.TileEntityType;
import net.minecraftforge.registries.ObjectHolder;

import static mrtjp.projectred.ProjectRedFabrication.MOD_ID;
import static mrtjp.projectred.fabrication.init.FabricationBlocks.ID_IC_WORKBENCH;
import static mrtjp.projectred.fabrication.init.FabricationItems.ID_IC_BLUEPRINT;
import static mrtjp.projectred.fabrication.init.FabricationParts.ID_FABRICATED_GATE;

@ObjectHolder(MOD_ID)
public class FabricationReferences {

    // Blocks
    @ObjectHolder(ID_IC_WORKBENCH)
    public static FabricationBaseBlock IC_WORKBENCH_BLOCK = null;
    @ObjectHolder(ID_IC_WORKBENCH)
    public static TileEntityType<ICWorkbenchTile> IC_WORKBENCH_TILE = null;

    // Items
    @ObjectHolder(ID_IC_BLUEPRINT)
    public static ICBlueprintItem IC_BLUEPRINT_ITEM = null;

    // Parts
    @ObjectHolder(ID_FABRICATED_GATE)
    public static ItemPartGate FABRICATED_GATE_ITEM = null;
    @ObjectHolder(ID_FABRICATED_GATE)
    public static MultiPartType<?> FABRICATED_GATE_PART = null;
}
