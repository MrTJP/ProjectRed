package mrtjp.projectred.exploration;

import net.minecraft.item.ItemSpade;
import net.minecraft.item.ItemStack;

/*
 * Ignore any missing class errors and side-effects; they are created at compile time only.
 * This class mirrors what would be produced by the Scala compiler.
 */
public class ItemGemShovel extends ItemSpade implements TGemTool {
    private final ToolDefs.ToolVal tool;

    public ItemGemShovel(ToolDefs.ToolVal tool) {
        super(tool.mat());
        this.tool = tool;
        TGemTool$class.$init$(this);
    }

    public boolean mrtjp$projectred$exploration$TGemTool$$super$getIsRepairable(ItemStack x$1, ItemStack x$2) {
        return super.getIsRepairable(x$1, x$2);
    }

    public boolean getIsRepairable(ItemStack ist1, ItemStack ist2) {
        return TGemTool$class.getIsRepairable(this, ist1, ist2);
    }

    public ToolDefs.ToolVal tool() {
        return this.tool;
    }
}