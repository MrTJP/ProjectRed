package mrtjp.projectred.redui;

import mrtjp.projectred.core.ProjectRedCore;
import net.minecraft.resources.ResourceLocation;

public class RedUISprites {

    public static final ResourceLocation REDUI_WIDGETS_TEXTURE = new ResourceLocation(ProjectRedCore.MOD_ID, "textures/gui/redui_widgets.png");
    public static final int REDUI_WIDGETS_TEX_SIZE = 256;

    // Checkbox
    public static final RedUISprite[] CHECKBOX_SPRITES = {
            // Disabled
            new RedUISprite(REDUI_WIDGETS_TEXTURE, 0,  0, 18, 18, REDUI_WIDGETS_TEX_SIZE, REDUI_WIDGETS_TEX_SIZE), // Unchecked
            new RedUISprite(REDUI_WIDGETS_TEXTURE, 0, 18, 18, 18, REDUI_WIDGETS_TEX_SIZE, REDUI_WIDGETS_TEX_SIZE), // Checked

            // Idle
            new RedUISprite(REDUI_WIDGETS_TEXTURE, 18,  0, 18, 18, REDUI_WIDGETS_TEX_SIZE, REDUI_WIDGETS_TEX_SIZE),
            new RedUISprite(REDUI_WIDGETS_TEXTURE, 18, 18, 18, 18, REDUI_WIDGETS_TEX_SIZE, REDUI_WIDGETS_TEX_SIZE),

            // Highlight
            new RedUISprite(REDUI_WIDGETS_TEXTURE, 36,  0, 18, 18, REDUI_WIDGETS_TEX_SIZE, REDUI_WIDGETS_TEX_SIZE),
            new RedUISprite(REDUI_WIDGETS_TEXTURE, 36, 18, 18, 18, REDUI_WIDGETS_TEX_SIZE, REDUI_WIDGETS_TEX_SIZE)
    };

    public static RedUISprite getCheckboxSprite(boolean disabled, boolean mouseover, boolean checked) {

        int a = disabled ? 0 : mouseover ? 2 : 1;
        int b = checked ? 1 : 0;

        return CHECKBOX_SPRITES[a * 2 + b];
    }

}
