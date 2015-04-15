/*
 * Copyright (c) 2015.
 * Created by MrTJP.
 * All rights reserved.
 */
package mrtjp.projectred.compatibility.cptcoloredlights;

/**
 * Workaround for CLC's very weirdly structured API.
 */
public class CLCAPIProxy
{
    /**
     * Computes a 20-bit lighting word, containing red, green, blue, and brightness settings.
     * Allows overriding of the Minecraft brightness value.
     * This value can be used directly for Block.lightValue
     *
     * Word format: 0RRRR 0GGGG 0BBBB 0LLLL
     *
     * @param r Red intensity, 0 to 15. Resolution is 4 bits.
     * @param g Green intensity, 0 to 15. Resolution is 4 bits.
     * @param b Blue intensity, 0 to 15. Resolution is 4 bits.
     * @param brightness The existing lightValue of a block. Only the lower-most 4 bits of this parameter are used.
     * @return Integer describing RGBL color for a block
     */
    public static int makeRGBLightValue(int r, int g, int b, int brightness) {
        // Clamp color channels
        if (r < 0)
            r = 0;
        else if (r > 15)
            r = 15;

        if (g < 0)
            g = 0;
        else if (g > 15)
            g = 15;

        if (b < 0)
            b = 0;
        else if (b > 15)
            b = 15;

        brightness &= 0xf;

        return brightness | ((b << 15) + (g << 10) + (r << 5));
    }
}