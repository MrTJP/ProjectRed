package coloredlightscore.src.api;

import net.minecraft.block.Block;

/**
 * Public API for ColoredLightsCore
 * 
 * @author CptSpaceToaster
 *
 */
public class CLApi {
    public static float l[] = new float[] { 0F, 1F / 15, 2F / 15, 3F / 15, 4F / 15, 5F / 15, 6F / 15, 7F / 15, 8F / 15, 9F / 15, 10F / 15, 11F / 15, 12F / 15, 13F / 15, 14F / 15, 1F };

    public static int r[] = new int[] { 0, 15, 0, 8, 0, 10, 0, 10, 5, 15, 8, 15, 0, 15, 15, 15 };
    public static int g[] = new int[] { 0, 0, 15, 3, 0, 0, 15, 10, 5, 10, 15, 15, 8, 0, 12, 15 };
    public static int b[] = new int[] { 0, 0, 0, 0, 15, 15, 15, 10, 5, 13, 0, 0, 15, 15, 10, 15 };

    /**
     * Computes a 20-bit lighting word, containing red, green, blue settings, and brightness settings.
     * Automatically computes the Minecraft brightness value using the brightest of the r, g and b channels.
     * This value can be used directly for Block.lightValue 
     * 
     * Word format: 0RRRR 0GGGG 0BBBB 0LLLL
     * 
     * @param r Red intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @param g Green intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @param b Blue intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @return Integer describing RGB color for a block
     */
    public static int makeRGBLightValue(float r, float g, float b) {
        // Clamp color channels
        if (r < 0.0f)
            r = 0.0f;
        else if (r > 1.0f)
            r = 1.0f;

        if (g < 0.0f)
            g = 0.0f;
        else if (g > 1.0f)
            g = 1.0f;

        if (b < 0.0f)
            b = 0.0f;
        else if (b > 1.0f)
            b = 1.0f;

        int brightness = (int) (15.0f * Math.max(Math.max(r, g), b));
        return brightness | ((((int) (15.0F * b)) << 15) + (((int) (15.0F * g)) << 10) + (((int) (15.0F * r)) << 5));
    }

    /**
     * Computes a 20-bit lighting word, containing red, green, blue settings, and brightness settings.
     * Automatically computes the Minecraft brightness value using the brightest of the r, g and b channels.
     * This value can be used directly for Block.lightValue 
     * 
     * Word format: 0RRRR 0GGGG 0BBBB 0LLLL
     * 
     * @param r Red intensity, 0 to 15. Resolution is 4 bits.
     * @param g Green intensity, 0 to 15. Resolution is 4 bits.
     * @param b Blue intensity, 0 to 15. Resolution is 4 bits.
     * @return Integer describing RGB color for a block
     */
    public static int makeRGBLightValue(int r, int g, int b) {
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

        int brightness = Math.max(Math.max(r, g), b);
        return brightness | ((b << 15) + (g << 10) + (r << 5));
    }

    /**
     * It is not recommended that you mess with lightValue yourself
     *
     * Computes a 20-bit lighting word, containing red, green, blue, and brightness settings.
     * Allows overriding of the Minecraft brightness value.
     * This value can be used directly for Block.lightValue
     *
     * Word format: 0RRRR 0GGGG 0BBBB 0LLLL
     *
     * @param r Red intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @param g Green intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @param b Blue intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @param currentLightValue The existing lightValue of a block. Only the lower-most 4 bits of this parameter are used.
     * @return Integer describing RGBL color for a block
     */
    @Deprecated
    public static int makeRGBLightValue(float r, float g, float b, float currentLightValue) {
        // Clamp color channels
        if (r < 0.0f)
            r = 0.0f;
        else if (r > 1.0f)
            r = 1.0f;

        if (g < 0.0f)
            g = 0.0f;
        else if (g > 1.0f)
            g = 1.0f;

        if (b < 0.0f)
            b = 0.0f;
        else if (b > 1.0f)
            b = 1.0f;

        int brightness = (int) (currentLightValue * 15.0f);
        brightness &= 0xf;

        return brightness | ((((int) (15.0F * b)) << 15) + (((int) (15.0F * g)) << 10) + (((int) (15.0F * r)) << 5));
    }

    /**
     * It is not recommended that you mess with lightValue yourself
     *
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
    @Deprecated
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

    /**
     * Sets the lighting colors for a given block. Vanilla brightness is recomputed.
     *
     * @param block The block to set color on
     * @param r Red intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @param g Green intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @param b Blue intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @return Reference to the block passed in.
     */
    public static Block setBlockColorRGB(Block block, int r, int g, int b) {
        block.setLightLevel(((float)makeRGBLightValue(r, g, b))/15F);
        return block;
    }

    /**
     * Sets the lighting colors for a given block. Vanilla brightness is recomputed.
     *
     * @param block The block to set color on
     * @param r Red intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @param g Green intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @param b Blue intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @return Reference to the block passed in.
     */
    public static Block setBlockColorRGB(Block block, float r, float g, float b) {
        block.setLightLevel(((float)makeRGBLightValue(r, g, b))/15F);
        return block;
    }

    /**
     * It is not recommended that you mess with lightValue yourself
     *
     * Sets the lighting colors for a given block.
     *
     * @param block The block to set color on
     * @param r Red intensity, 0 to 15. Resolution is 4 bits.
     * @param g Green intensity, 0 to 15. Resolution is 4 bits.
     * @param b Blue intensity, 0 to 15. Resolution is 4 bits.
     * @param lightValue The Minecraft brightness to set for the block, 0 to 15.
     * @return Reference to the block passed in.
     */
    @Deprecated
    public static Block setBlockColorRGB(Block block, int r, int g, int b, int lightValue) {
        block.setLightLevel(((float)makeRGBLightValue(r, g, b, lightValue))/15F);
        return block;
    }

    /**
     * It is not recommended that you mess with lightValue yourself
     *
     * Sets the lighting colors for a given block.
     *
     * @param block The block to set color on
     * @param r Red intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @param g Green intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @param b Blue intensity, 0.0f to 1.0f. Resolution is 4 bits.
     * @param lightValue The Minecraft brightness to set for the block, 0.0f to 1.0f.
     * @return Reference to the block passed in.
     */
    @Deprecated
    public static Block setBlockColorRGB(Block block, float r, float g, float b, float lightValue) {
        block.setLightLevel(((float)makeRGBLightValue(r, g, b, lightValue))/15F);
        return block;
    }

}
