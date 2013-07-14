package mrtjp.projectred.utils.codechicken.core.render;

import java.awt.image.BufferedImage;
import java.util.ArrayList;

import net.minecraft.client.renderer.texture.TextureManager;
import net.minecraft.client.renderer.texture.TextureStitched;
import net.minecraft.client.texturepacks.ITexturePack;

public class PlaceholderTexture extends TextureStitched
{
    protected PlaceholderTexture(String par1)
    {
        super(par1);
    }
    
    @Override
    public boolean loadTexture(TextureManager manager, ITexturePack texturepack, String name, String fileName, BufferedImage image, ArrayList textures)
    {
        return true;
    }
}
