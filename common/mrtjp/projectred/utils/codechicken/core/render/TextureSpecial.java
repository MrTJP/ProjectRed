package mrtjp.projectred.utils.codechicken.core.render;

import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

import mrtjp.projectred.utils.codechicken.core.render.SpriteSheetManager.SpriteSheet;
import mrtjp.projectred.utils.codechicken.core.render.TextureUtils.IIconRegister;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.client.renderer.texture.Texture;
import net.minecraft.client.renderer.texture.TextureManager;
import net.minecraft.client.renderer.texture.TextureMap;
import net.minecraft.client.renderer.texture.TextureStitched;
import net.minecraft.client.texturepacks.ITexturePack;
import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

@SideOnly(Side.CLIENT)
public class TextureSpecial extends TextureStitched implements IIconRegister
{
    //sprite sheet fields
    private int spriteIndex;
    private SpriteSheet spriteSheet;
    
    //textureFX fields
    private TextureFX textureFX;
    
    private int blankSize = -1;
    
    private String textureFile;
    
    private ArrayList<Texture> baseTextures;
    
    private boolean vanillaAnimate;
    private boolean selfRegister;
    public int atlasIndex;
    
    protected TextureSpecial(String par1)
    {
        super(par1);
    }
    
    public TextureSpecial addTexture(Texture t)
    {
        if(baseTextures == null)
            baseTextures = new ArrayList<Texture>();
        baseTextures.add(t);
        return this;
    }
    
    public TextureSpecial baseFromSheet(SpriteSheet spriteSheet, int spriteIndex)
    {
        this.spriteSheet = spriteSheet;
        this.spriteIndex = spriteIndex;
        return this;
    }
    
    public TextureSpecial addTextureFX(TextureFX fx)
    {
        textureFX = fx;
        return this;
    }
    
    @Override
    public void init(Texture par1Texture, List par2List, int originX, int originY, int width, int height, boolean par7)
    {
        super.init(par1Texture, par2List, originX, originY, width, height, par7);
        if(textureFX != null)
            textureFX.onTextureDimensionsUpdate(width, height);
        else if(textureFile != null && vanillaAnimate && textureList.size() > 0)
        {
            try
            {
                InputStream in = TextureUtils.getTextureResource(textureFile.replace(".png", ".txt"));
                readAnimationInfo(new BufferedReader(new InputStreamReader(in)));
            }
            catch(Exception e){}
        }
    }
    
    @Override
    public void updateAnimation()
    {
        if(textureFX != null)
        {
            textureFX.update();
            if(textureFX.changed())
                TextureUtils.write(textureFX.imageData, textureFX.tileSizeBase, textureFX.tileSizeBase, textureSheet, originX, originY);
        }
        else if(vanillaAnimate)
        {
            super.updateAnimation();
        }
    }
    
    @Override
    public boolean loadTexture(TextureManager manager, ITexturePack texturepack, String name, String fileName, BufferedImage image, ArrayList textures)
    {
        if(baseTextures != null)
            textures.addAll(baseTextures);
        
        if(spriteSheet != null)
            textures.add(spriteSheet.createSprite(spriteIndex));
        else if(blankSize > 0)
            textures.add(TextureUtils.createTextureObject(name, blankSize, blankSize));
        else if(textureFile != null)
        {
            Texture tex = TextureUtils.createTextureObject(textureFile);
            int width = tex.getWidth();
            int height = tex.getHeight();
            if(vanillaAnimate && height > width)
            {
                int frames = height / width;

                for (int frame = 0; frame < frames; ++frame)
                {
                    Texture frameTex = TextureUtils.createTextureObject(name, width, width);
                    TextureUtils.copySubImg(tex, 0, width * frame, width, width, frameTex, 0, 0);
                    textures.add(frameTex);
                }
            }
            else
            {
                textures.add(tex);
            }                
        }
        
        if(textureFX != null)
        {
            if(textures.isEmpty())
                throw new RuntimeException("TextureFX with no base sprite: "+name);
            Texture base = (Texture) textures.get(0);
            //add 2 so we get the update call
            textures.add(TextureUtils.createTextureObject(name+"$2", base.getWidth(), base.getHeight()));
        }
        
        if(!textures.isEmpty())
            return true;
        
        return super.loadTexture(manager, texturepack, name, fileName, image, textures);
    }

    public TextureSpecial blank(int size)
    {
        blankSize = size;
        return this;
    }
    
    public TextureSpecial setTextureFile(String fileName)
    {
        textureFile = fileName;
        return this;
    }
    
    public TextureSpecial selfRegister()
    {
        selfRegister = true;
        TextureUtils.addIconRegistrar(this);
        return this;
    }
    
    @Override
    public void registerIcons(IconRegister register)
    {
        if(selfRegister)
            ((TextureMap)register).setTextureEntry(getIconName(), this);
    }
    
    @Override
    public int atlasIndex()
    {
        return atlasIndex;
    }

    public TextureSpecial useVanillaAnimation(boolean b)
    {
        vanillaAnimate = b;
        return this;
    }
}
