package mrtjp.projectred.utils.codechicken.core.render;

import java.util.ArrayList;
import java.util.HashMap;

import mrtjp.projectred.utils.codechicken.core.render.TextureUtils.IIconRegister;
import net.minecraft.client.renderer.texture.IconRegister;
import net.minecraft.client.renderer.texture.Texture;
import net.minecraft.client.renderer.texture.TextureMap;
import net.minecraft.util.Icon;

import org.lwjgl.util.Dimension;

import cpw.mods.fml.relauncher.Side;
import cpw.mods.fml.relauncher.SideOnly;

public class SpriteSheetManager
{
    @SideOnly(Side.CLIENT)
    public static class SpriteSheet implements IIconRegister
    {
        private int tilesX;
        private int tilesY;
        private ArrayList<Integer> newSprites = new ArrayList<Integer>();
        private TextureSpecial[] sprites;
        private String textureFile;
        private Texture texture;
        private int spriteWidth;
        private int spriteHeight;
        
        public int atlasIndex;
        
        private SpriteSheet(int tilesX, int tilesY, String textureFile)
        {
            this.tilesX = tilesX;
            this.tilesY = tilesY;
            this.textureFile = textureFile;
            sprites = new TextureSpecial[tilesX*tilesY];
        }
        
        public void requestIndicies(int... indicies)
        {
            for(int i : indicies)
                setupSprite(i);
        }
        
        public void registerIcons(IconRegister register)
        {
            TextureMap textureMap = (TextureMap)register;
            
            if(TextureUtils.refreshTexture(textureMap, textureFile))
            {
                reloadTexture();
                for(int i = 0; i < sprites.length; i++)
                    if(sprites[i] != null)
                        textureMap.setTextureEntry(sprites[i].getIconName(), sprites[i]);
            }
            else
            {
                for(int i : newSprites)
                    textureMap.setTextureEntry(sprites[i].getIconName(), sprites[i]);
            }
            newSprites.clear();
        }
        
        public TextureSpecial setupSprite(int i)
        {
            if(sprites[i] == null)
            {
                String name = textureFile+"_"+i;
                sprites[i] = new TextureSpecial(name).baseFromSheet(this, i);
                newSprites.add(i);
            }
            return sprites[i];
        }

        private void reloadTexture()
        {
            texture = TextureUtils.createTextureObject(textureFile);
            Dimension dim = TextureUtils.getTextureDimension(textureFile);
            spriteWidth = dim.getWidth()/tilesX;
            spriteHeight = dim.getHeight()/tilesY;
        }

        public Icon getSprite(int index)
        {
            Icon i = sprites[index];
            if(i == null)
                throw new IllegalArgumentException("Sprite at index: "+index+" from texture file "+textureFile+" was not preloaded.");
            return i;
        }

        public Texture createSprite(int spriteIndex)
        {
            int sx = spriteIndex%tilesX;
            int sy = spriteIndex/tilesX;
            Texture sprite = TextureUtils.createTextureObject(textureFile+"_"+spriteIndex, spriteWidth, spriteHeight);            
            TextureUtils.copySubImg(texture, sx*spriteWidth, sy*spriteHeight, spriteWidth, spriteHeight, sprite, 0, 0);
            return sprite;
        }

        public int spriteWidth()
        {
            return spriteWidth;
        }
        
        public int spriteHeight()
        {
            return spriteHeight;
        }

        public TextureSpecial bindTextureFX(int i, TextureFX textureFX)
        {
            return setupSprite(i).addTextureFX(textureFX);
        }

        public SpriteSheet selfRegister(int atlas)
        {
            TextureUtils.addIconRegistrar(this);
            return this;
        }
        
        @Override
        public int atlasIndex()
        {
            return atlasIndex;
        }
    }
    
    private static HashMap<String, SpriteSheet> spriteSheets = new HashMap<String, SpriteSheet>();
    
    public static SpriteSheet getSheet(String textureFile)
    {
        return getSheet(16, 16, textureFile);
    }

    public static SpriteSheet getSheet(int tilesX, int tilesY, String textureFile)
    {
        SpriteSheet sheet = spriteSheets.get(textureFile);
        if(sheet == null)
            spriteSheets.put(textureFile, sheet = new SpriteSheet(tilesX, tilesY, textureFile));
        return sheet;
    }    
}
