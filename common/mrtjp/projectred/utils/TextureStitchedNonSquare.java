package mrtjp.projectred.utils;

import java.awt.image.BufferedImage;
import java.util.ArrayList;
import java.util.List;

import net.minecraft.client.renderer.texture.Texture;
import net.minecraft.client.renderer.texture.TextureManager;
import net.minecraft.client.renderer.texture.TextureStitched;
import net.minecraft.client.texturepacks.ITexturePack;

public class TextureStitchedNonSquare extends TextureStitched {
	public TextureStitchedNonSquare(String par1) {
		super(par1);
	}
	
	private int unpaddedW, unpaddedH;
	
	@Override
	public void init(Texture textureSheet, List par2List, int x, int y, int w, int h, boolean par7) {
		w = unpaddedW;
		h = unpaddedH;
		super.init(textureSheet, par2List, x, y, w, h, par7);
	}

	@Override
	public boolean loadTexture(TextureManager manager, ITexturePack texturepack, String name, String fileName, BufferedImage image, ArrayList textures) {
		// pad image to square
		
		int newSize = Math.max(image.getWidth(), image.getHeight());
		
		unpaddedW = image.getWidth();
		unpaddedH = image.getHeight();
		
		BufferedImage newImage = new BufferedImage(newSize, newSize, BufferedImage.TYPE_INT_ARGB);
		
		int[] rgb = new int[image.getWidth() * image.getHeight()];
		image.getRGB(0, 0, image.getWidth(), image.getHeight(), rgb, 0, image.getWidth());
		newImage.setRGB(0, 0, image.getWidth(), image.getHeight(), rgb, 0, image.getWidth());
		
		textures.add(manager.makeTexture(name, 2, newSize, newSize, 10496, 6408, 9728, 9728, false, newImage));
		return true;
	}
}
