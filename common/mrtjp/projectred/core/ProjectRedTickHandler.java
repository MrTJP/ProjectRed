package mrtjp.projectred.core;

import java.util.EnumSet;

import cpw.mods.fml.common.ITickHandler;
import cpw.mods.fml.common.TickType;

public class ProjectRedTickHandler implements ITickHandler {
	
	public static final ProjectRedTickHandler instance = new ProjectRedTickHandler();
	
	public static float degreeRotation = 0F;
	public static float radianRotation = 0F;
	
	public static float degreeRate = 3F;
	public static float radianRate = (float) (Math.PI/15);
	
	public static float maxDegree = 360F;
	public static float maxRadian = (float) (2*Math.PI);
	
	@Override
	public void tickStart(EnumSet<TickType> type, Object... tickData) {

	}

	@Override
	public void tickEnd(EnumSet<TickType> type, Object... tickData) {
		if (type.contains(TickType.CLIENT)) {
			degreeRotation = degreeRotation + degreeRate;
			if (degreeRotation > maxDegree) {
				degreeRotation = 0;
			}
			radianRotation = radianRotation + radianRate;
			if (radianRotation > maxRadian) {
				radianRotation = 0;
			}
		}
	}

	@Override
	public EnumSet<TickType> ticks() {
		return EnumSet.of(TickType.CLIENT);
	}

	@Override
	public String getLabel() {
		return "Project: Red client ticks";
	}

}
