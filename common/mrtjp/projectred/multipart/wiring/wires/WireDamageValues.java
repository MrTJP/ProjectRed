package mrtjp.projectred.multipart.wiring.wires;

public class WireDamageValues {
	// things controlling interpretation of item damage values
	public static final int DMG_FLAG_JACKETED = 16384;
	public static final int DMG_MASK_ORDINAL = 255;

	public static boolean isJacketed(int damageValue) {
		return (damageValue & DMG_FLAG_JACKETED) != 0;
	}

	public static EnumWire getType(int damageValue) {
		int ordinal = damageValue & DMG_MASK_ORDINAL;
		if (ordinal < 0 || ordinal >= EnumWire.VALUES.length) {
			return null;
		}
		return EnumWire.VALUES[ordinal];
	}

}
