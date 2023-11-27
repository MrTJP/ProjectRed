package mrtjp.projectred.fabrication.engine;

public class ICTileTypeOffsets {
    public static final int MAX_IO_GATES           = 16;
    public static final int MAX_GATES              = 64;
    public static final int MAX_REDSTONE_WIRES     = 20;
    public static final int MAX_BUNDLED_WIRES      = 20;

    public static final int ID_OFFSET_IOGATE       = 0;
    public static final int ID_OFFSET_GATE         = ID_OFFSET_IOGATE + MAX_IO_GATES;
    public static final int ID_OFFSET_WIRE_RED     = ID_OFFSET_GATE + MAX_GATES;
    public static final int ID_OFFSET_WIRE_BUNDLED = ID_OFFSET_WIRE_RED + MAX_REDSTONE_WIRES;

    public static final int ID_OFFSET_MAX          = ID_OFFSET_WIRE_BUNDLED + MAX_BUNDLED_WIRES;
}
