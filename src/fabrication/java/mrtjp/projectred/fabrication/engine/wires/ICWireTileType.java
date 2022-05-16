package mrtjp.projectred.fabrication.engine.wires;

import mrtjp.projectred.fabrication.engine.ICTileType;
import mrtjp.projectred.transmission.WireType;

public enum ICWireTileType {

    //@formatter:off
    RED_ALLOY(ICTileType.RED_ALLOY_WIRE, WireType.RED_ALLOY),

    INSULATED_WHITE     (ICTileType.INSULATED_WHITE_WIRE,      WireType.INSULATED_WHITE),
    INSULATED_ORANGE    (ICTileType.INSULATED_ORANGE_WIRE,     WireType.INSULATED_ORANGE),
    INSULATED_MAGENTA   (ICTileType.INSULATED_MAGENTA_WIRE,    WireType.INSULATED_MAGENTA),
    INSULATED_LIGHT_BLUE(ICTileType.INSULATED_LIGHT_BLUE_WIRE, WireType.INSULATED_LIGHT_BLUE),
    INSULATED_YELLOW    (ICTileType.INSULATED_YELLOW_WIRE,     WireType.INSULATED_YELLOW),
    INSULATED_LIME      (ICTileType.INSULATED_LIME_WIRE,       WireType.INSULATED_LIME),
    INSULATED_PINK      (ICTileType.INSULATED_PINK_WIRE,       WireType.INSULATED_PINK),
    INSULATED_GRAY      (ICTileType.INSULATED_GRAY_WIRE,       WireType.INSULATED_GRAY),
    INSULATED_LIGHT_GRAY(ICTileType.INSULATED_LIGHT_GRAY_WIRE, WireType.INSULATED_LIGHT_GRAY),
    INSULATED_CYAN      (ICTileType.INSULATED_CYAN_WIRE,       WireType.INSULATED_CYAN),
    INSULATED_PURPLE    (ICTileType.INSULATED_PURPLE_WIRE,     WireType.INSULATED_PURPLE),
    INSULATED_BLUE      (ICTileType.INSULATED_BLUE_WIRE,       WireType.INSULATED_BLUE),
    INSULATED_BROWN     (ICTileType.INSULATED_BROWN_WIRE,      WireType.INSULATED_BROWN),
    INSULATED_GREEN     (ICTileType.INSULATED_GREEN_WIRE,      WireType.INSULATED_GREEN),
    INSULATED_RED       (ICTileType.INSULATED_RED_WIRE,        WireType.INSULATED_RED),
    INSULATED_BLACK     (ICTileType.INSULATED_BLACK_WIRE,      WireType.INSULATED_BLACK),

    BUNDLED_NEUTRAL   (ICTileType.BUNDLED_NEUTRAL_WIRE,    WireType.BUNDLED_NEUTRAL),

    BUNDLED_WHITE     (ICTileType.BUNDLED_WHITE_WIRE,      WireType.BUNDLED_WHITE),
    BUNDLED_ORANGE    (ICTileType.BUNDLED_ORANGE_WIRE,     WireType.BUNDLED_ORANGE),
    BUNDLED_MAGENTA   (ICTileType.BUNDLED_MAGENTA_WIRE,    WireType.BUNDLED_MAGENTA),
    BUNDLED_LIGHT_BLUE(ICTileType.BUNDLED_LIGHT_BLUE_WIRE, WireType.BUNDLED_LIGHT_BLUE),
    BUNDLED_YELLOW    (ICTileType.BUNDLED_YELLOW_WIRE,     WireType.BUNDLED_YELLOW),
    BUNDLED_LIME      (ICTileType.BUNDLED_LIME_WIRE,       WireType.BUNDLED_LIME),
    BUNDLED_PINK      (ICTileType.BUNDLED_PINK_WIRE,       WireType.BUNDLED_PINK),
    BUNDLED_GRAY      (ICTileType.BUNDLED_GRAY_WIRE,       WireType.BUNDLED_GRAY),
    BUNDLED_LIGHT_GRAY(ICTileType.BUNDLED_LIGHT_GRAY_WIRE, WireType.BUNDLED_LIGHT_GRAY),
    BUNDLED_CYAN      (ICTileType.BUNDLED_CYAN_WIRE,       WireType.BUNDLED_CYAN),
    BUNDLED_PURPLE    (ICTileType.BUNDLED_PURPLE_WIRE,     WireType.BUNDLED_PURPLE),
    BUNDLED_BLUE      (ICTileType.BUNDLED_BLUE_WIRE,       WireType.BUNDLED_BLUE),
    BUNDLED_BROWN     (ICTileType.BUNDLED_BROWN_WIRE,      WireType.BUNDLED_BROWN),
    BUNDLED_GREEN     (ICTileType.BUNDLED_GREEN_WIRE,      WireType.BUNDLED_GREEN),
    BUNDLED_RED       (ICTileType.BUNDLED_RED_WIRE,        WireType.BUNDLED_RED),
    BUNDLED_BLACK     (ICTileType.BUNDLED_BLACK_WIRE,      WireType.BUNDLED_BLACK),
    ;
    //@formatter:on

    public static final ICWireTileType[] INSULATED = {
            INSULATED_WHITE,
            INSULATED_ORANGE,
            INSULATED_MAGENTA,
            INSULATED_LIGHT_BLUE,
            INSULATED_YELLOW,
            INSULATED_LIME,
            INSULATED_PINK,
            INSULATED_GRAY,
            INSULATED_LIGHT_GRAY,
            INSULATED_CYAN,
            INSULATED_PURPLE,
            INSULATED_BLUE,
            INSULATED_BROWN,
            INSULATED_GREEN,
            INSULATED_RED,
            INSULATED_BLACK };

    public static final ICWireTileType[] BUNDLED_COLOURED = {
            BUNDLED_WHITE,
            BUNDLED_ORANGE,
            BUNDLED_MAGENTA,
            BUNDLED_LIGHT_BLUE,
            BUNDLED_YELLOW,
            BUNDLED_LIME,
            BUNDLED_PINK,
            BUNDLED_GRAY,
            BUNDLED_LIGHT_GRAY,
            BUNDLED_CYAN,
            BUNDLED_PURPLE,
            BUNDLED_BLUE,
            BUNDLED_BROWN,
            BUNDLED_GREEN,
            BUNDLED_RED,
            BUNDLED_BLACK };

    public final ICTileType tileType;
    public final WireType multipartType;

    ICWireTileType(ICTileType tileType, WireType multipartType) {
        this.tileType = tileType;
        this.multipartType = multipartType;
    }
}
