package mrtjp.projectred.fabrication.engine;

import mrtjp.projectred.fabrication.engine.gates.*;
import mrtjp.projectred.fabrication.engine.wires.BundledWireTile;
import mrtjp.projectred.fabrication.engine.wires.InsulatedWireTile;
import mrtjp.projectred.fabrication.engine.wires.RedAlloyWireTile;

import java.util.function.Supplier;

import static mrtjp.projectred.fabrication.engine.ICTileTypeOffsets.*;

public enum ICTileType {

    //@formatter:off
    IO_GATE(ID_OFFSET_IOGATE, "IO Gate", IOGateTile::new),

    OR_GATE             (ID_OFFSET_GATE,      "OR Gate", ORGateTile::new),
    NOR_GATE            (ID_OFFSET_GATE + 1,  "NOR Gate", NORGateTile::new),
    NOT_GATE            (ID_OFFSET_GATE + 2,  "NOT Gate", NOTGateTile::new),
    AND_GATE            (ID_OFFSET_GATE + 3,  "AND Gate", ANDGateTile::new),
    NAND_GATE           (ID_OFFSET_GATE + 4,  "NAND Gate", NANDGateTile::new),
    XOR_GATE            (ID_OFFSET_GATE + 5,  "XOR Gate", XORGateTile::new),
    XNOR_GATE           (ID_OFFSET_GATE + 6,  "XOR Gate", XNORGateTile::new),
    BUFFER_GATE         (ID_OFFSET_GATE + 7,  "Buffer Gate", BufferGateTile::new),
    MULTIPLEXER_GATE    (ID_OFFSET_GATE + 8,  "Multiplexer Gate", MultiplexerGateTile::new),
    PULSE_GATE          (ID_OFFSET_GATE + 9,  "Pulse Gate", PulseGateTile::new),
    REPEATER_GATE       (ID_OFFSET_GATE + 10, "Repeater Gate", RepeaterGateTile::new),
    RANDOMIZER_GATE     (ID_OFFSET_GATE + 11, "Randomizer Gate", RandomizerGateTile::new),
    SR_LATCH_GATE       (ID_OFFSET_GATE + 12, "SR Latch Gate", SRLatchGateTile::new),
    TOGGLE_LATCH_GATE   (ID_OFFSET_GATE + 13, "Toggle Latch Gate", ToggleLatchGateTile::new),
    TRANSPARENT_LATCH_GATE(ID_OFFSET_GATE + 14, "Transparent Latch Gate", TransparentLatchGateTile::new),

    RED_ALLOY_WIRE           (ID_OFFSET_WIRE_RED,      "Red Alloy Wire", RedAlloyWireTile::new),
    INSULATED_WHITE_WIRE     (ID_OFFSET_WIRE_RED + 1,  "White Insulated Wire", () -> new InsulatedWireTile(0)),
    INSULATED_ORANGE_WIRE    (ID_OFFSET_WIRE_RED + 2,  "Orange Insulated Wire", () -> new InsulatedWireTile(1)),
    INSULATED_MAGENTA_WIRE   (ID_OFFSET_WIRE_RED + 3,  "Magenta Insulated Wire", () -> new InsulatedWireTile(2)),
    INSULATED_LIGHT_BLUE_WIRE(ID_OFFSET_WIRE_RED + 4,  "Light Blue Insulated Wire", () -> new InsulatedWireTile(3)),
    INSULATED_YELLOW_WIRE    (ID_OFFSET_WIRE_RED + 5,  "Yellow Insulated Wire", () -> new InsulatedWireTile(4)),
    INSULATED_LIME_WIRE      (ID_OFFSET_WIRE_RED + 6,  "Lime Insulated Wire", () -> new InsulatedWireTile(5)),
    INSULATED_PINK_WIRE      (ID_OFFSET_WIRE_RED + 7,  "Pink Insulated Wire", () -> new InsulatedWireTile(6)),
    INSULATED_GRAY_WIRE      (ID_OFFSET_WIRE_RED + 8,  "Gray Insulated Wire", () -> new InsulatedWireTile(7)),
    INSULATED_LIGHT_GRAY_WIRE(ID_OFFSET_WIRE_RED + 9,  "Light Gray Insulated Wire", () -> new InsulatedWireTile(8)),
    INSULATED_CYAN_WIRE      (ID_OFFSET_WIRE_RED + 10, "Cyan Insulated Wire", () -> new InsulatedWireTile(9)),
    INSULATED_PURPLE_WIRE    (ID_OFFSET_WIRE_RED + 11, "Purple Insulated Wire", () -> new InsulatedWireTile(10)),
    INSULATED_BLUE_WIRE      (ID_OFFSET_WIRE_RED + 12, "Blue Insulated Wire", () -> new InsulatedWireTile(11)),
    INSULATED_BROWN_WIRE     (ID_OFFSET_WIRE_RED + 13, "Brown Insulated Wire", () -> new InsulatedWireTile(12)),
    INSULATED_GREEN_WIRE     (ID_OFFSET_WIRE_RED + 14, "Green Insulated Wire", () -> new InsulatedWireTile(13)),
    INSULATED_RED_WIRE       (ID_OFFSET_WIRE_RED + 15, "Red Insulated Wire", () -> new InsulatedWireTile(14)),
    INSULATED_BLACK_WIRE     (ID_OFFSET_WIRE_RED + 16, "Black Insulated Wire", () -> new InsulatedWireTile(15)),

    BUNDLED_NEUTRAL_WIRE   (ID_OFFSET_WIRE_BUNDLED,      "Bundled Wire", () -> new BundledWireTile(-1)),
    BUNDLED_WHITE_WIRE     (ID_OFFSET_WIRE_BUNDLED + 1,  "White Bundled Wire", () -> new BundledWireTile(0)),
    BUNDLED_ORANGE_WIRE    (ID_OFFSET_WIRE_BUNDLED + 2,  "Orange Bundled Wire", () -> new BundledWireTile(1)),
    BUNDLED_MAGENTA_WIRE   (ID_OFFSET_WIRE_BUNDLED + 3,  "Magenta Bundled Wire", () -> new BundledWireTile(2)),
    BUNDLED_LIGHT_BLUE_WIRE(ID_OFFSET_WIRE_BUNDLED + 4,  "Light Blue Bundled Wire", () -> new BundledWireTile(3)),
    BUNDLED_YELLOW_WIRE    (ID_OFFSET_WIRE_BUNDLED + 5,  "Yellow Bundled Wire", () -> new BundledWireTile(4)),
    BUNDLED_LIME_WIRE      (ID_OFFSET_WIRE_BUNDLED + 6,  "Lime Bundled Wire", () -> new BundledWireTile(5)),
    BUNDLED_PINK_WIRE      (ID_OFFSET_WIRE_BUNDLED + 7,  "Pink Bundled Wire", () -> new BundledWireTile(6)),
    BUNDLED_GRAY_WIRE      (ID_OFFSET_WIRE_BUNDLED + 8,  "Gray Bundled Wire", () -> new BundledWireTile(7)),
    BUNDLED_LIGHT_GRAY_WIRE(ID_OFFSET_WIRE_BUNDLED + 9,  "Light Gray Bundled Wire", () -> new BundledWireTile(8)),
    BUNDLED_CYAN_WIRE      (ID_OFFSET_WIRE_BUNDLED + 10, "Cyan Bundled Wire", () -> new BundledWireTile(9)),
    BUNDLED_PURPLE_WIRE    (ID_OFFSET_WIRE_BUNDLED + 11, "Purple Bundled Wire", () -> new BundledWireTile(10)),
    BUNDLED_BLUE_WIRE      (ID_OFFSET_WIRE_BUNDLED + 12, "Blue Bundled Wire", () -> new BundledWireTile(11)),
    BUNDLED_BROWN_WIRE     (ID_OFFSET_WIRE_BUNDLED + 13, "Brown Bundled Wire", () -> new BundledWireTile(12)),
    BUNDLED_GREEN_WIRE     (ID_OFFSET_WIRE_BUNDLED + 14, "Green Bundled Wire", () -> new BundledWireTile(13)),
    BUNDLED_RED_WIRE       (ID_OFFSET_WIRE_BUNDLED + 15, "Red Bundled Wire", () -> new BundledWireTile(14)),
    BUNDLED_BLACK_WIRE     (ID_OFFSET_WIRE_BUNDLED + 16, "Black Bundled Wire", () -> new BundledWireTile(15));
    //@formatter:on

    private static final ICTileType[] VALUES_BY_ID;

    static {
        VALUES_BY_ID = new ICTileType[ID_OFFSET_MAX + 1];
        for (ICTileType type : ICTileType.values()) {
            VALUES_BY_ID[type.getID()] = type;
        }
    }

    private final int id;
    private final String name;
    private final Supplier<BaseTile> factory;

    ICTileType(int id, String name, Supplier<BaseTile> factory) {
        this.id = id;
        this.name = name; //TODO localize
        this.factory = factory;
    }

    public String getName() {
        return name;
    }

    public BaseTile create() {
        return factory.get();
    }

    public int getID() {
        return id;
    }

    public static BaseTile createFromId(int id) {
        return VALUES_BY_ID[id].create();
    }
}
