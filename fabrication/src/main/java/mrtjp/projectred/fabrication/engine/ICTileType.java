package mrtjp.projectred.fabrication.engine;

import mrtjp.projectred.fabrication.engine.gates.*;
import mrtjp.projectred.fabrication.engine.wires.BundledWireTile;
import mrtjp.projectred.fabrication.engine.wires.InsulatedWireTile;
import mrtjp.projectred.fabrication.engine.wires.RedAlloyWireTile;
import mrtjp.projectred.integration.GateType;
import mrtjp.projectred.transmission.WireType;

import java.util.function.Supplier;

import static mrtjp.projectred.fabrication.engine.ICTileTypeOffsets.*;
import static mrtjp.projectred.fabrication.init.FabricationUnlocal.UL_IO_GATE_TILE;
import static mrtjp.projectred.integration.GateType.*;
import static mrtjp.projectred.transmission.WireType.*;

public enum ICTileType {

    //@formatter:off
    IO_GATE(ID_OFFSET_IOGATE, UL_IO_GATE_TILE, IOGateTile::new),

    OR_GATE               (ID_OFFSET_GATE,      OR,                ORGateTile::new),
    NOR_GATE              (ID_OFFSET_GATE + 1,  NOR,               NORGateTile::new),
    NOT_GATE              (ID_OFFSET_GATE + 2,  NOT,               NOTGateTile::new),
    AND_GATE              (ID_OFFSET_GATE + 3,  AND,               ANDGateTile::new),
    NAND_GATE             (ID_OFFSET_GATE + 4,  NAND,              NANDGateTile::new),
    XOR_GATE              (ID_OFFSET_GATE + 5,  XOR,               XORGateTile::new),
    XNOR_GATE             (ID_OFFSET_GATE + 6,  XNOR,              XNORGateTile::new),
    BUFFER_GATE           (ID_OFFSET_GATE + 7,  BUFFER,            BufferGateTile::new),
    MULTIPLEXER_GATE      (ID_OFFSET_GATE + 8,  MULTIPLEXER,       MultiplexerGateTile::new),
    PULSE_GATE            (ID_OFFSET_GATE + 9,  PULSE,             PulseGateTile::new),
    REPEATER_GATE         (ID_OFFSET_GATE + 10, REPEATER,          RepeaterGateTile::new),
    RANDOMIZER_GATE       (ID_OFFSET_GATE + 11, RANDOMIZER,        RandomizerGateTile::new),
    SR_LATCH_GATE         (ID_OFFSET_GATE + 12, SR_LATCH,          SRLatchGateTile::new),
    TOGGLE_LATCH_GATE     (ID_OFFSET_GATE + 13, TOGGLE_LATCH,      ToggleLatchGateTile::new),
    TRANSPARENT_LATCH_GATE(ID_OFFSET_GATE + 14, TRANSPARENT_LATCH, TransparentLatchGateTile::new),

    RED_ALLOY_WIRE           (ID_OFFSET_WIRE_RED,      RED_ALLOY,           RedAlloyWireTile::new),
    INSULATED_WHITE_WIRE     (ID_OFFSET_WIRE_RED + 1,  INSULATED_WHITE,      () -> new InsulatedWireTile(0)),
    INSULATED_ORANGE_WIRE    (ID_OFFSET_WIRE_RED + 2,  INSULATED_ORANGE,     () -> new InsulatedWireTile(1)),
    INSULATED_MAGENTA_WIRE   (ID_OFFSET_WIRE_RED + 3,  INSULATED_MAGENTA,    () -> new InsulatedWireTile(2)),
    INSULATED_LIGHT_BLUE_WIRE(ID_OFFSET_WIRE_RED + 4,  INSULATED_BLUE,       () -> new InsulatedWireTile(3)),
    INSULATED_YELLOW_WIRE    (ID_OFFSET_WIRE_RED + 5,  INSULATED_YELLOW,     () -> new InsulatedWireTile(4)),
    INSULATED_LIME_WIRE      (ID_OFFSET_WIRE_RED + 6,  INSULATED_LIME,       () -> new InsulatedWireTile(5)),
    INSULATED_PINK_WIRE      (ID_OFFSET_WIRE_RED + 7,  INSULATED_PINK,       () -> new InsulatedWireTile(6)),
    INSULATED_GRAY_WIRE      (ID_OFFSET_WIRE_RED + 8,  INSULATED_GRAY,       () -> new InsulatedWireTile(7)),
    INSULATED_LIGHT_GRAY_WIRE(ID_OFFSET_WIRE_RED + 9,  INSULATED_LIGHT_GRAY, () -> new InsulatedWireTile(8)),
    INSULATED_CYAN_WIRE      (ID_OFFSET_WIRE_RED + 10, INSULATED_CYAN,       () -> new InsulatedWireTile(9)),
    INSULATED_PURPLE_WIRE    (ID_OFFSET_WIRE_RED + 11, INSULATED_PURPLE,     () -> new InsulatedWireTile(10)),
    INSULATED_BLUE_WIRE      (ID_OFFSET_WIRE_RED + 12, INSULATED_BLUE,       () -> new InsulatedWireTile(11)),
    INSULATED_BROWN_WIRE     (ID_OFFSET_WIRE_RED + 13, INSULATED_BROWN,      () -> new InsulatedWireTile(12)),
    INSULATED_GREEN_WIRE     (ID_OFFSET_WIRE_RED + 14, INSULATED_GREEN,      () -> new InsulatedWireTile(13)),
    INSULATED_RED_WIRE       (ID_OFFSET_WIRE_RED + 15, INSULATED_RED,        () -> new InsulatedWireTile(14)),
    INSULATED_BLACK_WIRE     (ID_OFFSET_WIRE_RED + 16, INSULATED_BLACK,      () -> new InsulatedWireTile(15)),

    BUNDLED_NEUTRAL_WIRE   (ID_OFFSET_WIRE_BUNDLED,      BUNDLED_NEUTRAL,    () -> new BundledWireTile(-1)),
    BUNDLED_WHITE_WIRE     (ID_OFFSET_WIRE_BUNDLED + 1,  BUNDLED_WHITE,      () -> new BundledWireTile(0)),
    BUNDLED_ORANGE_WIRE    (ID_OFFSET_WIRE_BUNDLED + 2,  BUNDLED_ORANGE,     () -> new BundledWireTile(1)),
    BUNDLED_MAGENTA_WIRE   (ID_OFFSET_WIRE_BUNDLED + 3,  BUNDLED_MAGENTA,    () -> new BundledWireTile(2)),
    BUNDLED_LIGHT_BLUE_WIRE(ID_OFFSET_WIRE_BUNDLED + 4,  BUNDLED_LIGHT_BLUE, () -> new BundledWireTile(3)),
    BUNDLED_YELLOW_WIRE    (ID_OFFSET_WIRE_BUNDLED + 5,  BUNDLED_YELLOW,     () -> new BundledWireTile(4)),
    BUNDLED_LIME_WIRE      (ID_OFFSET_WIRE_BUNDLED + 6,  BUNDLED_LIME,       () -> new BundledWireTile(5)),
    BUNDLED_PINK_WIRE      (ID_OFFSET_WIRE_BUNDLED + 7,  BUNDLED_PINK,       () -> new BundledWireTile(6)),
    BUNDLED_GRAY_WIRE      (ID_OFFSET_WIRE_BUNDLED + 8,  BUNDLED_GRAY,       () -> new BundledWireTile(7)),
    BUNDLED_LIGHT_GRAY_WIRE(ID_OFFSET_WIRE_BUNDLED + 9,  BUNDLED_LIGHT_GRAY, () -> new BundledWireTile(8)),
    BUNDLED_CYAN_WIRE      (ID_OFFSET_WIRE_BUNDLED + 10, BUNDLED_CYAN,       () -> new BundledWireTile(9)),
    BUNDLED_PURPLE_WIRE    (ID_OFFSET_WIRE_BUNDLED + 11, BUNDLED_PURPLE,     () -> new BundledWireTile(10)),
    BUNDLED_BLUE_WIRE      (ID_OFFSET_WIRE_BUNDLED + 12, BUNDLED_BLUE,       () -> new BundledWireTile(11)),
    BUNDLED_BROWN_WIRE     (ID_OFFSET_WIRE_BUNDLED + 13, BUNDLED_BROWN,      () -> new BundledWireTile(12)),
    BUNDLED_GREEN_WIRE     (ID_OFFSET_WIRE_BUNDLED + 14, BUNDLED_GREEN,      () -> new BundledWireTile(13)),
    BUNDLED_RED_WIRE       (ID_OFFSET_WIRE_BUNDLED + 15, BUNDLED_RED,        () -> new BundledWireTile(14)),
    BUNDLED_BLACK_WIRE     (ID_OFFSET_WIRE_BUNDLED + 16, BUNDLED_BLACK,      () -> new BundledWireTile(15));
    //@formatter:on

    private static final ICTileType[] VALUES_BY_ID;

    static {
        VALUES_BY_ID = new ICTileType[ID_OFFSET_MAX + 1];
        for (ICTileType type : ICTileType.values()) {
            VALUES_BY_ID[type.getID()] = type;
        }
    }

    private final int id;
    private final String unlocalizedName;
    private final Supplier<BaseTile> factory;

    ICTileType(int id, String unlocalizedName, Supplier<BaseTile> factory) {
        this.id = id;
        this.unlocalizedName = unlocalizedName;
        this.factory = factory;
    }

    ICTileType(int id, GateType gateType, Supplier<BaseTile> factory) {
        this(id, gateType.getItem().getDescriptionId(), factory);
    }

    ICTileType(int id, WireType wireType, Supplier<BaseTile> factory) {
        this(id, wireType.getItem().getDescriptionId(), factory);
    }

    public String getUnlocalizedName() {
        return unlocalizedName;
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
