package mrtjp.projectred.core;

import java.io.File;

import net.minecraftforge.common.Configuration;
import net.minecraftforge.common.Property;
import cpw.mods.fml.common.event.FMLPreInitializationEvent;

public class Configurator {
    protected static File _configFolder;
    protected static File _configFile;

    /** Constants **/
    public static final String modId = "ProjectRed";
    public static final String modNetworkChannel = "ProjRed";
    public static final String version = "@VERSION@";
    public static final String buildnumber = "@BUILD_NUMBER@";
    public static final String modName = "Project: Red";

    public static final String corePacketChannel = "PR:Core";
    public static final String integrationPacketChannel = "PR:Int";
    public static final String transmissionPacketChannel = "PR:Trans";
    public static final String expansionPacketChannel = "PR:Expan";

    /** Modules **/
    public static Property module_Core;
    public static Property module_Integration;
    public static Property module_Transmission;
    public static Property module_Illumination;
    public static Property module_Expansion;
    public static Property module_Exploration;

    /** Multipart IDs **/
    public static Property part_gate;
    public static Property part_wire;
    public static Property part_jwire;
    public static Property part_lantern;
    public static Property part_invlantern;
    public static Property part_lamp;
    public static Property part_invlamp;
    public static Property part_tube;

    /** Block IDs **/
    public static Property block_machinesID;
    public static Property block_oresID;
    public static Property block_stonesID;

    /** Item IDs **/
    public static Property item_screwdriverID;
    public static Property item_componentsID;
    public static Property item_drawplateID;
    public static Property item_woolginID;
    public static Property item_backpackID;
    public static Property item_vawtID;
    public static Property item_wireDebuggerID;
    

    public static Property item_rubyAxe;
    public static Property item_sapphireAxe;
    public static Property item_peridotAxe;
    public static Property item_rubyHoe;
    public static Property item_sapphireHoe;
    public static Property item_peridotHoe;
    public static Property item_rubyPickaxe;
    public static Property item_sapphirePickaxe;
    public static Property item_peridotPickaxe;
    public static Property item_rubyShovel;
    public static Property item_sapphireShovel;
    public static Property item_peridotShovel;
    public static Property item_rubySword;
    public static Property item_sapphireSword;
    public static Property item_peridotSword;
    public static Property item_woodSaw;
    public static Property item_stoneSaw;
    public static Property item_ironSaw;
    public static Property item_goldSaw;
    public static Property item_rubySaw;
    public static Property item_sapphireSaw;
    public static Property item_peridotSaw;
    public static Property item_diamondSaw;
    public static Property item_woodSickle;
    public static Property item_stoneSickle;
    public static Property item_ironSickle;
    public static Property item_goldSickle;
    public static Property item_rubySickle;
    public static Property item_sapphireSickle;
    public static Property item_peridotSickle;
    public static Property item_diamondSickle;

    /** Generation **/
    public static Property gen_MarbleCave;
    public static Property gen_Volcano;
    public static Property gen_Ruby;
    public static Property gen_Sapphire;
    public static Property gen_Peridot;
    public static Property gen_SpreadingMoss;

    /** Settings **/
    public static Property debugMode;
    public static Property logicwires3D;
    public static Property logicGateSounds;

    public static void initConfig(FMLPreInitializationEvent event) {
        _configFolder = event.getModConfigurationDirectory();
        _configFile = new File(_configFolder.getAbsolutePath() + "/ProjectRed.cfg");
        loadPropertiesFromFile(_configFile);
    }

    public static void loadPropertiesFromFile(File file) {
        Configuration localConfig = new Configuration(file);
        localConfig.load();

        module_Core = localConfig.get("Modules", "Core", true);
        module_Integration = localConfig.get("Modules", "Integration", true);
        module_Transmission = localConfig.get("Modules", "Transmission", true);
        module_Illumination = localConfig.get("Modules", "Illumination", true);
        module_Expansion = localConfig.get("Modules", "Expansion", true);
        module_Exploration = localConfig.get("Modules", "Exploration", true);

        part_gate = localConfig.get("MultiPart Item IDs", "Gate Part ID", 9030);
        part_wire = localConfig.get("MultiPart Item IDs", "Wire Part ID", 9031);
        part_jwire = localConfig.get("MultiPart Item IDs", "Jacketed Wire Part ID", 9032);
        part_lantern = localConfig.get("MultiPart Item IDs", "Lantern Part ID", 9033);
        part_invlantern = localConfig.get("MultiPart Item IDs", "Inverted Lantern Part ID", 9034);
        part_lamp = localConfig.get("MultiPart Item IDs", "Lamp Part ID", 9035);
        part_invlamp = localConfig.get("MultiPart Item IDs", "Inverted Lamp Part ID", 9036);
        part_tube = localConfig.get("MultiPart Item IDs", "Tube Part ID", 9037);

        block_machinesID = localConfig.getBlock("block_machinesID", 2129);
        block_oresID = localConfig.getBlock("block_oresID", 2130);
        block_stonesID = localConfig.getBlock("block_stonesID", 2131);

        item_screwdriverID = localConfig.getItem("item_screwdriverID", 9024);
        item_componentsID = localConfig.getItem("item_componentsID", 9025);
        item_drawplateID = localConfig.getItem("item_drawplateID", 9026);
        item_woolginID = localConfig.getItem("item_woolginID", 9027);
        item_backpackID = localConfig.getItem("item_backpackID", 9028);
        item_vawtID = localConfig.getItem("item_turbineID", 9029);

        item_rubyAxe = localConfig.getItem("rubyaxe", 9097);
        item_sapphireAxe = localConfig.getItem("sapphireaxe", 9098);
        item_peridotAxe = localConfig.getItem("peridotaxe", 9099);
        item_rubyHoe = localConfig.getItem("rubyhoe", 9100);
        item_sapphireHoe = localConfig.getItem("sapphirehoe", 9101);
        item_peridotHoe = localConfig.getItem("peridothoe", 9102);
        item_rubyPickaxe = localConfig.getItem("rubypickaxe", 9103);
        item_sapphirePickaxe = localConfig.getItem("sapphirepickaxe", 9104);
        item_peridotPickaxe = localConfig.getItem("peridotpickaxe", 9105);
        item_rubyShovel = localConfig.getItem("rubyshovel", 9106);
        item_sapphireShovel = localConfig.getItem("sapphireshovel", 9107);
        item_peridotShovel = localConfig.getItem("peridotshovel", 9108);
        item_rubySword = localConfig.getItem("rubysword", 9109);
        item_sapphireSword = localConfig.getItem("sapphiresword", 9110);
        item_peridotSword = localConfig.getItem("peridotsword", 9112);
        item_woodSaw = localConfig.getItem("woodsaw", 9113);
        item_stoneSaw = localConfig.getItem("stonesaw", 9114);
        item_ironSaw = localConfig.getItem("ironsaw", 9115);
        item_goldSaw = localConfig.getItem("goldsaw", 9116);
        item_rubySaw = localConfig.getItem("rubysaw", 9117);
        item_sapphireSaw = localConfig.getItem("sapphiresaw", 9118);
        item_peridotSaw = localConfig.getItem("peridotsaw", 9119);
        item_diamondSaw = localConfig.getItem("diamondsaw", 9120);
        item_woodSickle = localConfig.getItem("woodsickle", 9121);
        item_stoneSickle = localConfig.getItem("stonesickle", 9122);
        item_ironSickle = localConfig.getItem("ironsickle", 9123);
        item_goldSickle = localConfig.getItem("goldsickle", 9124);
        item_rubySickle = localConfig.getItem("rubysickle", 9125);
        item_sapphireSickle = localConfig.getItem("sapphiresickle", 9126);
        item_peridotSickle = localConfig.getItem("peridotsickle", 9127);
        item_diamondSickle = localConfig.getItem("diamondsickle", 9128);
        item_wireDebuggerID = localConfig.getItem("item_wireDebuggerID", 9129);

        gen_Ruby = localConfig.get("World Generation", "Ruby Ore", true);
        gen_Sapphire = localConfig.get("World Generation", "Sapphire Ore", true);
        gen_Peridot = localConfig.get("World Generation", "Peridot Ore", true);
        gen_MarbleCave = localConfig.get("World Generation", "Marble Caves", true);
        gen_Volcano = localConfig.get("World Generation", "Volcanos", true);
        gen_SpreadingMoss = localConfig.get("World Generation", "Spreading Moss", true);

        debugMode = localConfig.get("general", "Enable Debugging", false);
        debugMode.comment = "Enable advanced debugging, should ALWAYS be false.";
        
        logicwires3D = localConfig.get("general", "3Dlogicwires", true);
        logicwires3D.comment = "If set to false, flat wire textures will be used for logic gates. Significant performance improvement";

        logicGateSounds = localConfig.get("general", "Logic Sounds", true);
        logicGateSounds.comment = "If set to false, logic gates will not make sounds.";
        localConfig.save();
    }
}