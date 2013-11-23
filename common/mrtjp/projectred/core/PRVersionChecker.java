package mrtjp.projectred.core;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.URL;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.List;

import net.minecraft.entity.player.EntityPlayer;
import cpw.mods.fml.common.ITickHandler;
import cpw.mods.fml.common.TickType;

public class PRVersionChecker extends Thread implements ITickHandler {

    public PRVersionChecker() {
        this.setName("PR Version Checker");
        this.setDaemon(true);
        this.start();
    }
    
    public boolean isOutdated = false;
    public String newVersion;
    public List<String> changes = new ArrayList<String>();
    
    boolean run = false;
    
    public void run() {
        if (run) return;
        try {
            String current = Configurator.version;
            if (current.contains("@"))
                return;
            
            URL url = new URL("https://raw.github.com/MrTJP/ProjectRed/master/resources/Changelog");
            BufferedReader reader = new BufferedReader(new InputStreamReader(url.openStream()));
            
            String version;
            while((version=reader.readLine()) != null) {
                if (version.startsWith("v")) {
                    newVersion = version.substring(1);
                    break;
                }
            }
            
            String changelog;
            while((changelog=reader.readLine()) != null) {
                if (!changelog.startsWith("-"))
                    break;
                changes.add(changelog);
            }            
            
            isOutdated = current != newVersion;
            
        } catch (Throwable e) {

        }
        run = true;
    }
    
    @Override
    public void tickStart(EnumSet<TickType> type, Object... tickData) {
    }

    boolean displayed = false;
    
    @Override
    public void tickEnd(EnumSet<TickType> type, Object... tickData) {
        
        if (!isOutdated || displayed) return;
        
        EntityPlayer p = (EntityPlayer) tickData[0];
        
        p.addChatMessage("Version " + newVersion + " of ProjectRed available.");
        
        for (String s : changes)
            p.addChatMessage(s);
        
        displayed = true;
    }

    @Override
    public EnumSet<TickType> ticks() {
        return EnumSet.of(TickType.PLAYER);
    }

    @Override
    public String getLabel() {
        return "PR Version Checker";
    }
}
