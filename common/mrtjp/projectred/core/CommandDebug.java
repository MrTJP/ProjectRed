package mrtjp.projectred.core;

import net.minecraft.command.CommandBase;
import net.minecraft.command.ICommandSender;
import net.minecraft.command.WrongUsageException;
import net.minecraft.server.MinecraftServer;
import net.minecraft.util.ChatMessageComponent;

public class CommandDebug extends CommandBase {
    /**
     * If true:
     * Fire particles indicate block updates.
     * Bonemeal particles indicate packets sent.
     */
    public static boolean WIRE_LAG_PARTICLES;
    
    /**
     * If true, bonemeal particles indicate wire strength updates.
     */
    public static boolean WIRE_DEBUG_PARTICLES;
    
    /**
     * If true, right-click a wire for signal strength.
     * Will not work properly in SMP.
     */
    public static boolean WIRE_READING;

    @Override
    public String getCommandName() {
        return "prdebug";
    }
    
    @Override
    public String getCommandUsage(ICommandSender par1iCommandSender) {
        return "/"+getCommandName()+" {lagpart|debugpart|reading} {on|off}";
    }

    @Override
    public void processCommand(ICommandSender icommandsender, String[] astring) {
        if(astring.length != 2)
            throw new WrongUsageException(getCommandUsage(icommandsender));
        
        String thing = astring[0];
        boolean on;
        
        if(astring[1].equals("on"))
            on = true;
        else if(astring[1].equals("off"))
            on = false;
        else
            throw new WrongUsageException(getCommandUsage(icommandsender));
        
        if(thing.equals("lagpart"))
            WIRE_LAG_PARTICLES = on;
        else if(thing.equals("debugpart"))
            WIRE_DEBUG_PARTICLES = on;
        else if(thing.equals("reading"))
            WIRE_READING = on;
        else
            throw new WrongUsageException(getCommandUsage(icommandsender));
        
        MinecraftServer.getServer().getConfigurationManager().sendChatMsg(ChatMessageComponent.createFromTranslationKey("ProjectRed debug feature '" + thing + "' is now " + astring[1]));
    }
    
    @Override
    public boolean canCommandSenderUseCommand(ICommandSender par1iCommandSender) {
        return super.canCommandSenderUseCommand(par1iCommandSender);
    }
}
