package mrtjp.projectred.core;

import java.util.logging.Level;
import java.util.logging.Logger;

import cpw.mods.fml.common.FMLLog;

public class PRLogger
{
    public static Logger pr_logger;

    static
    {
        pr_logger = Logger.getLogger(Configurator.modName);
        pr_logger.setParent(FMLLog.getLogger());
    }

    private static void log(Level lvl, String msg)
    {
        pr_logger.log(lvl, msg);
    }

    public static void severe(String msg)
    {
        log(Level.SEVERE, msg);
    }

    public static void warn(String msg)
    {
        log(Level.WARNING, msg);
    }

    public static void info(String msg)
    {
        log(Level.INFO, msg);
    }

    public static void config(String msg)
    {
        log(Level.CONFIG, msg);
    }

    public static void fine(String msg)
    {
        log(Level.FINE, msg);
    }

    public static void finer(String msg)
    {
        log(Level.FINER, msg);
    }

    public static void finest(String msg)
    {
        log(Level.FINEST, msg);
    }
}