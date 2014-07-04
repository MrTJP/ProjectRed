package mrtjp.projectred.core;


import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class PRLogger
{
    public static Logger pr_logger;

    static
    {
        pr_logger = LogManager.getFormatterLogger(Configurator.modName);
    }

    public static void fatal(String msg)
    {
        pr_logger.fatal(msg);
    }

    public static void error(String msg)
    {
        pr_logger.error(msg);
    }

    public static void warn(String msg)
    {
        pr_logger.warn(msg);
    }

    public static void info(String msg)
    {
        pr_logger.info(msg);
    }

    public static void debug(String msg)
    {
        pr_logger.debug(msg);
    }

    public static void trace(String msg)
    {
        pr_logger.trace(msg);
    }
}