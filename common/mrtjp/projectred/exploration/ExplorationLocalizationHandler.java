package mrtjp.projectred.exploration;

import cpw.mods.fml.common.registry.LanguageRegistry;

public class ExplorationLocalizationHandler {
	public static void loadLanguages() {
        for (String localizationFile : ExplorationLocalizations.localeFiles) {
            LanguageRegistry.instance().loadLocalization(localizationFile, ExplorationLocalizationUtility.getLocaleFromFileName(localizationFile), ExplorationLocalizationUtility.isXMLLanguageFile(localizationFile));
        }
    }
}
