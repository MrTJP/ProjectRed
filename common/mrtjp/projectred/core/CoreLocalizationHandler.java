package mrtjp.projectred.core;

import cpw.mods.fml.common.registry.LanguageRegistry;

public class CoreLocalizationHandler {
	public static void loadLanguages() {
        for (String localizationFile : CoreLocalizations.localeFiles) {
            LanguageRegistry.instance().loadLocalization(localizationFile, CoreLocalizationUtility.getLocaleFromFileName(localizationFile), CoreLocalizationUtility.isXMLLanguageFile(localizationFile));
        }
    }
}
