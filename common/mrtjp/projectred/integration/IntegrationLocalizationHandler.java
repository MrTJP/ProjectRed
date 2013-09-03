package mrtjp.projectred.integration;

import cpw.mods.fml.common.registry.LanguageRegistry;

public class IntegrationLocalizationHandler {
	public static void loadLanguages() {
        for (String localizationFile : IntegrationLocalizations.localeFiles) {
            LanguageRegistry.instance().loadLocalization(localizationFile, IntegrationLocalizationUtility.getLocaleFromFileName(localizationFile), IntegrationLocalizationUtility.isXMLLanguageFile(localizationFile));
        }
    }
}
