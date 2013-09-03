package mrtjp.projectred.illumination;

import cpw.mods.fml.common.registry.LanguageRegistry;

public class IlluminationLocalizationHandler {
	public static void loadLanguages() {
        for (String localizationFile : IlluminationLocalizations.localeFiles) {
            LanguageRegistry.instance().loadLocalization(localizationFile, IlluminationLocalizationUtility.getLocaleFromFileName(localizationFile), IlluminationLocalizationUtility.isXMLLanguageFile(localizationFile));
        }
    }
}
