package mrtjp.projectred.transmission;

import cpw.mods.fml.common.registry.LanguageRegistry;

public class TransmissionLocalizationHandler {
	public static void loadLanguages() {
        for (String localizationFile : TransmissionLocalizations.localeFiles) {
            LanguageRegistry.instance().loadLocalization(localizationFile, TransmissionLocalizationUtility.getLocaleFromFileName(localizationFile), TransmissionLocalizationUtility.isXMLLanguageFile(localizationFile));
        }
    }
}
