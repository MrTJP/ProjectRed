package mrtjp.projectred.expansion;

import cpw.mods.fml.common.registry.LanguageRegistry;

public class ExpansionLocalizationHandler {
	public static void loadLanguages() {
        for (String localizationFile : ExpansionLocalizations.localeFiles) {
            LanguageRegistry.instance().loadLocalization(localizationFile, ExpansionLocalizationUtility.getLocaleFromFileName(localizationFile), ExpansionLocalizationUtility.isXMLLanguageFile(localizationFile));
        }
    }
}
