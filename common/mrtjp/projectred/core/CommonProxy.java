package mrtjp.projectred.core;

import static mrtjp.projectred.ProjectRed.initializedModules;
import static mrtjp.projectred.ProjectRed.registeredModules;

public class CommonProxy implements IProxy {
	
	
	@Override
	public void preinit() {
		for (IProjectRedModule m : registeredModules) {
			if (areDependenciesLoaded(m.getModuleDependencies())) {
				initializedModules.add(m);
			} else {
				throw new RuntimeException("Module " + m.getModuleID() + " was missing required dependency");
			}
		}
		for (IProjectRedModule m : initializedModules) {
			m.getCommonProxy().preinit();
		}
	}
	
	@Override
	public void init() {
		for (IProjectRedModule m : initializedModules) {
			m.getCommonProxy().init();
		}
	}
	
	@Override
	public void postinit() {
		for (IProjectRedModule m : initializedModules) {
			m.getCommonProxy().postinit();
		}
	}
	
	private boolean areDependenciesLoaded(String[] moduleID) {
		if (moduleID == null || moduleID.length == 0) {
			return true;
		}
		for (String id : moduleID) {
			boolean isLoaded = false;
			for (IProjectRedModule m : initializedModules) {
				if (id == m.getModuleID()) {
					isLoaded = true;
					break;
				}
			}
			if (!isLoaded) {
				return false;
			}
 		}
		return true;
	}		

	
	public void initOreDictionaryDefinitions() {
	}
}