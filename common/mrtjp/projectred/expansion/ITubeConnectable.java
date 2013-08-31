package mrtjp.projectred.expansion;

import mrtjp.projectred.transmission.IConnectable;

/**
 * Interface implemented by things to connect to various types of tubes.
 */
public interface ITubeConnectable {
        /**
         * Called to check whether a tube part can connect to this.
         * If a part returns true it is expected to immediately reflect the fact that it is now connected to wire.
         * 
         * @param part The part asking for connection.
         * @param absDir The absolute direction to connect from. 
         * @return True to allow the tube connection.
         */
        public boolean connect(ITubeConnectable tube, int fromAbsDir);
        
        public boolean canConnectTo(ITubeConnectable tube);
}
