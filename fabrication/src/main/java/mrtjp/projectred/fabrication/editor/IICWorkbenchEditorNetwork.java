package mrtjp.projectred.fabrication.editor;

import codechicken.lib.data.MCDataOutput;

/**
 * Interface implemented on an IC Workbench tile containing network-related methods that the editor uses
 * to send data to and from clients that have the editor open.
 */
public interface IICWorkbenchEditorNetwork {

    /**
     * Returns a buffered output stream that can be freely written. It is automatically
     * sent at regular intervals. If client, packet is sent to server. If server, packets are
     * sent to all clients watching the editor UI.
     *
     * The stream is always terminated with a frame of value 255 before being sent. The other side
     * must read through the stream until a terminator frame byte of 255 is read.
     *
     * @param streamKey Identifier for the buffered stream (between 0 and 255)
     * @param frameKey Frame byte for each group of data. Cannot be 255, as that is used for end-of-packet terminator.
     * @return The buffered stream
     */
    MCDataOutput getBufferedStream(int streamKey, int frameKey);

    /**
     * @return True if this is the client
     */
    boolean isClientSide();

    /**
     * Called by the editor when something inside this editor has changed that would cause the tile's
     * NBT data to change
     */
    void markSave();

    /**
     * @return The world game time
     */
    long getGameTime();
}
