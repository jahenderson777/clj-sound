import javax.sound.midi.*;
import java.util.ArrayList;
import java.util.List;
import java.io.*;
import java.util.function.*;

public class MidiHandler {
    public static void listDevices() {
        MidiDevice.Info[] infos = MidiSystem.getMidiDeviceInfo();
        for (int i = 0; i < infos.length; i++) {
                System.out.println(infos[i]);
        }
    }

    public MidiHandler(String name, Function<byte[], Void> onMessage) {
        MidiDevice device;
        MidiDevice.Info[] infos = MidiSystem.getMidiDeviceInfo();
        for (int i = 0; i < infos.length; i++) {
            try {
                device = MidiSystem.getMidiDevice(infos[i]);
                if (device.getDeviceInfo().toString().contains(name)) {
                    Transmitter trans = device.getTransmitter();
                    trans.setReceiver(new MidiInputReceiver(device.getDeviceInfo().toString(), onMessage));
                    device.open();
                    System.out.println(device.getDeviceInfo()+" Was Opened");
                    break;
                }
            } catch (MidiUnavailableException e) {}
        }
    }

    public class MidiInputReceiver implements Receiver {
        public String name;
        private Function<byte[], Void> onMessage;
        public MidiInputReceiver(String name, Function<byte[], Void> fn) {
            onMessage = fn;
            this.name = name;
        }
        public void send(MidiMessage msg, long timeStamp) {
            onMessage.apply(msg.getMessage());
        }
        public void close() {}
    }
}
