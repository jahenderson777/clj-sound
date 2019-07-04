import javax.sound.sampled.SourceDataLine;
import javax.sound.sampled.*;

public class SoundUtil {
    public static float[] sumBuffers(float[][] buffers) {
        int numBufs = buffers.length;
        for (int n = 1; n < numBufs; n++) {
            for (int i = 0; i < buffers[0].length; i++) {
                buffers[0][i] += buffers[n][i];
            }
        }
        return buffers[0];
    }

    public static float[] multiplyBuffers(float[][] buffers) {
        int numBufs = buffers.length;
        for (int n = 1; n < numBufs; n++) {
            for (int i = 0; i < buffers[0].length; i++) {
                buffers[0][i] *= buffers[n][i];
            }
        }
        return buffers[0];
    }

    public static float[] filledBuf(int n, float value) {
        float[] buf = new float[n];
        java.util.Arrays.fill(buf, value);
        return buf;
    }

    public static float[] nullBuf(int n, float value) {
        float[] buf = new float[n];

        return null;
    }

    public static boolean anyNullBufs(float[][] bufs) {
        for (int i = 0; i < bufs.length; i++) {
            if (bufs[i] == null) {
                return true;
            }
        }
        return false;
    }

    public static void writeToLine(SourceDataLine line, byte[] ba, int offset, int count) {
        line.write(ba, offset, count);
    }

    public static float maxFromBuf(float[] buf) {
        float max = 0;
        for (int i = 0; i<buf.length; i++) {
            if (buf[i]>max)
                max = buf[i];
        }
        return max;
    }

    public static void listTargetDataLines() {
        System.out.println("Available Mixers:");
        Mixer.Info[] aInfos = AudioSystem.getMixerInfo();
        for (int i = 0; i < aInfos.length; i++) {
            Mixer mixer = AudioSystem.getMixer(aInfos[i]);
            // mixer.open();
            Line.Info[] lines = mixer.getTargetLineInfo();
            System.out.println(aInfos[i].getName());
            for (int j = 0; j < lines.length; j++) {
                System.out.println("  " + lines[j].toString());
                if (lines[j] instanceof DataLine.Info) {
                    AudioFormat[] formats = ((DataLine.Info) lines[j]).getFormats();
                    for (int k = 0; k < formats.length; k++) {
                        System.out.println("    " + formats[k].toString());
                    }
                }
            }
        }
    }

    public static Mixer getLineByName(String name) {
        Mixer.Info[] aInfos = AudioSystem.getMixerInfo();
        for (int i = 0; i < aInfos.length; i++) {
            Mixer mixer = AudioSystem.getMixer(aInfos[i]);
            if (aInfos[i].getName().equals(name))
                return mixer;
        }
        return null;
    }

}
