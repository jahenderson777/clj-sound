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

    public static boolean anyNullBufs(float[][] bufs) {
        for (int i = 0; i < bufs.length; i++) {
            if (bufs[i] == null) {
                return true;
            }
        }
        return false;
    }
}
