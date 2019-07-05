import java.util.Arrays;

//Moog 24 dB/oct resonant lowpass VCF
//References: CSound source code, Stilson/Smith CCRMA paper.
//Modified by paul.kellett@maxim.abel.co.uk July 2000, jahenderson777@gmail.com 2019
//Based on Java implementation by Damien Di Fede September 2010

public class MoogFilter implements UGen {
    public enum Type {
        HP,
        LP,
        BP
    }

    // public float	audio;
    //public float	frequency; //hz
    // public float	resonance; // 0..1
    public Type type; //low pass, high pass, or band pass.
    private float[]	b;	// filter buffers (beware denormals!)
    public int sampleRate;
    //private float y;
    private int initialX = 0;


    public MoogFilter(int initialX) {
        type = Type.LP;
        sampleRate = 48000;
        b = new float[5];
    }

    public float[] process(int count, float[][] inputs) {
        float[] out = new float[count];
        float[] input = inputs[0];
        float[] frequency = inputs[1];
        float[] resonance = inputs[2];
        // Set coefficients given frequency & resonance [0.0...1.0]
        float t1, t2, normFreq, rez, q, p, f, in; // temporary buffers
        //y1 = y;

        for (int i = - this.initialX; i<count; i++) {
            normFreq = frequency[i] / (sampleRate * 0.5f);
            rez = constrain(resonance[i], 0.f, 1.f);

            q = 1.0f - normFreq;
            p = normFreq + 0.8f * normFreq * q;
            f = p + p - 1.0f;
            q = rez * ( 1.0f + 0.5f * q * ( 1.0f - q + 5.6f * q * q ) );

            // Filter (in [-1.0...+1.0])
            in = constrain(input[i], -1, 1); // hard clip

            in -= q * b[4]; // feedback

            t1 = b[1];
            b[1] = ( in + b[0] ) * p - b[1] * f;

            t2 = b[2];
            b[2] = ( b[1] + t1 ) * p - b[2] * f;

            t1 = b[3];
            b[3] = ( b[2] + t2 ) * p - b[3] * f;

            b[4] = ( b[3] + t1 ) * p - b[4] * f;
            b[4] = b[4] - b[4] * b[4] * b[4] * 0.166667f; // clipping

            // inelegantly squash denormals
            if (Float.isNaN(b[4])) {
                Arrays.fill(b, 0);
            }

            b[0] = in;

            switch(type) {
            case HP:
                out[i] = in - b[4];
                break;

            case LP:
                out[i] = b[4];
                break;

            case BP:
                out[i] = 3.0f * (b[3] - b[4]);
            }
        }
        initialX = 0;
        return out;
    }

    private static float constrain(float value, float min, float max) {
        if (value < min) return min;
        if (value > max) return max;
        return value;
    }
}

