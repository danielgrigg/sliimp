import com.sun.jna.Library;
import com.sun.jna.Native;
import com.sun.jna.Platform;

/** Simple example of JNA interface mapping and usage. */
public class SplatExr {

  // This is the standard, stable way of mapping, which supports extensive
  // customization and mapping of Java to native types.
  public interface CLibrary extends Library {
    CLibrary INSTANCE = (CLibrary)
      Native.loadLibrary("exru", CLibrary.class);

    void write_rgba(int width, int height, String filename, float[] raw_rgbas);
  }

  public static void image_process(Film film) {

  
    for (int y = 0; y < film._height; ++y){
      for (int x = 0; x < film._width; ++x) {

        Pixel p = new Pixel((float)(Math.sin(0.1 * x) * Math.sin(0.1 * y)),
                            0.0f, 
                            0.0f, 
                            1.0f);
        film.set_pixel(p, x, y);
      }
    }
  }

  public static void main(String[] args) {
    // System.setProperty("jna.Library.path", "/some/absolute/path");

    int w = 512;
    int h = 512;

    int iterations = args.length < 1 ? 10 : Integer.parseInt(args[0]);
    System.out.println("Iterations " + iterations);
    for (int n = 0; n < iterations; ++n) {
      Film f = new Film(w, h);
      image_process(f);
    }

      Film f = new Film(w, h);
      image_process(f);
    System.out.println("Writing to /tmp/jsplat.exr...");
    CLibrary.INSTANCE.write_rgba(w,h, "/tmp/jsplat.exr", f.write_floats());
  }
}

