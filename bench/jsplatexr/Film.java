
public class Film {
  int _width;
  int _height;
  Pixel[] _pixels;

  Film(int w, int h) {
    _width = w;
    _height = h;
    _pixels = new Pixel[w * h];
  }

  void set_pixel(Pixel p, int x, int y) {
    _pixels[y * _width + x] = p;
  }

  float[] write_floats() {
    float[] out_floats = new float[_width * _height * 4];

    for (int i = 0; i < _width * _height; i++) {
      out_floats[4*i] = _pixels[i]._xyzw[0];
      out_floats[4*i+1] = _pixels[i]._xyzw[1];
      out_floats[4*i+2] = _pixels[i]._xyzw[2];
      out_floats[4*i+3] = _pixels[i]._xyzw[3];
    }
    return out_floats;
  }

};


