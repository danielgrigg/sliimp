#include <vector>
#include <OpenEXR/ImfRgbaFile.h>
#include <functional>

using namespace std;

int write_rgba(int width, 
    int height, 
    const char* filename, 
    const float* raw_rgbas) {

  std::vector<Imf::Rgba> pixels(width * height);
  for (int p = 0; p < width * height; ++p) {
    pixels[p] = Imf::Rgba(raw_rgbas[4*p+0], 
                          raw_rgbas[4*p+1], 
                          raw_rgbas[4*p+2],
                          raw_rgbas[4*p+3]);
  }
  Imf::RgbaOutputFile file (filename, width, height, Imf::WRITE_RGBA);
  file.setFrameBuffer(&pixels[0], 1, width);
  file.writePixels (height); 
  return 1;
}

struct Pixel {
  float _xyz[3];
  float _weight;
};

struct Film {
  std::vector<Pixel> _pixels;

  Film(int w, int h):
    _w(w),
    _h(h),
    _pixels(w * h)
  {}

  void set_pixel(Pixel p, int x, int y){
    _pixels[y * _w + x] = p;
  }

  int _w;
  int _h;
};

void image_process(Film& film, std::function<Pixel(int,int)> f) {

  for (int y = 0; y < film._h; ++y){
    for (int x = 0; x < film._w; ++x) {
      film.set_pixel(f(x,y), x, y);
    }
  }

}

Pixel boxes_fn(int x, int y) {
  return {{(float)sin(0.01 * 0.01 * x * y), 0, 0}, 1};
}

Pixel bench_fn(int x, int y) {
  return {{(float)sin(0.03 * 0.03 * x * y), 0, 0}, 1};
}
int main(int argc, char** argv) {

  int w = 512;
  int h = 512;
  int iterations = 10;
  if (argc > 1) {
    iterations = atoi(argv[1]);
  }

  printf("%d iterations\n", iterations);
  Film film(w, h);
  for (int i = 0; i < iterations; ++i) {
    image_process(film, bench_fn);
  }
  write_rgba(w, h, "/tmp/cpp.exr", (const float*)&film._pixels[0]);
  return 0;
}

