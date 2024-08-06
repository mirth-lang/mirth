#include<stdint.h>
#include<SDL_video.h>
#include<SDL_pixels.h>
#include<stdio.h>
#include<math.h>

void set_pixel(SDL_Surface* surface, int x, int y, int iterations) {
    uint8_t adjusted = (uint8_t)(((double)iterations / 250.0) * 254.0);

    ((uint32_t*)(surface->pixels))[(y*surface->w) + x] = SDL_MapRGB(
      surface->format, adjusted, adjusted, adjusted);
}
