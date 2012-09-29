#include "SDL.h"
#include "VoxelGFX.h"

#define RGBA(r,g,b,a) (((unsigned int)(r) << 16) | ((unsigned int)(g) << 8) | (unsigned int)(b) | ((unsigned int)(a) << 24))

int SDL_main(int argc, char *argv[]) {
    SDL_Event Event;
    int ScreenW = 800, ScreenH = 600;
    
    if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_TIMER) != 0) exit(1);
    
    SDL_SetVideoMode(ScreenW, ScreenH, 0, SDL_RESIZABLE | SDL_OPENGL);
    SDL_WM_SetCaption("Voxel Lines SDL C++", "");
    
    VoxInit((void*)&SDL_GL_GetProcAddress, VOXEL_SCREEN128);
    
    unsigned int PrevT = SDL_GetTicks();
    for(;;) {
        VoxVolumeLock();
        VoxSetColor RGBA(255*rand()/RAND_MAX, 255*rand()/RAND_MAX, 255*rand()/RAND_MAX, 255);
        for (int I = 1; I <= 5; I++) {
            VoxLineTo(Vec3I(127*rand()/RAND_MAX, 127*rand()/RAND_MAX, 127*rand()/RAND_MAX));
        }
        VoxVolumeUnlock();
        
        VoxRender(ScreenW, ScreenH);
        SDL_GL_SwapBuffers();
        
        VoxScreenTurnRight((SDL_GetTicks() - PrevT)/10000.0);
        PrevT = SDL_GetTicks();
        
        while (SDL_PollEvent(&Event) != 0) {
            switch (Event.type) {
            case(SDL_QUIT):
                SDL_Quit();
                exit(1);
                break;
            case(SDL_VIDEORESIZE):
                ScreenW = Event.resize.w;
                ScreenH = Event.resize.h;
                break;
            }
        }
    }
}