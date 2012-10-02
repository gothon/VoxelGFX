#include "VoxelGFX.h"

void VoxInitGl(unsigned int Flags) {
#ifdef __WIN32
    VoxInit((void*)&wglGetProcAddress, Flags);
#else
    VoxInit((void*)&glXGetProcAddress, Flags);
#endif
}

void VoxGetVolumeSize(void* Size, Vox_Volume Vol) {
    *(Vec3I*)Size = VoxGetVolumeSize(Vol);
}

static Vec3I VecStack[3]; static int I = 0;

void V3I(int X, int Y, int Z) {
    VecStack[I] = Vec3I(X, Y, Z);
    I = (I + 1) % 3;
}

void VSet(unsigned int Col) { VSet(VecStack[0], Col); I = 0; }
void VoxLine() { VoxLine(VecStack[0], VecStack[1]); I = 0; }
void VoxLineTo() { VoxLineTo(VecStack[0]); I = 0; }
void VoxTriangle() { VoxTriangle(VecStack[0], VecStack[1], VecStack[2]); I = 0; }
void VoxTriangleTo() { VoxTriangleTo(VecStack[0], VecStack[1]); I = 0; }
void VoxTriangleFanTo() { VoxTriangleFanTo(VecStack[0]); I = 0; }
void VoxTriangleStripTo() { VoxTriangleStripTo(VecStack[0]); I = 0; }
void VoxBlit() { VoxBlit(VecStack[0], VecStack[1], VecStack[2]); I = 0; }

void VoxRenderSubVolume(Vox_Volume Vol) { VoxRenderSubVolume(Vol, VecStack[0], VecStack[1]); I = 0; }

int VoxCursorTest(void* V1, void* V2, int PixX, int PixY, double& MaxDist) {
    VoxCursorTest((Vec3I&)V1, (Vec3I&)V2, PixX, PixY, MaxDist);
}
int VoxSubCursorTest(void* V1, void* V2, int PixX, int PixY, double& MaxDist) {
    VoxSubCursorTest((Vec3I&)V1, (Vec3I&)V2, VecStack[0], VecStack[1], PixX, PixY, MaxDist);
    I = 0;
}
unsigned int VoxPoint(int X, int Y, int Z) { return VoxPoint(Vec3I(X, Y, Z)); }
