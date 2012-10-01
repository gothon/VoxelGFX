#ifndef VOXEL_GFX
#define VOXEL_GFX

#define VOXEL_SCREEN32 0x0005
#define VOXEL_SCREEN64 0x0006
#define VOXEL_SCREEN128 0x0007
#define VOXEL_SCREEN256 0x0008
#define VOXEL_SCREEN512 0x0009

#define VOXEL_NOCLEAR 0x0001
#define VOXEL_NOLIGHT 0x0002
#define VOXEL_NOMODELVIEW 0x0004
#define VOXEL_NOGLSTATE 0x0008

#define VOXEL_AXIS_X 0x0001
#define VOXEL_AXIS_Y 0x0002
#define VOXEL_AXIS_Z 0x0003

#define VOXEL_SCREEN (Vox_Volume)(-1)

enum VoxVolumeType {
    Volume_Dynamic = 0,
    Volume_Static = 1,
    Volume_OffScreen = 2,
};

typedef int Vox_Volume;
typedef int Vox_Context;

struct Vec3I {
    union {
        struct {
            int x, y, z;
        };
        int v[3];
    };
    Vec3I();
    Vec3I(const Vec3I& V);
    Vec3I(int X, int Y, int Z);
};

//Set Up
void VoxInit (void* GlExtFetch = NULL, unsigned int Flags = 0);
void VoxScreenRes (Vec3I Size, unsigned int BackColor = 0);
void VoxScreenRes (int SizeX, int SizeY, int SizeZ, unsigned int BackColor = 0);
Vox_Volume VoxNewVolume (VoxVolumeType T = Volume_Dynamic);
Vox_Volume VoxNewVolume (Vec3I Size, VoxVolumeType T = Volume_Dynamic);
Vox_Volume VoxNewVolume (int SizeX, int SizeY, int SizeZ, VoxVolumeType T = Volume_Dynamic);
void VoxSizeVolume (Vec3I Size);
void VoxSizeVolume (int SizeX, int SizeY, int SizeZ);
Vec3I VoxGetVolumeSize (Vox_Volume Vol = -2);
void VoxReloadVolumes();
Vox_Context VoxNewContext(Vox_Volume ScreenVolume = 0);

//File Save/Load
Vox_Volume VoxLoadFile (char* FileName, int Depth = 0, VoxVolumeType T = Volume_OffScreen);
void VoxSaveFile (char* FileName, Vox_Volume Vol);

//Drawing State
void VoxSetContext(Vox_Context C = 0);
void VoxSetVolumeType(VoxVolumeType T);
void VoxSetColor(unsigned int C);
void VoxSetVolume(Vox_Volume Vol = VOXEL_SCREEN);
void VoxSetSource(Vox_Volume Vol);
void VoxSetBlitDefault();
void VoxBlitRightRotate(unsigned int Axis, int Amount = 1);
void VoxBlitReflect(unsigned int Axis);

//Drawing
void VoxVolumeLock();
unsigned int* VoxVolumePtr();
void VoxVolumeUnlock();
void VoxCls();
void VSet(Vec3I Voxel);
void VSet(Vec3I Voxel, unsigned int Col);
void VoxLine(Vec3I A, Vec3I B);
void VoxLineTo(Vec3I B);
void VoxTriangle(Vec3I A, Vec3I B, Vec3I C);
void VoxTriangleTo(Vec3I B, Vec3I C);
void VoxTriangleFanTo(Vec3I C);
void VoxTriangleStripTo(Vec3I C);
void VoxBlit(Vec3I DestV, Vec3I SrcV, Vec3I Size);

//Rendering
void VoxRender(int ScreenW, int ScreenH, unsigned int Flags = 0);
void VoxGlRenderState(int ScreenW = 0, int ScreenH = 0, unsigned int Flags = 0);
void VoxRenderVolume(Vox_Volume Vol);
void VoxRenderSubVolume(Vox_Volume Vol, Vec3I A, Vec3I B);

//Perspective Control
void VoxScreenTurnRight(double Angle);
void VoxScreenTurnDown(double Angle);
void VoxScreenTurnCCW(double Angle);
void VoxScreenMoveRight(double Dist);
void VoxScreenMoveUp(double Dist);
void VoxScreenMoveForward(double Dist);
void VoxScreenCenter(Vec3I V);
void VoxScreenCenter(double X, double Y, double Z);
void VoxScreenDistance(double Dist);

//Reading
int VoxCursorTest(Vec3I& V1, Vec3I& V2, int PixX, int PixY, double& MaxDist); // = -1.0);
int VoxSubCursorTest(Vec3I& V1, Vec3I& V2, Vec3I A, Vec3I B, int PixX, int PixY, double& MaxDist); // = -1.0);
unsigned int VoxPoint(Vec3I Voxel);

#endif //VOXEL_GFX