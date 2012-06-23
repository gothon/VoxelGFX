#Include "VoxelGFX.bi"
#Include "fbgfx.bi"

ScreenRes 800, 600, 32, , FB.GFX_OPENGL
VoxInit GetGLProcAddressCast(@ScreenGLProc), VOXEL_SCREEN128

Dim As Double PrevT = Timer
Do
    VoxVolumeLock
    VoxSetColor RGBA(255*Rnd, 255*Rnd, 255*Rnd, 255)
    For I As Integer = 1 To 5
        VoxLineTo Vec3I(127*Rnd, 127*Rnd, 127*Rnd)
    Next I
    VoxVolumeUnlock
    
    VoxScreenTurnRight (Timer - PrevT)/10
    PrevT = Timer
    
    VoxRender 800, 600
    Flip
Loop Until InKey = Chr(255) & "k" Or MultiKey(FB.SC_ESCAPE)