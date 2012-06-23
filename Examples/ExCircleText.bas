#Include "VoxelGFX.bi"
#Include "fbgfx.bi"

#Include Once "VoxelFBGFXHelper.bas"

ScreenRes 800, 600, 32, , FB.GFX_OPENGL
VoxInit GetGLProcAddressCast(@ScreenGLProc), VOXEL_SCREEN64

Dim Im As Any Ptr = ImageCreate(63, 63, 0, 32)
If Im = 0 Then
    ConsolePrint "Failed to create image."
    Sleep
    End -1
End If

Circle Im, (31, 31), 31, RGBA(255, 0, 0, 255),,, 1.0

Dim As Double PrevT = Timer-1, dT = 1
Do
    dT = (Timer - PrevT)
    PrevT = Timer
    If MultiKey(FB.SC_RIGHT) Then VoxScreenTurnRight dT
    If MultiKey(FB.SC_LEFT) Then VoxScreenTurnRight -dT
    If MultiKey(FB.SC_DOWN) Then VoxScreenTurnDown dT
    If MultiKey(FB.SC_UP) Then VoxScreenTurnDown -dT
    
    VoxVolumeLock
    VoxCls
    DrawFBGfxImage Im, Vec3I(0,0,31), 0
    DrawFBGfxImage Im, Vec3I(0,31,0), 1
    DrawFBGfxImage Im, Vec3I(31,0,0), 2
    VoxelPrint "Hello!", Vec3I(0,56,CInt(32+30*Sin(Timer*3)))
    VoxVolumeUnLock
    
    VoxRender 800, 600
    Flip
Loop Until InKey = Chr(255) & "k" Or MultiKey(FB.SC_ESCAPE)

ImageDestroy Im