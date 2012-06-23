#Include "VoxelGFX.bi"
#Include "SDL/SDL.bi"

Dim Event As SDL_Event
Dim As Integer ScreenW = 800, ScreenH = 600

If SDL_Init(SDL_INIT_VIDEO Or SDL_INIT_AUDIO Or SDL_INIT_TIMER) <> 0 Then End 1

SDL_SetVideoMode(ScreenW, ScreenH, 0, SDL_RESIZABLE Or SDL_OPENGL)
SDL_WM_SetCaption "Voxel Lines SDL", ""

VoxInit GetGLProcAddressCast(@SDL_GL_GetProcAddress), VOXEL_SCREEN128

Dim PrevT As Double = Timer
Do
    VoxVolumeLock
    VoxSetColor RGBA(255*Rnd, 255*Rnd, 255*Rnd, 255)
    For I As Integer = 1 To 5
        VoxLineTo Vec3I(127*Rnd, 127*Rnd, 127*Rnd)
    Next I
    VoxVolumeUnlock
    
    VoxRender ScreenW, ScreenH
    SDL_GL_SwapBuffers
    
    VoxScreenTurnRight (Timer - PrevT)/10
    PrevT = Timer
    
    Do While SDL_PollEvent(@Event) <> 0
        Select Case Event.Type
        Case SDL_QUIT_
            Exit Do, Do
        Case SDL_VIDEORESIZE
            ScreenW = Event.resize.w
            ScreenH = Event.resize.h
        End Select
    Loop
Loop

SDL_Quit