#Include "VoxelGFX.bi"
#Include "fbgfx.bi"
#Include "File.bi"

ScreenRes 800, 600, 32, , FB.GFX_OPENGL
VoxInit GetGLProcAddressCast(@ScreenGLProc), VOXEL_SCREEN64

Dim As Integer X, Y, Z, B, PrvX, PrvY
If FileExists("Sphere.png") Then
    VoxSetSource VoxLoadFile("Sphere.png")
    VoxSetVolume 0
    VoxBlit Vec3I(0,0,0), Vec3I(0,0,0), Vec3I(64,64,64)
   Else
    For X = 0 To 63
        For Y = 0 To 63
            For Z = 0 To 63
                If (X-31)^2+(Y-31)^2+(Z-31)^2 < 32^2 Then
                    VSet Vec3I(X, Y, Z), RGB(127, 127, 127)
                End If
            Next Z
        Next Y
    Next X
End If

Dim As Double PrevT = Timer, dT = 1
Dim As Vec3I V1, V2
VoxScreenTurnRight -1
VoxScreenTurnDown .5
VoxSetColor RGB(255, 0, 0)
Do
    dT = (Timer - PrevT)
    PrevT = Timer
    If MultiKey(FB.SC_RIGHT) Then VoxScreenTurnRight dT
    If MultiKey(FB.SC_LEFT) Then VoxScreenTurnRight -dT
    If MultiKey(FB.SC_DOWN) Then VoxScreenTurnDown dT
    If MultiKey(FB.SC_UP) Then VoxScreenTurnDown -dT
    If MultiKey(FB.SC_1) Then VoxSetColor RGB(255, 0, 0)
    If MultiKey(FB.SC_2) Then VoxSetColor RGB(0, 255, 0)
    If MultiKey(FB.SC_3) Then VoxSetColor RGB(0, 0, 255)
    If MultiKey(FB.SC_4) Then VoxSetColor RGB(0, 255, 255)
    If MultiKey(FB.SC_5) Then VoxSetColor RGB(255, 0, 255)
    If MultiKey(FB.SC_6) Then VoxSetColor RGB(255, 255, 0)
    If MultiKey(FB.SC_7) Then VoxSetColor RGB(255, 255, 255)
    If MultiKey(FB.SC_8) Then VoxSetColor RGB(20, 20, 20) 'Absolute black reflects no light
    If MultiKey(FB.SC_9) Then VoxSetColor RGB(127, 127, 127)
    If B And 1 Or B And 2 Then
        GetMouse X, Y, , B
        If B And 1 AndAlso VoxCursorTest(V1, V2, X, Y) Then
            For Z = 0 To 100
                If VoxCursorTest(V1, V2, (X*Z + PrvX*(100-Z))\100, (Y*Z + PrvY*(100-Z))\100) Then VSet V1
            Next Z
            PrvX = X
            PrvY = Y
        End If
        If B And 2 Then
            VoxScreenTurnRight (X - PrvX) / 200.0
            VoxScreenTurnDown (Y - PrvY) / 200.0
            PrvX = X
            PrvY = Y
        End If
       Else
        GetMouse X, Y, , B
        If B And 1 Then
            PrvX = X
            PrvY = Y
            If VoxCursorTest(V1, V2, X, Y) Then VSet V1
        End If
        If B And 2 Then
            PrvX = X
            PrvY = Y
        End If
    End If
    
    VoxRender 800, 600
    Flip
Loop Until InKey = Chr(255) & "k" Or MultiKey(FB.SC_ESCAPE)

VoxSaveFile "Sphere.png", 0