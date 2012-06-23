#Include "VoxelGFX.bi"
#Include "fbgfx.bi"

'Initialize Graphics display
ScreenRes 800, 600, 32, , FB.GFX_OPENGL
VoxInit GetGLProcAddressCast(@ScreenGLProc), VOXEL_SCREEN32
VoxScreenRes Vec3I(19, 19, 19), RGB(25, 76, 153)

'Draw the Cube
Dim I As Integer
VoxSetColor RGB(255, 255, 255)
For I = 0 To 18 Step 6
    VoxLine Vec3I(I, 1, 1), Vec3I(I,17, 1)
    VoxLine Vec3I(I, 1,17), Vec3I(I,17,17)
    VoxLine Vec3I(I, 1, 1), Vec3I(I, 1,17)
    VoxLine Vec3I(I,17, 1), Vec3I(I,17,17)
    
    VoxLine Vec3I(1, I, 1), Vec3I( 1,I,17)
    VoxLine Vec3I(17,I, 1), Vec3I(17,I,17)
    VoxLine Vec3I(1, I, 1), Vec3I(17,I, 1)
    VoxLine Vec3I(1, I,17), Vec3I(17,I,17)
    
    VoxLine Vec3I(1, 1,I), Vec3I(17, 1,I)
    VoxLine Vec3I(1,17,I), Vec3I(17,17,I)
    VoxLine Vec3I(1, 1,I), Vec3I( 1,17,I)
    VoxLine Vec3I(17,1,I), Vec3I(17,17,I)
Next I

For I = 0 To 18
    VoxSetColor RGB(255, 0, 0)
    VoxLine Vec3I(I,0,0), Vec3I(I,18,0)
    VoxSetColor RGB(0, 255, 255)
    VoxLine Vec3I(I,0,18), Vec3I(I,18,18)
    VoxSetColor RGB(0, 255, 0)
    VoxLine Vec3I(0,I,0), Vec3I(0,I,18)
    VoxSetColor RGB(255, 0, 255)
    VoxLine Vec3I(18,I,0), Vec3I(18,I,18)
    VoxSetColor RGB(0, 0, 255)
    VoxLine Vec3I(0,0,I), Vec3I(18,0,I)
    VoxSetColor RGB(255, 255, 0)
    VoxLine Vec3I(0,18,I), Vec3I(18,18,I)
Next I

VoxSetColor 0
For I = 0 To 18 Step 6
    VoxLine Vec3I(I, 0, 0), Vec3I(I,18, 0)
    VoxLine Vec3I(I, 0,18), Vec3I(I,18,18)
    VoxLine Vec3I(I, 0, 0), Vec3I(I, 0,18)
    VoxLine Vec3I(I,18, 0), Vec3I(I,18,18)
    
    VoxLine Vec3I(0, I, 0), Vec3I( 0,I,18)
    VoxLine Vec3I(18,I, 0), Vec3I(18,I,18)
    VoxLine Vec3I(0, I, 0), Vec3I(18,I, 0)
    VoxLine Vec3I(0, I,18), Vec3I(18,I,18)
    
    VoxLine Vec3I(0, 0,I), Vec3I(18, 0,I)
    VoxLine Vec3I(0,18,I), Vec3I(18,18,I)
    VoxLine Vec3I(0, 0,I), Vec3I( 0,18,I)
    VoxLine Vec3I(18,0,I), Vec3I(18,18,I)
Next I

'Create a Buffer for Blitting
Var Vol = VoxNewVolume(19,19,19,Volume_OffScreen)

'Message Loop
Dim As Integer X, Y, B, PrvX, PrvY
Dim As Double PrevT = Timer, dT = 1
Dim As Vec3I V1, V2
Do
    dT = (Timer - PrevT)
    PrevT = Timer
    'Read the Keyboard
    If MultiKey(FB.SC_RIGHT) Then VoxScreenTurnRight dT
    If MultiKey(FB.SC_LEFT) Then VoxScreenTurnRight -dT
    If MultiKey(FB.SC_DOWN) Then VoxScreenTurnDown dT
    If MultiKey(FB.SC_UP) Then VoxScreenTurnDown -dT
    'Click Test the mouse
    If B = 1 Or B = 2 Then
        GetMouse X, Y, , B
        If B = 2 Then
            VoxScreenTurnRight (X - PrvX) / 200.0
            VoxScreenTurnDown (Y - PrvY) / 200.0
            PrvX = X
            PrvY = Y
        End If
       Else
        GetMouse X, Y, , B
        If B = 2 Then
            PrvX = X
            PrvY = Y
        End If
        If B = 1 Then
            VoxSetVolume 0 'Hit Test the Mouse
            If VoxCursorTest(V1, V2, X, Y) Then
                VoxSetVolume Vol
                VoxSetSource 0
                VoxSetBlitDefault
                VoxBlit Vec3I(0,0,0), Vec3I(0,0,0), Vec3I(19, 19, 19)
                VoxSetVolume 0
                VoxSetSource Vol
                If V1.X = 0 Then 'Rotate the clicked face with Blit
                    VoxBlitRightRotate VOXEL_AXIS_X, -1
                    VoxBlit Vec3I(0,0,0), Vec3I(0,0,0), Vec3I(6, 19, 19)
                ElseIf V1.X = 18 Then
                    VoxBlitRightRotate VOXEL_AXIS_X, 1
                    VoxBlit Vec3I(13,0,0), Vec3I(13,0,0), Vec3I(6, 19, 19)
                ElseIf V1.Y = 0 Then
                    VoxBlitRightRotate VOXEL_AXIS_Y, -1
                    VoxBlit Vec3I(0,0,0), Vec3I(0,0,0), Vec3I(19, 6, 19)
                ElseIf V1.Y = 18 Then
                    VoxBlitRightRotate VOXEL_AXIS_Y, 1
                    VoxBlit Vec3I(0,13,0), Vec3I(0,13,0), Vec3I(19, 6, 19)
                ElseIf V1.Z = 0 Then
                    VoxBlitRightRotate VOXEL_AXIS_Z, -1
                    VoxBlit Vec3I(0,0,0), Vec3I(0,0,0), Vec3I(19, 19, 6)
                ElseIf V1.Z = 18 Then
                    VoxBlitRightRotate VOXEL_AXIS_Z, 1
                    VoxBlit Vec3I(0,0,13), Vec3I(0,0,13), Vec3I(19, 19, 6)
                End If
            End If
        End If
    End If
    'Render to the Display
    VoxRender 800, 600
    Flip
Loop Until InKey = Chr(255) & "k" Or MultiKey(FB.SC_ESCAPE)