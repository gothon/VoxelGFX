'/////////////////////////////////////
'|| VoxelGFX - Voxel Graphics Library
'||   Copyright (C) 2012 Alex Thomson
'||
'|| This file is part of VoxelGFX.
'||
'|| VoxelGFX is free software: you can redistribute it and/or modify
'|| it under the terms of the GNU Lesser General Public License as published by
'|| the Free Software Foundation, either version 3 of the License, or
'|| (at your option) any later version.
'||
'|| VoxelGFX is distributed in the hope that it will be useful,
'|| but WITHOUT ANY WARRANTY; without even the implied warranty of
'|| MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'|| GNU Lesser General Public License for more details.
'||
'|| You should have received a copy of the GNU Lesser General Public License
'|| along with VoxelGFX.  If not, see <http://www.gnu.org/licenses/>.
'\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

#Include "modGeometry.bi"
#Include "modVolume.bi"

Namespace InternalVoxelGFX

VA_WRAPPER(Byte)
VA_WRAPPER(UInteger)

Sub InternalVoxelVolume.RenderAll()
    Dim I As Integer, V As Single
    glBegin GL_QUADS
    For I = 0 To D-1
        V = (I + 0.5) / D
        glNormal3f 0, 0, -1
        glTexCoord3f 0, 0, V: glVertex3f 0, 0, I
        glTexCoord3f 0, 1, V: glVertex3f 0, H, I
        glTexCoord3f 1, 1, V: glVertex3f W, H, I
        glTexCoord3f 1, 0, V: glVertex3f W, 0, I
        glNormal3f 0, 0, 1
        glTexCoord3f 0, 1, V: glVertex3f 0, H, I + 1
        glTexCoord3f 0, 0, V: glVertex3f 0, 0, I + 1
        glTexCoord3f 1, 0, V: glVertex3f W, 0, I + 1
        glTexCoord3f 1, 1, V: glVertex3f W, H, I + 1
    Next I
    For I = 0 To H-1
        V = (I + 0.5) / H
        glNormal3f 0, -1, 0
        glTexCoord3f 0, V, 0: glVertex3f 0, I, 0
        glTexCoord3f 1, V, 0: glVertex3f W, I, 0
        glTexCoord3f 1, V, 1: glVertex3f W, I, D
        glTexCoord3f 0, V, 1: glVertex3f 0, I, D
        glNormal3f 0, 1, 0
        glTexCoord3f 0, V, 1: glVertex3f 0, I + 1, D
        glTexCoord3f 1, V, 1: glVertex3f W, I + 1, D
        glTexCoord3f 1, V, 0: glVertex3f W, I + 1, 0
        glTexCoord3f 0, V, 0: glVertex3f 0, I + 1, 0
    Next I
    For I = 0 To W-1
        V = (I + 0.5) / W
        glNormal3f -1, 0, 0
        glTexCoord3f V, 0, 0: glVertex3f I, 0, 0
        glTexCoord3f V, 0, 1: glVertex3f I, 0, D
        glTexCoord3f V, 1, 1: glVertex3f I, H, D
        glTexCoord3f V, 1, 0: glVertex3f I, H, 0
        glNormal3f 1, 0, 0
        glTexCoord3f V, 0, 1: glVertex3f I + 1, 0, D
        glTexCoord3f V, 0, 0: glVertex3f I + 1, 0, 0
        glTexCoord3f V, 1, 0: glVertex3f I + 1, H, 0
        glTexCoord3f V, 1, 1: glVertex3f I + 1, H, D
    Next I
    
    glEnd
End Sub

Sub InternalVoxelVolume.Render()
    Dim I As Integer, V As Single
    glBegin GL_QUADS
    For I = 0 To D-1
        V = (I + 0.5) / D
        If VisLayerZ.A[I] And 2 Then
            glNormal3f 0, 0, -1
            glTexCoord3f 0, 0, V: glVertex3f 0, 0, I
            glTexCoord3f 0, 1, V: glVertex3f 0, H, I
            glTexCoord3f 1, 1, V: glVertex3f W, H, I
            glTexCoord3f 1, 0, V: glVertex3f W, 0, I
        End If
        If VisLayerZ.A[I] And 1 Then
            glNormal3f 0, 0, 1
            glTexCoord3f 0, 1, V: glVertex3f 0, H, I + 1
            glTexCoord3f 0, 0, V: glVertex3f 0, 0, I + 1
            glTexCoord3f 1, 0, V: glVertex3f W, 0, I + 1
            glTexCoord3f 1, 1, V: glVertex3f W, H, I + 1
        End If
    Next I
    For I = 0 To H-1
        V = (I + 0.5) / H
        If VisLayerY.A[I] And 2 Then
            glNormal3f 0, -1, 0
            glTexCoord3f 0, V, 0: glVertex3f 0, I, 0
            glTexCoord3f 1, V, 0: glVertex3f W, I, 0
            glTexCoord3f 1, V, 1: glVertex3f W, I, D
            glTexCoord3f 0, V, 1: glVertex3f 0, I, D
        End If
        If VisLayerY.A[I] And 1 Then
            glNormal3f 0, 1, 0
            glTexCoord3f 0, V, 1: glVertex3f 0, I + 1, D
            glTexCoord3f 1, V, 1: glVertex3f W, I + 1, D
            glTexCoord3f 1, V, 0: glVertex3f W, I + 1, 0
            glTexCoord3f 0, V, 0: glVertex3f 0, I + 1, 0
        End If
    Next I
    For I = 0 To W-1
        V = (I + 0.5) / W
        If VisLayerX.A[I] And 2 Then
            glNormal3f -1, 0, 0
            glTexCoord3f V, 0, 0: glVertex3f I, 0, 0
            glTexCoord3f V, 0, 1: glVertex3f I, 0, D
            glTexCoord3f V, 1, 1: glVertex3f I, H, D
            glTexCoord3f V, 1, 0: glVertex3f I, H, 0
        End If
        If VisLayerX.A[I] And 1 Then
            glNormal3f 1, 0, 0
            glTexCoord3f V, 0, 1: glVertex3f I + 1, 0, D
            glTexCoord3f V, 0, 0: glVertex3f I + 1, 0, 0
            glTexCoord3f V, 1, 0: glVertex3f I + 1, H, 0
            glTexCoord3f V, 1, 1: glVertex3f I + 1, H, D
        End If
    Next I
    
    glEnd
End Sub

Sub InternalVoxelVolume.RenderAll(V1 As Vec3I, V2 As Vec3I)
    Dim I As Integer, V As Single
    Dim As Vec3F U1 = Vec3F(V1.X / W, V1.Y / H, V1.Z / D), U2 = Vec3F(V2.X / W, V2.Y / H, V2.Z / D)
    
    glBegin GL_QUADS
    For I = V1.Z To V2.Z - 1
        V = (I + 0.5) / D
        glNormal3f 0, 0, -1
        glTexCoord3f U1.X, U1.Y, V: glVertex3f V1.X, V1.Y, I
        glTexCoord3f U1.X, U2.Y, V: glVertex3f V1.X, V2.Y, I
        glTexCoord3f U2.X, U2.Y, V: glVertex3f V2.X, V2.Y, I
        glTexCoord3f U2.X, U1.Y, V: glVertex3f V2.X, V1.Y, I
        glNormal3f 0, 0, 1
        glTexCoord3f U1.X, U2.Y, V: glVertex3f V1.X, V2.Y, I + 1
        glTexCoord3f U1.X, U1.Y, V: glVertex3f V1.X, V1.Y, I + 1
        glTexCoord3f U2.X, U1.Y, V: glVertex3f V2.X, V1.Y, I + 1
        glTexCoord3f U2.X, U2.Y, V: glVertex3f V2.X, V2.Y, I + 1
    Next I
    For I = V1.Y To V2.Y - 1
        V = (I + 0.5) / H
        glNormal3f 0, -1, 0
        glTexCoord3f U1.X, V, U1.Z: glVertex3f V1.X, I, V1.Z
        glTexCoord3f U2.X, V, U1.Z: glVertex3f V2.X, I, V1.Z
        glTexCoord3f U2.X, V, U2.Z: glVertex3f V2.X, I, V2.Z
        glTexCoord3f U1.X, V, U2.Z: glVertex3f V1.X, I, V2.Z
        glNormal3f 0, 1, 0
        glTexCoord3f U1.X, V, U2.Z: glVertex3f V1.X, I + 1, V2.Z
        glTexCoord3f U2.X, V, U2.Z: glVertex3f V2.X, I + 1, V2.Z
        glTexCoord3f U2.X, V, U1.Z: glVertex3f V2.X, I + 1, V1.Z
        glTexCoord3f U1.X, V, U1.Z: glVertex3f V1.X, I + 1, V1.Z
    Next I
    For I = V1.X To V2.X - 1
        V = (I + 0.5) / W
        glNormal3f -1, 0, 0
        glTexCoord3f V, U1.Y, U1.Z: glVertex3f I, V1.Y, V1.Z
        glTexCoord3f V, U1.Y, U2.Z: glVertex3f I, V1.Y, V2.Z
        glTexCoord3f V, U2.Y, U2.Z: glVertex3f I, V2.Y, V2.Z
        glTexCoord3f V, U2.Y, U1.Z: glVertex3f I, V2.Y, V1.Z
        glNormal3f 1, 0, 0
        glTexCoord3f V, U1.Y, U2.Z: glVertex3f I + 1, V1.Y, V2.Z
        glTexCoord3f V, U1.Y, U1.Z: glVertex3f I + 1, V1.Y, V1.Z
        glTexCoord3f V, U2.Y, U1.Z: glVertex3f I + 1, V2.Y, V1.Z
        glTexCoord3f V, U2.Y, U2.Z: glVertex3f I + 1, V2.Y, V2.Z
    Next I
    
    glEnd
End Sub

Sub InternalVoxelVolume.Render(V1 As Vec3I, V2 As Vec3I)
    Dim I As Integer, V As Single
    Dim As Vec3F U1 = Vec3F(V1.X / W, V1.Y / H, V1.Z / D), U2 = Vec3F(V2.X / W, V2.Y / H, V2.Z / D)
    
    glBegin GL_QUADS
    For I = V1.Z To V2.Z - 1
        V = (I + 0.5) / D
        If VisLayerZ.A[I] And 2 Or I = V1.Z Then
            glNormal3f 0, 0, -1
            glTexCoord3f U1.X, U1.Y, V: glVertex3f V1.X, V1.Y, I
            glTexCoord3f U1.X, U2.Y, V: glVertex3f V1.X, V2.Y, I
            glTexCoord3f U2.X, U2.Y, V: glVertex3f V2.X, V2.Y, I
            glTexCoord3f U2.X, U1.Y, V: glVertex3f V2.X, V1.Y, I
        End If
        If VisLayerZ.A[I] And 1 Or I = V2.Z-1 Then
            glNormal3f 0, 0, 1
            glTexCoord3f U1.X, U2.Y, V: glVertex3f V1.X, V2.Y, I + 1
            glTexCoord3f U1.X, U1.Y, V: glVertex3f V1.X, V1.Y, I + 1
            glTexCoord3f U2.X, U1.Y, V: glVertex3f V2.X, V1.Y, I + 1
            glTexCoord3f U2.X, U2.Y, V: glVertex3f V2.X, V2.Y, I + 1
        End If
    Next I
    For I = V1.Y To V2.Y - 1
        V = (I + 0.5) / H
        If VisLayerY.A[I] And 2 Or I = V1.Y Then
            glNormal3f 0, -1, 0
            glTexCoord3f U1.X, V, U1.Z: glVertex3f V1.X, I, V1.Z
            glTexCoord3f U2.X, V, U1.Z: glVertex3f V2.X, I, V1.Z
            glTexCoord3f U2.X, V, U2.Z: glVertex3f V2.X, I, V2.Z
            glTexCoord3f U1.X, V, U2.Z: glVertex3f V1.X, I, V2.Z
        End If
        If VisLayerY.A[I] And 1 Or I = V2.Y-1 Then
            glNormal3f 0, 1, 0
            glTexCoord3f U1.X, V, U2.Z: glVertex3f V1.X, I + 1, V2.Z
            glTexCoord3f U2.X, V, U2.Z: glVertex3f V2.X, I + 1, V2.Z
            glTexCoord3f U2.X, V, U1.Z: glVertex3f V2.X, I + 1, V1.Z
            glTexCoord3f U1.X, V, U1.Z: glVertex3f V1.X, I + 1, V1.Z
        End If
    Next I
    For I = V1.X To V2.X - 1
        V = (I + 0.5) / W
        If VisLayerX.A[I] And 2 Or I = V1.X Then
            glNormal3f -1, 0, 0
            glTexCoord3f V, U1.Y, U1.Z: glVertex3f I, V1.Y, V1.Z
            glTexCoord3f V, U1.Y, U2.Z: glVertex3f I, V1.Y, V2.Z
            glTexCoord3f V, U2.Y, U2.Z: glVertex3f I, V2.Y, V2.Z
            glTexCoord3f V, U2.Y, U1.Z: glVertex3f I, V2.Y, V1.Z
        End If
        If VisLayerX.A[I] And 1 Or I = V2.X-1 Then
            glNormal3f 1, 0, 0
            glTexCoord3f V, U1.Y, U2.Z: glVertex3f I + 1, V1.Y, V2.Z
            glTexCoord3f V, U1.Y, U1.Z: glVertex3f I + 1, V1.Y, V1.Z
            glTexCoord3f V, U2.Y, U1.Z: glVertex3f I + 1, V2.Y, V1.Z
            glTexCoord3f V, U2.Y, U2.Z: glVertex3f I + 1, V2.Y, V2.Z
        End If
    Next I
    
    glEnd
End Sub

Sub InternalVoxelVolume.DetermineLayerVisability()
    Dim As UInteger X, Y, Z, I, WH = W*H
    If ClientTex.UBound_ <= -1 Then Exit Sub
    VisLayerX.ReDim_ W-1
    VisLayerY.ReDim_ H-1
    VisLayerZ.ReDim_ D-1
    'X
    For X = 0 To W-2
        For Y = 0 To H-1
            I = X + W*Y
            For Z = 0 To D-1
                If ClientTex.A[I] <> 0 And ClientTex.A[I+1] = 0 Then
                    VisLayerX.A[X] Or= 1
                    Exit For, For
                End If
                I += WH
            Next Z
        Next Y
        For Y = 0 To H-1
            I = X + W*Y
            For Z = 0 To D-1
                If ClientTex.A[I+1] <> 0 And ClientTex.A[I] = 0 Then
                    VisLayerX.A[X+1] Or= 2
                    Exit For, For
                End If
                I += WH
            Next Z
        Next Y
    Next X
    For Y = 0 To H-1
        I = X + W*Y
        For Z = 0 To D-1
            If ClientTex.A[I] <> 0 Then
                VisLayerX.A[X] Or= 1
                Exit For, For
            End If
            I += WH
        Next Z
    Next Y
    For Y = 0 To H-1
        I = W*Y
        For Z = 0 To D-1
            If ClientTex.A[I] <> 0 Then
                VisLayerX.A[0] Or= 2
                Exit For, For
            End If
            I += WH
        Next Z
    Next Y
    'Y
    For Y = 0 To H-2
        For X = 0 To W-1
            I = X + W*Y
            For Z = 0 To D-1
                If ClientTex.A[I] <> 0 And ClientTex.A[I+W] = 0 Then
                    VisLayerY.A[Y] Or= 1
                    Exit For, For
                End If
                I += WH
            Next Z
        Next X
        For X = 0 To W-1
            I = X + W*Y
            For Z = 0 To D-1
                If ClientTex.A[I+W] <> 0 And ClientTex.A[I] = 0 Then
                    VisLayerY.A[Y+1] Or= 2
                    Exit For, For
                End If
                I += WH
            Next Z
        Next X
    Next Y
    For X = 0 To W-1
        I = X + W*Y
        For Z = 0 To D-1
            If ClientTex.A[I] <> 0 Then
                VisLayerY.A[Y] Or= 1
                Exit For, For
            End If
            I += WH
        Next Z
    Next X
    For X = 0 To W-1
        I = X
        For Z = 0 To D-1
            If ClientTex.A[I] <> 0 Then
                VisLayerY.A[0] Or= 2
                Exit For, For
            End If
            I += WH
        Next Z
    Next X
    'Z
    For Z = 0 To D-2
        For X = 0 To W-1
            I = X + WH*Z
            For Y = 0 To H-1
                If ClientTex.A[I] <> 0 And ClientTex.A[I+WH] = 0 Then
                    VisLayerZ.A[Z] Or= 1
                    Exit For, For
                End If
                I += W
            Next Y
        Next X
        For X = 0 To W-1
            I = X + WH*Z
            For Y = 0 To H-1
                If ClientTex.A[I+WH] <> 0 And ClientTex.A[I] = 0 Then
                    VisLayerZ.A[Z+1] Or= 2
                    Exit For, For
                End If
                I += W
            Next Y
        Next X
    Next Z
    For X = 0 To W-1
        I = X + WH*Z
        For Y = 0 To H-1
            If ClientTex.A[I] <> 0 Then
                VisLayerZ.A[Z] Or= 1
                Exit For, For
            End If
            I += W
        Next Y
    Next X
    For X = 0 To W-1
        I = X
        For Y = 0 To H-1
            If ClientTex.A[I] <> 0 Then
                VisLayerZ.A[0] Or= 2
                Exit For, For
            End If
            I += W
        Next Y
    Next X
End Sub

Sub InternalVoxelVolume.Lock()
    If LockedCount = 0 And VolType = Volume_Static Then
        ClientTex.ReDim_ W * H * D - 1
        
        glBindTexture GL_TEXTURE_3D, Tex
        glGetTexImage GL_TEXTURE_3D, 0, GL_RGBA, GL_UNSIGNED_BYTE, ClientTex
        glBindTexture GL_TEXTURE_3D, 0
    End If
    LockedCount += 1
End Sub

Sub InternalVoxelVolume.UnLock()
    If LockedCount = 1 Then
        If VolType <> Volume_Offscreen Then
            glBindTexture GL_TEXTURE_3D, Tex
            If glTexImage3D <> NULL Then glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA8, W, H, D, 0, GL_RGBA, GL_UNSIGNED_BYTE, ClientTex)
            glBindTexture GL_TEXTURE_3D, 0
        End If
        If VolType = Volume_Static Then
            DetermineLayerVisability
            ClientTex.Erase_
        End If
    End If
    If LockedCount > 0 Then LockedCount -= 1
End Sub

Sub InternalVoxelVolume.UnLockNoUpdate()
    If LockedCount = 1 And VolType = Volume_Static Then ClientTex.Erase_
    If LockedCount > 0 Then LockedCount -= 1
End Sub

End Namespace 'InternalVoxelGFX