#Include "VoxelGFX.bi"

#Include "modVolume.bi"
#Include "modGeometry.bi"

'PNG File Handeling
#Define PNG_STATICZ
#Include Once "fbpng.bi"
#Include Once "File.bi"

Namespace InternalVoxelGFX

Type Bytef As Byte
Type uInt As UInteger

extern "c"
declare function crc32 (byval crc as uLong, byval buf as Bytef ptr, byval len as uInt) as uLong
end extern

#Define make_u32(n) ((((n) and &h000000ff) shl 24) _
                  or (((n) and &h0000ff00) shl 8) _
                  or (((n) and &h00ff0000) shr 8) _
                  or (((n) and &hff000000) shr 24))

#Define put_u32(p, n) *cptr( uinteger ptr, p ) = make_u32(n)
'End PNG File Handeling

#Define OutsideVolume(V) ((V).X < 0 OrElse (V).Y < 0 OrElse (V).Z < 0 OrElse (V).X >= .W OrElse (V).Y >= .H OrElse (V).Z >= .D)
#Define InsideVolume(V) ((V).X >= 0 AndAlso (V).Y >= 0 AndAlso (V).Z >= 0 AndAlso (V).X < .W AndAlso (V).Y < .H AndAlso (V).Z < .D)

#Define RGBA_R(C) (CUInt(C) Shr 16 And 255)
#Define RGBA_G(C) (CUInt(C) Shr  8 And 255)
#Define RGBA_B(C) (CUInt(C)        And 255)
#Define RGBA_A(C) (CUInt(C) Shr 24        )

'#Define SwapRB(C) RGBA(RGBA_B(C), RGBA_G(C), RGBA_R(C), RGBA_A(C))
#Define SwapRB(C) (CUInt(C) Shr 16 And &HFF Or (CUInt(C) And &HFF) Shl 16 Or CUInt(C) And &HFF00FF00)

Type VoxelGFXContext
    DefaultVol As Vox_Volume
    CurVol As Vox_Volume
    CurColor As UInteger
    SourceVol As Vox_Volume = -1
    Camera As Camera3D
    DrawPos As Vec3I
    BlitPerm(2) As UByte = {0, 1, 2}
    BlitReflect As UByte
End Type

Dim Shared InternalVoxModels() As InternalVoxelVolume
Dim Shared VC As VoxelGFXContext Ptr, VoxContext() As VoxelGFXContext

End Namespace
Using InternalVoxelGFX

Extern "C++"

Constructor Vec3I()
    X = 0
    Y = 0
    Z = 0
End Constructor

Constructor Vec3I(V As Vec3I)
    X = V.X
    Y = V.Y
    Z = V.Z
End Constructor

Constructor Vec3I(X As Integer, Y As Integer, Z As Integer)
    This.X = X
    This.Y = Y
    This.Z = Z
End Constructor

Operator Vec3I.+= (ByRef Rhs As Vec3I)
    X += Rhs.X
    Y += Rhs.Y
    Z += Rhs.Z
End Operator

Operator Vec3I.-= (ByRef Rhs As Vec3I)
    X -= Rhs.X
    Y -= Rhs.Y
    Z -= Rhs.Z
End Operator

Operator Vec3I.*= (ByRef Rhs As Integer)
    X *= Rhs
    Y *= Rhs
    Z *= Rhs
End Operator

Operator Vec3I.\= (ByRef Rhs As Integer)
    X \= Rhs
    Y \= Rhs
    Z \= Rhs
End Operator

Operator Vec3I.Cast () As String
    Return "(" & X & ", " & Y & ", " & Z & ")"
End Operator

Operator -(ByRef Rhs As Vec3I) As Vec3I
    Return Type(-Rhs.X, -Rhs.Y, -Rhs.Z)
End Operator

Operator Abs(ByRef Rhs As Vec3I) As Double
    Return Sqr(Rhs.X*Rhs.X + Rhs.Y*Rhs.Y + Rhs.Z*Rhs.Z)
End Operator

Operator + (ByRef Lhs As Vec3I, ByRef Rhs As Vec3I) As Vec3I
    Return Type(Lhs.X + Rhs.X, Lhs.Y + Rhs.Y, Lhs.Z + Rhs.Z)
End Operator

Operator - (ByRef Lhs As Vec3I, ByRef Rhs As Vec3I) As Vec3I
    Return Type(Lhs.X - Rhs.X, Lhs.Y - Rhs.Y, Lhs.Z - Rhs.Z)
End Operator

'Scalar Products
Operator * (ByRef Lhs As Vec3I, ByRef Rhs As Integer) As Vec3I
    Return Type(Lhs.X * Rhs, Lhs.Y * Rhs, Lhs.Z * Rhs)
End Operator
Operator * (ByRef Lhs As Integer, ByRef Rhs As Vec3I) As Vec3I
    Return Type(Lhs * Rhs.X, Lhs * Rhs.Y, Lhs * Rhs.Z)
End Operator
Operator \ (ByRef Lhs As Vec3I, ByRef Rhs As Integer) As Vec3I
    Return Type(Lhs.X \ Rhs, Lhs.Y \ Rhs, Lhs.Z \ Rhs)
End Operator
'Dot Product
Operator * (ByRef Lhs As Vec3I, ByRef Rhs As Vec3I) As Integer
    Return Lhs.X*Rhs.X + Lhs.Y*Rhs.Y + Lhs.Z*Rhs.Z
End Operator

Operator = (ByRef Lhs As Vec3I, ByRef Rhs As Vec3I) As Integer
    Return (Lhs.X = Rhs.X And Lhs.Y = Rhs.Y And Lhs.Z = Rhs.Z)
End Operator

Operator <> (ByRef Lhs As Vec3I, ByRef Rhs As Vec3I) As Integer
    Return (Lhs.X <> Rhs.X Or Lhs.Y <> Rhs.Y Or Lhs.Z <> Rhs.Z)
End Operator

'Sub VoxInit(GlExtFetch As Function Stdcall(ByVal Proc As ZString Ptr) As Any Ptr, Flags As UInteger = 0)
Sub VoxInit(GlExtFetch As Function Stdcall(ByRef Proc As Const ZString) As Any Ptr = NULL, Flags As UInteger = 0)
    If GlExtFetch <> NULL Then
        glTexImage3D = GlExtFetch("glTexImage3D")
        glTexSubImage3D = GlExtFetch("glTexSubImage3D")
        glGenBuffers = GlExtFetch("glGenBuffers")
        glBindBuffer = GlExtFetch("glBindBuffer")
        glBufferData = GlExtFetch("glBufferData")
    End If
    
    ReDim VoxContext(0)
    VC = @VoxContext(0)
    
    ReDim InternalVoxModels(0)
    
    If Flags And &H000F& Then
        Dim As Integer I = 1 Shl (Flags And &H000F&)
        VoxScreenRes I, I, I
    End If
End Sub

Sub VoxScreenRes(Size As Vec3I, BackColor As UInteger = 0)
    VoxScreenRes Size.X, Size.Y, Size.Z, BackColor
End Sub

Sub VoxScreenRes(SizeX As Integer, SizeY As Integer, SizeZ As Integer, BackColor As UInteger = 0)
    Var Temp = VC->CurVol
    VC->CurVol = VC->DefaultVol
    glClearColor RGBA_R(BackColor)/255.0, RGBA_G(BackColor)/255.0, RGBA_B(BackColor)/255.0, 1.0f
    
    VoxSizeVolume SizeX, SizeY, SizeZ
    
    VC->Camera.MidP = Vec3F(SizeX/2.0, SizeY/2.0, SizeZ/2.0)
    VC->Camera.Location = Vec3F(SizeX/2.0, SizeY/2.0, (SizeX+SizeY)/2.0+1.5*SizeZ)
    
    VC->CurVol = Temp
End Sub

Function VoxNewVolume(T As VoxVolumeType = Volume_Dynamic) As Vox_Volume
    ReDim Preserve InternalVoxModels(UBound(InternalVoxModels) + 1)
    VC->CurVol = UBound(InternalVoxModels)
    InternalVoxModels(VC->CurVol).VolType = T
    Return UBound(InternalVoxModels)
End Function

Function VoxNewVolume(Size As Vec3I, T As VoxVolumeType = Volume_Dynamic) As Vox_Volume
    Function = VoxNewVolume(T)
    VoxSizeVolume Size
End Function

Function VoxNewVolume(SizeX As Integer, SizeY As Integer, SizeZ As Integer, T As VoxVolumeType = Volume_Dynamic) As Vox_Volume
    Return VoxNewVolume(Vec3I(SizeX, SizeY, SizeZ), T)
End Function

Sub VoxSizeVolume(Size As Vec3I)
    VoxSizeVolume Size.X, Size.Y, Size.Z
End Sub

Sub VoxSizeVolume(SizeX As Integer, SizeY As Integer, SizeZ As Integer)
    If SizeX < 1 Then SizeX = 1
    If SizeY < 1 Then SizeY = 1
    If SizeZ < 1 Then SizeZ = 1
    With InternalVoxModels(VC->CurVol)
        .W = SizeX
        .H = SizeY
        .D = SizeZ
        
        If .VolType <> Volume_OffScreen Then
            If .Tex = 0 Then glGenTextures(1, @.Tex)
            glBindTexture GL_TEXTURE_3D, .Tex
            glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP
            glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP
            glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP
            
            glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_NEAREST
            glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_NEAREST
            
            If glTexImage3D <> NULL Then glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA8, .W, .H, .D, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL)
            glBindTexture GL_TEXTURE_3D, 0
        End If
        If .VolType <> Volume_Static Then .ClientTex.ReDim_ .W * .H * .D - 1
    End With
End Sub

Function VoxGetVolumeSize(Vol As Vox_Volume = -2) As Vec3I
    If Vol = VOXEL_SCREEN Then Vol = VC->DefaultVol
    If Vol < 0 Or Vol > UBound(InternalVoxModels) Then
        With InternalVoxModels(VC->CurVol)
            Return Vec3I(.W, .H, .D)
        End With
       Else
        With InternalVoxModels(Vol)
            Return Vec3I(.W, .H, .D)
        End With
    End If
End Function

Sub VoxReloadVolumes
    For Model As Vox_Volume = 0 To UBound(InternalVoxModels)
        With InternalVoxModels(Model)
            If .VolType <> Volume_Offscreen Then
                glDeleteTextures 1, @.Tex
                glGenTextures 1, @.Tex
                glBindTexture GL_TEXTURE_3D, .Tex
                glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP
                glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP
                glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP
                
                glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_NEAREST
                glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_NEAREST
                
                glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA8, .W, .H, .D, 0, GL_RGBA, GL_UNSIGNED_BYTE, .ClientTex)
            End If
        End With
    Next Model
End Sub

Function VoxNewContext(ScreenVolume As Vox_Volume = VOXEL_SCREEN) As Vox_Context
    If ScreenVolume = VOXEL_SCREEN Then ScreenVolume = VC->DefaultVol
    ReDim Preserve VoxContext(UBound(VoxContext) + 1) As VoxelGFXContext
    If ScreenVolume < 0 Or ScreenVolume > UBound(InternalVoxModels) Then ScreenVolume = 0
    VC = @(VoxContext(UBound(VoxContext)))
    VC->DefaultVol = ScreenVolume
    VC->CurVol = ScreenVolume
    With InternalVoxModels(ScreenVolume)
        VC->Camera.MidP = Vec3F(.W/2.0, .H/2.0, .D/2.0)
        VC->Camera.Location = Vec3F(.W/2.0, .H/2.0, (.W+.H)/2.0+1.5*.D)
    End With
    Return UBound(VoxContext)
End Function

Function VoxLoadFile(FileName As ZString, Depth As Integer = 0, T As VoxVolumeType = Volume_OffScreen) As Vox_Volume
    Dim P As Any Ptr
    Function = VoxNewVolume(T)
    
    P = png_load(FileName, PNG_TARGET_OPENGL)
    If P <> NULL Then
        With InternalVoxModels(VC->CurVol)
            png_dimensions(FileName, .W, .H)
            
            If Depth <> 0 AndAlso .H Mod Depth <> 0 Then Depth = 0
            If Depth = 0 Then  'Load voXdepth from the file
                Dim As UByte B(FileLen(FileName)-1)
                Dim As Integer F = FreeFile
                Open FileName For Binary As #F
                    Get #F, , B(0), UBound(B)+1
                Close #F
                Dim As Integer I = 8, J = Any
                Do Until I > UBound(B)
                    J = ((B(I) Shl 8 Or B(I+1)) Shl 8 Or B(I+2)) Shl 8 Or B(I+3)
                    If B(I+4) = Asc("v") And B(I+5) = Asc("o") And B(I+6) = Asc("X") And B(I+7) = Asc("d") Then
                        If B(I+8) = Asc("e") And B(I+9) = Asc("p") And B(I+10) = Asc("t") And B(I+11) = Asc("h") Then
                            I += 12
                            Depth = ((B(I) Shl 8 Or B(I+1)) Shl 8 Or B(I+2)) Shl 8 Or B(I+3)
                            Exit Do
                        End If
                    End If
                    I += J+12
                Loop
            End If
            If Depth <> 0 AndAlso .H Mod Depth <> 0 Then Depth = 0
            If Depth = 0 Then Depth = .H\.W
            If Depth <> 0 AndAlso .H Mod Depth <> 0 Then Depth = 0
            If Depth = 0 Then Depth = 1
            .D = Depth
            .H \= .D
            
            If .VolType <> Volume_OffScreen Then
                If .Tex = 0 Then glGenTextures(1, @.Tex)
                glBindTexture GL_TEXTURE_3D, .Tex
                glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP
                glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP
                glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP
                
                glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_NEAREST
                glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_NEAREST
                
                If glTexImage3D <> NULL Then glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA8, .W, .H, .D, 0, GL_RGBA, GL_UNSIGNED_BYTE, P)
                glBindTexture GL_TEXTURE_3D, 0
            End If
            If .VolType <> Volume_Static Then
                .ClientTex.ReDim_ .W * .H * .D - 1
                For I As Integer = 0 To .ClientTex.UBound_
                    .ClientTex.A[I] = Cast(UInteger Ptr, P)[I]
                Next I
            End If
        End With
    End If
End Function

Sub VoxSaveFile Alias "VoxSaveFile" (FileName As ZString, V As Vox_Volume)
    If V = VOXEL_SCREEN Then V = VC->DefaultVol
    If V < 0 Or V > UBound(InternalVoxModels) Then Exit Sub
    Dim As Integer I, J, L, F = Any
    With InternalVoxModels(V)
        .Lock  'New Style Freebasic Image Buffer
        Dim As UInteger Ptr B = New UInteger[.ClientTex.UBound_ + 9]
        B[0] = 7
        B[1] = 4
        B[2] = .W
        B[3] = .H * .D
        B[4] = .W * 4
        J = .ClientTex.UBound_+9+.W
        For I = 0 To .ClientTex.UBound_
            If I Mod .W = 0 Then J -= 2*.W
            B[I+J] = SwapRB(.ClientTex.A[I])
        Next I
        .UnLock
        Dim As UByte Ptr P = png_save_mem(L, B, PNG_TARGET_FBNEW)
        Delete[] B: B = NULL
        If P = NULL Then Exit Sub
        
        I = (((P[8] Shl 8 Or P[9]) Shl 8 Or P[10]) Shl 8 Or P[11]) + 20
        
        Dim As UByte voXd(19) = {0, 0, 0, 8, Asc("v"), Asc("o"), Asc("X"), Asc("d"), Asc("e"), Asc("p"), Asc("t"), Asc("h")}
        put_u32(@voXd(12), .D)
        Dim As UInteger crc = Any
	    crc = crc32(0, @voXd(4), 4)
	    crc = crc32(crc, @voXd(8), 8)
	    put_u32(@voXd(16), crc)
	    
	    If FileExists(FileName) Then Kill FileName
	    F = FreeFile
        Open FileName For Binary As #F
            Put #F, , P[0], I
            Put #F, , voXd(0), 20  'Insert voXdepth Chunk
            Put #F, , P[I], L - I
        Close #F
        Delete P: P = NULL
    End With
End Sub

Sub VoxSetContext(C As Vox_Context = 0)
    If C > UBound(VoxContext) Or C < 0 Then C = 0
    VC = @(VoxContext(C))
End Sub

Sub VoxSetVolumeType(T As VoxVolumeType)
    With InternalVoxModels(VC->CurVol)
        If T = .VolType Then Exit Sub
        If .VolType = Volume_Static Then
            .ClientTex.ReDim_ .W * .H * .D - 1
            
            glBindTexture GL_TEXTURE_3D, .Tex
            glGetTexImage GL_TEXTURE_3D, 0, GL_RGBA, GL_UNSIGNED_BYTE, .ClientTex
            glBindTexture GL_TEXTURE_3D, 0
        End If
        If T = Volume_OffScreen Then
            glDeleteTextures(1, @.Tex)
            .Tex = 0
           Else
            If .VolType = Volume_OffScreen Then
                If .Tex = 0 Then glGenTextures(1, @.Tex)
                glBindTexture GL_TEXTURE_3D, .Tex
                glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP
                glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP
                glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP
                
                glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_NEAREST
                glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_NEAREST
                
                glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA8, .W, .H, .D, 0, GL_RGBA, GL_UNSIGNED_BYTE, .ClientTex)
                
                glBindTexture GL_TEXTURE_3D, 0
            End If
        End If
        If T = Volume_Static Then
            .DetermineLayerVisability
            .ClientTex.Erase_
        End If
        .VolType = T
    End With
End Sub

Sub VoxSetColor(C As UInteger)
    VC->CurColor = SwapRB(C)
    VC->SourceVol = -1
End Sub

Sub VoxSetVolume(Vol As Vox_Volume = VOXEL_SCREEN)
    If Vol = VOXEL_SCREEN Then Vol = VC->DefaultVol
    If Vol < 0 Or Vol > UBound(InternalVoxModels) Then Exit Sub
    VC->CurVol = Vol
End Sub

Sub VoxSetSource(Vol As Vox_Volume)
    If Vol = VOXEL_SCREEN Then Vol = VC->DefaultVol
    If Vol < 0 Or Vol > UBound(InternalVoxModels) Then Exit Sub
    VC->SourceVol = Vol 
End Sub

Sub VoxSetBlitDefault
    VC->BlitPerm(0) = 0
    VC->BlitPerm(1) = 1
    VC->BlitPerm(2) = 2
    VC->BlitReflect = 0
End Sub

Sub VoxBlitRightRotate(Axis As UInteger, ByVal Amount As Integer = 1)
    Amount Mod= 4
    If Amount < 0 Then Amount += 4
    If VC->BlitReflect And 1 Then VC->BlitPerm(0) Or= 128
    If VC->BlitReflect And 2 Then VC->BlitPerm(1) Or= 128
    If VC->BlitReflect And 4 Then VC->BlitPerm(2) Or= 128
    Select Case Axis
    Case VOXEL_AXIS_X '(X, -Z, Y)
        For I As Integer = 1 To Amount
            Swap VC->BlitPerm(1), VC->BlitPerm(2)
            VC->BlitPerm(1) XOr= 128
        Next I
    Case VOXEL_AXIS_Y '(Z, Y, -X)
        For I As Integer = 1 To Amount
            Swap VC->BlitPerm(0), VC->BlitPerm(2)
            VC->BlitPerm(2) XOr= 128
        Next I
    Case VOXEL_AXIS_Z '(-Y, X, Z)
        For I As Integer = 1 To Amount
            Swap VC->BlitPerm(0), VC->BlitPerm(1)
            VC->BlitPerm(0) XOr= 128
        Next I
    End Select
    VC->BlitReflect = 0
    If VC->BlitPerm(0) And 128 Then VC->BlitReflect XOr= 1
    If VC->BlitPerm(1) And 128 Then VC->BlitReflect XOr= 2
    If VC->BlitPerm(2) And 128 Then VC->BlitReflect XOr= 4
    VC->BlitPerm(0) And= 127
    VC->BlitPerm(1) And= 127
    VC->BlitPerm(2) And= 127
End Sub

Sub VoxBlitReflect(Axis As UInteger)
    If Axis = VOXEL_AXIS_X Then VC->BlitReflect XOr= 1
    If Axis = VOXEL_AXIS_Y Then VC->BlitReflect XOr= 2
    If Axis = VOXEL_AXIS_Z Then VC->BlitReflect XOr= 4
End Sub

Sub VoxCls
    With InternalVoxModels(VC->CurVol)
        .Lock
        For I As Integer = 0 To .ClientTex.UBound_
            .ClientTex.A[I] = 0
        Next I
        .UnLock
    End With
End Sub

Sub VoxVolumeLock()
    InternalVoxModels(VC->CurVol).Lock
End Sub

Function VoxVolumePtr() As UInteger Ptr
    If InternalVoxModels(VC->CurVol).LockedCount = 0 Then Return NULL
    Return InternalVoxModels(VC->CurVol).ClientTex.A
End Function

Sub VoxVolumeUnlock()
    InternalVoxModels(VC->CurVol).UnLock
End Sub

Sub VSet(V As Vec3I)
    VC->DrawPos = V
    With InternalVoxModels(VC->CurVol)
        If OutsideVolume(V) Then Exit Sub
        If .ClientTex.UBound_ > -1 Then .ClientTex.A[V.X+.W*(V.Y+.H*V.Z)] = VC->CurColor
        If .LockedCount = 0 And .VolType <> Volume_Offscreen Then
            glBindTexture GL_TEXTURE_3D, .Tex
            If glTexSubImage3D <> NULL Then glTexSubImage3D(GL_TEXTURE_3D, 0, V.X, V.Y, V.Z, 1, 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, @VC->CurColor)
            glBindTexture GL_TEXTURE_3D, 0
        End If
    End With
End Sub

Sub VSet(V As Vec3I, ByVal C As UInteger)
    VC->DrawPos = V
    With InternalVoxModels(VC->CurVol)
        If OutsideVolume(V) Then Exit Sub
        C = SwapRB(C)
        If .ClientTex.UBound_ > -1 Then .ClientTex.A[V.X+.W*(V.Y+.H*V.Z)] = C
        If .LockedCount = 0 And .VolType <> Volume_Offscreen Then
            glBindTexture GL_TEXTURE_3D, .Tex
            If glTexSubImage3D <> NULL Then glTexSubImage3D(GL_TEXTURE_3D, 0, V.X, V.Y, V.Z, 1, 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, @C)
            glBindTexture GL_TEXTURE_3D, 0
        End If
    End With
End Sub

Sub VoxLine(A As Vec3I, B As Vec3I)
    Dim V As Vec3I = A - B
    Dim Max As Integer = Abs(V.X)
    If Abs(V.Y) > Max Then Max = Abs(V.Y)
    If Abs(V.Z) > Max Then Max = Abs(V.Z)
    
    '(2*Max)*0 = B.X*2*Max + (A.X-B.X)*T
    
    With InternalVoxModels(VC->CurVol)
        Dim As Integer W = .W, H = .H, D = .D '<- Temporary until FBC bug fix
        .Lock
        If OutsideVolume(A) OrElse OutsideVolume(B) Then
            For T As Integer = 0 To 2*Max - 2 Step 2 'Note: Integer division rounds toward 0
                V = (A*T + B*(2*Max-T) + 3*Vec3I(Max, Max, Max))\(2*Max) - Vec3I(1, 1, 1)
                If InsideVolume(V) Then .ClientTex.A[V.X+.W*(V.Y+.H*V.Z)] = VC->CurColor
            Next T
            If InsideVolume(A) Then .ClientTex.A[A.X+W*(A.Y+H*A.Z)] = VC->CurColor
           Else
            For T As Integer = 0 To 2*Max - 2 Step 2
                V = (A*T + B*(2*Max-T) + Vec3I(Max, Max, Max))\(2*Max)
                Dim I As Integer = V.X+.W*(V.Y+.H*V.Z) '<- Another work arround
                .ClientTex.A[I] = VC->CurColor
                '.ClientTex.A[V.X+.W*(V.Y+.H*V.Z)] = VC->CurColor
            Next T
            .ClientTex.A[A.X+.W*(A.Y+.H*A.Z)] = VC->CurColor
        End If
        .UnLock
    End With
    VC->DrawPos = B
End Sub

Sub VoxLineTo(B As Vec3I)
    VoxLine VC->DrawPos, B
End Sub

Sub VoxBlit(ByVal DestV As Vec3I, ByVal SrcV As Vec3I, ByVal Size As Vec3I)
    If VC->CurVol = VC->SourceVol Or VC->SourceVol = -1 Then Exit Sub
    
    Dim IP(2) As Byte = Any 'Inverse Permutation
    IP(VC->BlitPerm(0)) = 0
    IP(VC->BlitPerm(1)) = 1
    IP(VC->BlitPerm(2)) = 2
    
    'Shift coordinates so that Size is positive
    If Size.X < 0 Then
        DestV.V(IP(0)) += Size.X+1
        SrcV.X += Size.X+1
        Size.X = -Size.X
    End If
    If Size.Y < 0 Then
        DestV.V(IP(1)) += Size.Y+1
        SrcV.Y += Size.Y+1
        Size.Y = -Size.Y
    End If
    If Size.Z < 0 Then
        DestV.V(IP(2)) += Size.Z+1
        SrcV.Z += Size.Z+1
        Size.Z = -Size.Z
    End If
    
    With InternalVoxModels(VC->SourceVol) 'Clip against the source
        If SrcV.X + Size.X > .W Then Size.X = .W - SrcV.X
        If SrcV.Y + Size.Y > .H Then Size.Y = .H - SrcV.Y
        If SrcV.Z + Size.Z > .D Then Size.Z = .D - SrcV.Z
        If SrcV.X < 0 Then Size.X += SrcV.X: SrcV.X = 0
        If SrcV.Y < 0 Then Size.Y += SrcV.Y: SrcV.Y = 0
        If SrcV.Z < 0 Then Size.Z += SrcV.Z: SrcV.Z = 0
    End With
    
    With InternalVoxModels(VC->CurVol) 'Clip against the destination
        If DestV.X + Size.V(VC->BlitPerm(0)) > .W Then Size.V(VC->BlitPerm(0)) = .W - DestV.X
        If DestV.Y + Size.V(VC->BlitPerm(1)) > .H Then Size.V(VC->BlitPerm(1)) = .H - DestV.Y
        If DestV.Z + Size.V(VC->BlitPerm(2)) > .D Then Size.V(VC->BlitPerm(2)) = .D - DestV.Z
        If DestV.X < 0 Then Size.V(VC->BlitPerm(0)) += DestV.X: DestV.X = 0
        If DestV.Y < 0 Then Size.V(VC->BlitPerm(1)) += DestV.Y: DestV.Y = 0
        If DestV.Z < 0 Then Size.V(VC->BlitPerm(2)) += DestV.Z: DestV.Z = 0
        
        'IP(0) = VC->BlitPerm(0) 'No Longer Inverse
        'IP(1) = VC->BlitPerm(1)
        'IP(2) = VC->BlitPerm(2)
        
        Dim As Integer Xd, Yd, Zd, I
        Dim As Integer Xs, Ys, Zs, J, K
        Dim As Vec3I S
        
        If IP(0) = 0 Then K = 1
        If IP(0) = 1 Then K = InternalVoxModels(VC->SourceVol).W
        If IP(0) = 2 Then K = InternalVoxModels(VC->SourceVol).W * InternalVoxModels(VC->SourceVol).H
        If VC->BlitReflect And 1 Then K = -K
        
        InternalVoxModels(VC->SourceVol).Lock
        .Lock
        S.V(VC->BlitPerm(2)) = SrcV.V(VC->BlitPerm(2))
        If VC->BlitReflect And 4 Then S.V(VC->BlitPerm(2)) += Size.V(VC->BlitPerm(2))-1
        For Zd = DestV.Z To DestV.Z + Size.V(VC->BlitPerm(2))-1
            S.V(VC->BlitPerm(1)) = SrcV.V(VC->BlitPerm(1))
            If VC->BlitReflect And 2 Then S.V(VC->BlitPerm(1)) += Size.V(VC->BlitPerm(1))-1
            For Yd = DestV.Y To DestV.Y + Size.V(VC->BlitPerm(1))-1
                I = DestV.X + .W*(Yd + .H*Zd)
                S.V(VC->BlitPerm(0)) = SrcV.V(VC->BlitPerm(0))
                If VC->BlitReflect And 1 Then S.V(VC->BlitPerm(0)) += Size.V(VC->BlitPerm(0))-1
                J = S.X + InternalVoxModels(VC->SourceVol).W*(S.Y + InternalVoxModels(VC->SourceVol).H*S.Z)
                For Xd = DestV.X To DestV.X + Size.V(VC->BlitPerm(0))-1
                    .ClientTex.A[I] = InternalVoxModels(VC->SourceVol).ClientTex.A[J]
                    J += K
                    I += 1
                Next Xd
                S.V(VC->BlitPerm(1)) += IIf(VC->BlitReflect And 2, -1, 1)
            Next Yd
            S.V(VC->BlitPerm(2)) += IIf(VC->BlitReflect And 4, -1, 1)
        Next Zd
        .UnLock
        InternalVoxModels(VC->SourceVol).UnLockNoUpdate
    End With
End Sub

Sub SetUpLights
    Dim Ler(3) As GLfloat = {0.2f, 0.2f, 0.2f, 1.0f}
    glLightModelfv GL_LIGHT_MODEL_AMBIENT, @Ler(0)
    Dim Lor(3) As GLfloat = {1, 1, 1, 1}
    glLightfv GL_LIGHT0, GL_DIFFUSE, @Lor(0)
    Dim Lur(3) As GLfloat = {0.5, 0.5, 0.5, 1}
    glLightfv GL_LIGHT1, GL_DIFFUSE, @Lur(0)
    
    Dim Lar(3) As GLfloat = {-0.25, 0.5, 1.0, 0.0}
    glLightfv GL_LIGHT0, GL_POSITION, @Lar(0)
    Dim L(3) As GLfloat = {0.75, -0.25, -0.5, 0.0}
    glLightfv GL_LIGHT1, GL_POSITION, @L(0)
    
    glEnable GL_LIGHTING
    glEnable GL_LIGHT0
    glEnable GL_LIGHT1
    
    glEnable GL_COLOR_MATERIAL
    glColorMaterial GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE
End Sub

Sub VoxRender(ScreenW As Integer, ScreenH As Integer, Flags As UInteger = 0)
    VoxGlRenderState ScreenW, ScreenH, Flags
    VoxRenderVolume VC->DefaultVol
End Sub

Sub VoxGlRenderState(ScreenW As Integer = 0, ScreenH As Integer = 0, Flags As UInteger = 0)
    If (Flags And VOXEL_NOCLEAR) = 0 Then glClear GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT
    If (Flags And VOXEL_NOLIGHT) = 0 Then SetUpLights
    If ScreenW <> 0 And ScreenH <> 0 Then AspectProjectionView ScreenW, ScreenH
    If (Flags And VOXEL_NOMODELVIEW) = 0 Then VC->Camera.SetCamera
    
    If (Flags And VOXEL_NOGLSTATE) = 0 Then
        glCullFace GL_BACK
        glFrontFace GL_CCW
        glEnable GL_CULL_FACE
        glEnable GL_DEPTH_TEST
        glTexEnvf GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE
        glAlphaFunc GL_GREATER, 0.0
        glEnable GL_ALPHA_TEST
        
        glColor4ub(255, 255, 255, 255)
        glEnable GL_TEXTURE_3D
    End If
End Sub

Sub VoxRenderVolume(Model As Vox_Volume)
    If Model = VOXEL_SCREEN Then Model = VC->DefaultVol
    If Model < 0 Or Model > UBound(InternalVoxModels) Then Exit Sub
    With InternalVoxModels(Model)
        Select Case .VolType
        Case Volume_Dynamic
            glBindTexture GL_TEXTURE_3D, .Tex
            .RenderAll
        Case Volume_Static
            glBindTexture GL_TEXTURE_3D, .Tex
            .Render
        Case Volume_Offscreen
            If .Tex = 0 Then glGenTextures(1, @.Tex)
            glBindTexture GL_TEXTURE_3D, .Tex
            glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP
            glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP
            glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP
            
            glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_NEAREST
            glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_NEAREST
            
            If glTexImage3D <> NULL Then
                glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA8, .W, .H, .D, 0, GL_RGBA, GL_UNSIGNED_BYTE, .ClientTex)
                .RenderAll
                glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA8, 0, 0, 0, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL)
            End If 
        End Select
        glBindTexture GL_TEXTURE_3D, 0
    End With
End Sub

Sub VoxRenderSubVolume(Model As Vox_Volume, ByVal A As Vec3I, ByVal B As Vec3I)
    If Model = VOXEL_SCREEN Then Model = VC->DefaultVol
    If Model < 0 Or Model > UBound(InternalVoxModels) Then Exit Sub
    If A.X > B.X Then Swap A.X, B.X
    If A.Y > B.Y Then Swap A.Y, B.Y
    If A.Z > B.Z Then Swap A.Z, B.Z
    If A.X < 0 Then A.X = 0
    If A.Y < 0 Then A.Y = 0
    If A.Z < 0 Then A.Z = 0
    With InternalVoxModels(Model)
        If B.X >= .W Then B.X = .W - 1
        If B.Y >= .H Then B.Y = .H - 1
        If B.Z >= .D Then B.Z = .D - 1
        Select Case .VolType
        Case Volume_Dynamic
            glBindTexture GL_TEXTURE_3D, .Tex
            .RenderAll A, B + Vec3I(1, 1, 1)
        Case Volume_Static
            glBindTexture GL_TEXTURE_3D, .Tex
            .Render A, B + Vec3I(1, 1, 1)
        Case Volume_Offscreen
            If .Tex = 0 Then glGenTextures(1, @.Tex)
            glBindTexture GL_TEXTURE_3D, .Tex
            glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP
            glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP
            glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP
            
            glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_NEAREST
            glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_NEAREST
            
            If glTexImage3D <> NULL Then
                glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA8, .W, .H, .D, 0, GL_RGBA, GL_UNSIGNED_BYTE, .ClientTex)
                .RenderAll A, B + Vec3I(1, 1, 1)
                glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA8, 0, 0, 0, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL)
            End If 
        End Select
        glBindTexture GL_TEXTURE_3D, 0
    End With
End Sub

Sub VoxScreenTurnRight(Angle As Double)
    VC->Camera.TurnRight Angle
End Sub

Sub VoxScreenTurnDown(Angle As Double)
    VC->Camera.TurnDown Angle
End Sub

Sub VoxScreenTurnCCW(Angle As Double)
    VC->Camera.SpinCounterClockwise -Angle
End Sub

Sub VoxScreenMoveRight(Dist As Double)
    VC->Camera.Location += VC->Camera.LeftVect*Dist
    VC->Camera.MidP += VC->Camera.LeftVect*Dist
End Sub

Sub VoxScreenMoveUp(Dist As Double)
    VC->Camera.Location += VC->Camera.UpVect*Dist
    VC->Camera.MidP += VC->Camera.UpVect*Dist
End Sub

Sub VoxScreenMoveForward(Dist As Double)
    VC->Camera.Location += VC->Camera.ForeVect*Dist
    VC->Camera.MidP += VC->Camera.ForeVect*Dist
End Sub

Sub VoxScreenCenter(V As Vec3I)
    VoxScreenCenter V.X, V.Y, V.Z
End Sub

Sub VoxScreenCenter(X As Double, Y As Double, Z As Double)
    Dim V As Vec3F =  Vec3F(X, Y, Z) - VC->Camera.MidP
    VC->Camera.Location += V
    VC->Camera.MidP += V
End Sub

Sub VoxScreenDistance(D As Double)
    VC->Camera.Location = VC->Camera.MidP - VC->Camera.ForeVect * D
End Sub

Function VoxCursorTest(ByRef V1 As Vec3I, ByRef V2 As Vec3I, PixX As Integer, PixY As Integer, ByRef MaxDist As Double = -1) As Integer
    Dim As Vec3F EyeP, Ray, Temp
    Dim As Double Dist
    Dim As GLdouble MvMat(15)
    
    glGetDoublev GL_MODELVIEW_MATRIX, @MvMat(0)
    Temp.X = MvMat(0)^2 + MvMat(1)^2 + MvMat(2)^2
    Temp.Y = MvMat(4)^2 + MvMat(5)^2 + MvMat(6)^2
    Temp.Z = MvMat(8)^2 + MvMat(9)^2 + MvMat(10)^2
    EyeP.X = (-MvMat(0)*MvMat(12) - MvMat(1)*MvMat(13) - MvMat(2)*MvMat(14))/Temp.X
    EyeP.Y = (-MvMat(4)*MvMat(12) - MvMat(5)*MvMat(13) - MvMat(6)*MvMat(14))/Temp.Y
    EyeP.Z = (-MvMat(8)*MvMat(12) - MvMat(9)*MvMat(13) - MvMat(10)*MvMat(14))/Temp.Z
    
    Ray = DeProject(PixX, PixY) - EyeP
    Function = 0
    With InternalVoxModels(VC->CurVol)
        Dim As Integer X, Y, Z, I, WH = .W*.H
        .Lock
        For X = 0 To .W
            Temp = Ray * ((X - EyeP.X) / Ray.X) + EyeP
            Y = CInt(Int(Temp.Y))
            Z = CInt(Int(Temp.Z))
            If 0 <= Y And Y < .H And 0 <= Z And Z < .D Then
                I = X+.W*(Y+.H*Z)
                If (X=.W OrElse .ClientTex.A[I] = 0) Xor (X=0 OrElse .ClientTex.A[I-1] = 0) Then
                    Dist = Abs(MvMult(MvMat(), Temp))
                    If MaxDist < 0 Or Dist < MaxDist Then
                        MaxDist = Dist
                        V1 = Vec3I(X, Y, Z)
                        V2 = Vec3I(X-1, Y, Z)
                        If X = .W OrElse .ClientTex.A[I] = 0 Then Swap V1, V2
                        Function = -1
                    End If
                End If
            End If
        Next X
        For Y = 0 To .H
            Temp = Ray * ((Y - EyeP.Y) / Ray.Y) + EyeP
            X = CInt(Int(Temp.X))
            Z = CInt(Int(Temp.Z))
            If 0 <= X And X < .W And 0 <= Z And Z < .D Then
                I = X+.W*(Y+.H*Z)
                If (Y=.H OrElse .ClientTex.A[I] = 0) Xor (Y=0 OrElse .ClientTex.A[I-.W] = 0) Then
                    Dist = Abs(MvMult(MvMat(), Temp))
                    If MaxDist < 0 Or Dist < MaxDist Then
                        MaxDist = Dist
                        V1 = Vec3I(X, Y, Z)
                        V2 = Vec3I(X, Y-1, Z)
                        If Y = .H OrElse .ClientTex.A[I] = 0 Then Swap V1, V2
                        Function = -1
                    End If
                End If
            End If
        Next Y
        For Z = 0 To .D
            Temp = Ray * ((Z - EyeP.Z) / Ray.Z) + EyeP
            X = CInt(Int(Temp.X))
            Y = CInt(Int(Temp.Y))
            If 0 <= X And X < .W And 0 <= Y And Y < .H Then
                I = X+.W*(Y+.H*Z)
                If (Z=.D OrElse .ClientTex.A[I] = 0) Xor (Z=0 OrElse .ClientTex.A[I-WH] = 0) Then
                    Dist = Abs(MvMult(MvMat(), Temp))
                    If MaxDist < 0 Or Dist < MaxDist Then
                        MaxDist = Dist
                        V1 = Vec3I(X, Y, Z)
                        V2 = Vec3I(X, Y, Z-1)
                        If Z = .D OrElse .ClientTex.A[I] = 0 Then Swap V1, V2
                        Function = -1
                    End If
                End If
            End If
        Next Z
        .UnLockNoUpdate
    End With
End Function

Function VoxSubCursorTest(ByRef V1 As Vec3I, ByRef V2 As Vec3I, ByVal A As Vec3I, ByVal B As Vec3I, PixX As Integer, PixY As Integer, ByRef MaxDist As Double = -1) As Integer
    Dim As Vec3F EyeP, Ray, Temp
    Dim As Double Dist
    Dim As GLdouble MvMat(15)
    
    glGetDoublev GL_MODELVIEW_MATRIX, @MvMat(0)
    Temp.X = MvMat(0)^2 + MvMat(1)^2 + MvMat(2)^2
    Temp.Y = MvMat(4)^2 + MvMat(5)^2 + MvMat(6)^2
    Temp.Z = MvMat(8)^2 + MvMat(9)^2 + MvMat(10)^2
    EyeP.X = (-MvMat(0)*MvMat(12) - MvMat(1)*MvMat(13) - MvMat(2)*MvMat(14))/Temp.X
    EyeP.Y = (-MvMat(4)*MvMat(12) - MvMat(5)*MvMat(13) - MvMat(6)*MvMat(14))/Temp.Y
    EyeP.Z = (-MvMat(8)*MvMat(12) - MvMat(9)*MvMat(13) - MvMat(10)*MvMat(14))/Temp.Z
    
    Ray = DeProject(PixX, PixY) - EyeP
    Function = 0
    If A.X > B.X Then Swap A.X, B.X
    If A.Y > B.Y Then Swap A.Y, B.Y
    If A.Z > B.Z Then Swap A.Z, B.Z
    If A.X < 0 Then A.X = 0
    If A.Y < 0 Then A.Y = 0
    If A.Z < 0 Then A.Z = 0
    With InternalVoxModels(VC->CurVol)
        If B.X >= .W Then B.X = .W - 1
        If B.Y >= .H Then B.Y = .H - 1
        If B.Z >= .D Then B.Z = .D - 1
        Dim As Integer X, Y, Z, I, WH = .W*.H
        .Lock
        For X = A.X To B.X+1
            Temp = Ray * ((X - EyeP.X) / Ray.X) + EyeP
            Y = CInt(Int(Temp.Y))
            Z = CInt(Int(Temp.Z))
            If A.Y <= Y And Y <= B.Y And A.Z <= Z And Z <= B.Z Then
                I = X+.W*(Y+.H*Z)
                If (X=B.X+1 OrElse .ClientTex.A[I] = 0) Xor (X=A.X OrElse .ClientTex.A[I-1] = 0) Then
                    Dist = Abs(MvMult(MvMat(), Temp))
                    If MaxDist < 0 Or Dist < MaxDist Then
                        MaxDist = Dist
                        V1 = Vec3I(X, Y, Z)
                        V2 = Vec3I(X-1, Y, Z)
                        If X = B.X+1 OrElse .ClientTex.A[I] = 0 Then Swap V1, V2
                        Function = -1
                    End If
                End If
            End If
        Next X
        For Y = A.Y To B.Y+1
            Temp = Ray * ((Y - EyeP.Y) / Ray.Y) + EyeP
            X = CInt(Int(Temp.X))
            Z = CInt(Int(Temp.Z))
            If A.X <= X And X <= B.X And A.Z <= Z And Z <= B.Z Then
                I = X+.W*(Y+.H*Z)
                If (Y=B.Y+1 OrElse .ClientTex.A[I] = 0) Xor (Y=A.Y OrElse .ClientTex.A[I-.W] = 0) Then
                    Dist = Abs(MvMult(MvMat(), Temp))
                    If MaxDist < 0 Or Dist < MaxDist Then
                        MaxDist = Dist
                        V1 = Vec3I(X, Y, Z)
                        V2 = Vec3I(X, Y-1, Z)
                        If Y = B.Y+1 OrElse .ClientTex.A[I] = 0 Then Swap V1, V2
                        Function = -1
                    End If
                End If
            End If
        Next Y
        For Z = A.Z To B.Z+1
            Temp = Ray * ((Z - EyeP.Z) / Ray.Z) + EyeP
            X = CInt(Int(Temp.X))
            Y = CInt(Int(Temp.Y))
            If A.X <= X And X <= B.X And A.Y <= Y And Y <= B.Y Then
                I = X+.W*(Y+.H*Z)
                If (Z=B.Z+1 OrElse .ClientTex.A[I] = 0) Xor (Z=A.Z OrElse .ClientTex.A[I-WH] = 0) Then
                    Dist = Abs(MvMult(MvMat(), Temp))
                    If MaxDist < 0 Or Dist < MaxDist Then
                        MaxDist = Dist
                        V1 = Vec3I(X, Y, Z)
                        V2 = Vec3I(X, Y, Z-1)
                        If Z = B.Z+1 OrElse .ClientTex.A[I] = 0 Then Swap V1, V2
                        Function = -1
                    End If
                End If
            End If
        Next Z
        .UnLockNoUpdate
    End With
End Function

Function VoxPoint(V As Vec3I) As UInteger
    Dim C As UInteger = Any
    With InternalVoxModels(VC->CurVol)
        If OutsideVolume(V) Then Return 0
        .Lock
        C = .ClientTex.A[V.X+.W*(V.Y+.H*V.Z)]
        .UnLockNoUpdate
        Return SwapRB(C)
    End With
End Function

End Extern