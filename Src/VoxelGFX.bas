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

#Define FloorDivide(N, D) (((N)-((N)Mod(D)+(D))Mod(D))\(D))

Type VoxelFontChar
    SrcVec As Vec3I
    SrcWidth As Integer
    DestWidth As Integer
End Type

#Define VA_NO_DESTRUCTOR
VA_MAKE_WRAPPER(VoxelFontChar)
#UnDef VA_NO_DESTRUCTOR

Type VoxelFont
    CharPosn As VarArray_VoxelFontChar
    FontVol As Vox_Volume
    CharHeight As Integer
    CharDepth As Integer
    FirstChar As Integer
    LastChar As Integer
    Extent As Vec3I
End Type

Type VoxelGFXContext
    DefaultVol As Vox_Volume
    CurVol As Vox_Volume
    CurColor As UInteger
    SourceVol As Vox_Volume = -1
    CurFont As Vox_Font = -1
    Camera As Camera3D
    DrawPos As Vec3I
    DrawPos2 As Vec3I
    BlitPerm(2) As UByte = {0, 1, 2}
    BlitReflect As UByte
End Type

#Define VA_NO_DESTRUCTOR
VA_MAKE_WRAPPER(VoxelGFXContext)
#UnDef VA_NO_DESTRUCTOR

Dim Shared InternalVoxModels() As InternalVoxelVolume
Dim Shared VoxFonts() As VoxelFont
Dim Shared VC_MinusOne As VoxelGFXContext
Dim Shared VC As VoxelGFXContext Ptr = @VC_MinusOne
Dim Shared VoxContext As VarArray_VoxelGFXContext

End Namespace
Using InternalVoxelGFX

Extern "C++"

Constructor Vec3I()
    X = 0
    Y = 0
    Z = 0
End Constructor

Constructor Vec3I(V As Const Vec3I)
    X = V.X
    Y = V.Y
    Z = V.Z
End Constructor

Constructor Vec3I(X As Integer, Y As Integer, Z As Integer)
    This.X = X
    This.Y = Y
    This.Z = Z
End Constructor

Operator Vec3I.+= (Rhs As Vec3I)
    X += Rhs.X
    Y += Rhs.Y
    Z += Rhs.Z
End Operator

Operator Vec3I.-= (Rhs As Vec3I)
    X -= Rhs.X
    Y -= Rhs.Y
    Z -= Rhs.Z
End Operator

Operator Vec3I.*= (Rhs As Integer)
    X *= Rhs
    Y *= Rhs
    Z *= Rhs
End Operator

Operator Vec3I.\= (ByVal Rhs As Integer)
    X = FloorDivide(X, Rhs)
    Y = FloorDivide(Y, Rhs)
    Z = FloorDivide(Z, Rhs)
End Operator

Operator Vec3I.Cast () As String
    Return "(" & X & ", " & Y & ", " & Z & ")"
End Operator

Operator -(Rhs As Vec3I) As Vec3I
    Return Type(-Rhs.X, -Rhs.Y, -Rhs.Z)
End Operator

Operator Abs(Rhs As Vec3I) As Double
    Return Sqr(Rhs.X*Rhs.X + Rhs.Y*Rhs.Y + Rhs.Z*Rhs.Z)
End Operator

Operator + (Lhs As Vec3I, Rhs As Vec3I) As Vec3I
    Return Type(Lhs.X + Rhs.X, Lhs.Y + Rhs.Y, Lhs.Z + Rhs.Z)
End Operator

Operator - (Lhs As Vec3I, Rhs As Vec3I) As Vec3I
    Return Type(Lhs.X - Rhs.X, Lhs.Y - Rhs.Y, Lhs.Z - Rhs.Z)
End Operator

'Scalar Products
Operator * (Lhs As Vec3I, Rhs As Integer) As Vec3I
    Return Type(Lhs.X * Rhs, Lhs.Y * Rhs, Lhs.Z * Rhs)
End Operator
Operator * (Lhs As Integer, Rhs As Vec3I) As Vec3I
    Return Type(Lhs * Rhs.X, Lhs * Rhs.Y, Lhs * Rhs.Z)
End Operator
Operator \ (Lhs As Vec3I, Rhs As Integer) As Vec3I
    Return Type(FloorDivide(Lhs.X, Rhs), FloorDivide(Lhs.Y, Rhs), FloorDivide(Lhs.Z, Rhs))
End Operator
'Dot Product
Operator * (Lhs As Vec3I, Rhs As Vec3I) As Integer
    Return Lhs.X*Rhs.X + Lhs.Y*Rhs.Y + Lhs.Z*Rhs.Z
End Operator
Namespace InternalVoxelGFX
'Cross Product
Function Cross (Lhs As Vec3I, Rhs As Vec3I) As Vec3I
    Return Type(Lhs.Y*Rhs.Z - Lhs.Z*Rhs.Y, Lhs.Z*Rhs.X - Lhs.X*Rhs.Z, Lhs.X*Rhs.Y - Lhs.Y*Rhs.X)
End Function
End Namespace

Operator = (Lhs As Vec3I, Rhs As Vec3I) As Integer
    Return (Lhs.X = Rhs.X And Lhs.Y = Rhs.Y And Lhs.Z = Rhs.Z)
End Operator

Operator <> (Lhs As Vec3I, Rhs As Vec3I) As Integer
    Return (Lhs.X <> Rhs.X Or Lhs.Y <> Rhs.Y Or Lhs.Z <> Rhs.Z)
End Operator

Sub VoxInit(GlExtFetch As Any Ptr = NULL, Flags As UInteger = 0)
    #ifdef __FB_WIN32__
        Dim ExtFetch As Function Stdcall(ByRef Proc As Const ZString) As Any Ptr = GlExtFetch
    #else
        Dim ExtFetch As Function (ByRef Proc As Const ZString) As Any Ptr = GlExtFetch
    #endif
    
    If GlExtFetch <> NULL Then
        glTexImage3D = ExtFetch("glTexImage3D")
        glTexSubImage3D = ExtFetch("glTexSubImage3D")
        glGenBuffers = ExtFetch("glGenBuffers")
        glBindBuffer = ExtFetch("glBindBuffer")
        glBufferData = ExtFetch("glBufferData")
    End If
    
    ReDim InternalVoxModels(0)
    ReDim VoxFonts(0)
    
    If Flags And &H000F& Then
        Dim As Integer I = 1 Shl (Flags And &H000F&)
        VoxScreenRes I, I, I
    End If
End Sub

Sub VoxScreenRes(ByVal Size As Vec3I, BackColor As UInteger = 0)
    VoxScreenRes Size.X, Size.Y, Size.Z, BackColor
End Sub

Sub VoxScreenRes(SizeX As Integer, SizeY As Integer, SizeZ As Integer, BackColor As UInteger = 0)
    Var Temp = VC->CurVol
    VC->CurVol = VC->DefaultVol
    glClearColor RGBA_R(BackColor)/255.0, RGBA_G(BackColor)/255.0, RGBA_B(BackColor)/255.0, 1.0f
    
    VoxSizeVolume SizeX, SizeY, SizeZ
    
    Dim Max As Integer = SizeX
    If SizeY > Max Then Max = SizeY
    If SizeZ > Max Then Max = SizeZ
    
    VC->Camera.MidP = Vec3F(SizeX/2.0, SizeY/2.0, SizeZ/2.0)
    VC->Camera.Location = VC->Camera.MidP - (Max+SizeX+SizeY+SizeZ)/2*VC->Camera.ForeVect
    
    VC->CurVol = Temp
End Sub

Function VoxNewVolume(T As VoxVolumeType = Volume_Dynamic) As Vox_Volume
    ReDim Preserve InternalVoxModels(UBound(InternalVoxModels) + 1)
    VC->CurVol = UBound(InternalVoxModels)
    InternalVoxModels(VC->CurVol).VolType = T
    Return UBound(InternalVoxModels)
End Function

Function VoxNewVolume(ByVal Size As Vec3I, T As VoxVolumeType = Volume_Dynamic) As Vox_Volume
    Function = VoxNewVolume(T)
    VoxSizeVolume Size
End Function

Function VoxNewVolume(SizeX As Integer, SizeY As Integer, SizeZ As Integer, T As VoxVolumeType = Volume_Dynamic) As Vox_Volume
    Return VoxNewVolume(Vec3I(SizeX, SizeY, SizeZ), T)
End Function

Sub VoxSizeVolume(ByVal Size As Vec3I)
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
        If .VolType <> Volume_OffScreen Then .Lock: .Unlock 'Force Texture Update
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

Function VoxNewFont (Volume As Vox_Volume, ByVal CharSize As Vec3I, NumChars As Integer, CharPosn As Vec3I Ptr = NULL, CharWidth As Integer Ptr = NULL, DestWidth As Integer Ptr = NULL, FirstChar As Integer = 0, ByVal StartVec As Vec3I = Vec3I(-1,-1,-1), ByVal StopVec As Vec3I = Vec3I(-1,-1,-1), Flags As UInteger = 0) As Vox_Font
    'Sanitize the parameters
    If Volume = VOXEL_SCREEN Then Volume = VC->DefaultVol
    If Volume < 0 Or Volume > UBound(InternalVoxModels) Then Return -1
    If CharSize.Y <= 0 Or CharSize.Z <= 0 Then Return -1
    
    If DestWidth = NULL Then DestWidth = CharWidth 'If unspecified alias to the CharWidth
    If CharWidth = NULL Then
        If CharSize.X <= 0 Then Return -1
       Else
        For I As Integer = 0 To NumChars - 1
            If CharWidth[I] < 0 Then Return -1
            If DestWidth[I] < 0 Then Return -1
        Next I
    End If
    
    If FirstChar < 0 Then FirstChar = 0
    With InternalVoxModels(Volume)
        If StartVec.X < 0 Or StartVec.X >= .W Then StartVec.X = 0
        If StartVec.Y < 0 Or StartVec.Y >= .H Then StartVec.Y = 0
        If StartVec.Z < 0 Or StartVec.Z >= .D Then StartVec.Z = 0
        If StopVec.X < 0 Or StopVec.X >= .W Then StopVec.X = .W - 1
        If StopVec.Y < 0 Or StopVec.Y >= .H Then StopVec.Y = .H - 1
        If StopVec.Z < 0 Or StopVec.Z >= .D Then StopVec.Z = .D - 1
    End With
    If StartVec.X > StopVec.X Then Swap StartVec.X, StopVec.X
    If StartVec.Y > StopVec.Y Then Swap StartVec.Y, StopVec.Y
    If StartVec.Z > StopVec.Z Then Swap StartVec.Z, StopVec.Z
    
    If CharPosn <> NULL Then
        For I As Integer = 0 To NumChars - 1
            If CharWidth = NULL Then
                If CharPosn[I].X + CharSize.X - 1 > StopVec.X Then Return -1
               Else
                If CharPosn[I].X + CharWidth[I] - 1 > StopVec.X Then Return -1
            End If
            If CharPosn[I].Y + CharSize.Y - 1 > StopVec.Y Then Return -1
            If CharPosn[I].Z + CharSize.Z - 1 > StopVec.Z Then Return -1
        Next I
    End If
    
    ' Allocate a new VoxFont
    ReDim Preserve VoxFonts(UBound(VoxFonts) + 1)
    VC->CurFont = UBound(VoxFonts)
    
    With VoxFonts(VC->CurFont)
        .FontVol = Volume
        .CharHeight = CharSize.Y
        .CharDepth = CharSize.Z
        .FirstChar = FirstChar
        
        .LastChar = FirstChar + NumChars - 1
        
        'Allocate an array of char position/widths
        .CharPosn.ReDim_ NumChars - 1
        
        If CharWidth <> NULL Then
            For I As Integer = 0 To NumChars - 1
                .CharPosn.A[I].SrcWidth = CharWidth[I]
                .CharPosn.A[I].DestWidth = DestWidth[I]
            Next I
           Else
            For I As Integer = 0 To NumChars - 1
                .CharPosn.A[I].SrcWidth = CharSize.X
                If DestWidth = NULL Then
                    .CharPosn.A[I].DestWidth = CharSize.X
                    Else
                    .CharPosn.A[I].DestWidth = DestWidth[I]
                End If
            Next I
        End If
        
        If CharPosn <> NULL Then
            For I As Integer = 0 To NumChars - 1
                .CharPosn.A[I].SrcVec = CharPosn[I]
                If CharPosn[I].X + .CharPosn.A[I].SrcWidth - 1 > .Extent.X Then .Extent.X = CharPosn[I].X + .CharPosn.A[I].SrcWidth - 1
                If CharPosn[I].Y + CharSize.Y - 1 > .Extent.Y Then .Extent.Y = CharPosn[I].Y + CharSize.Y - 1
                If CharPosn[I].Z + CharSize.Z - 1 > .Extent.Z Then .Extent.Z = CharPosn[I].Z + CharSize.Z - 1
            Next I
           Else
            .Extent.Y = (StopVec.Y+1 - StartVec.Y)\CharSize.Y
            .Extent.Z = (StopVec.Z+1 - StartVec.Z)\CharSize.Z
            .Extent.Y = StartVec.Y + .Extent.Y * CharSize.Y - 1
            .Extent.Z = StartVec.Z + .Extent.Z * CharSize.Z - 1
            
            Dim As Integer X, Y, Z, I = 0
            For Z = StartVec.Z To .Extent.Z Step CharSize.Z
                For Y = StartVec.Y To .Extent.Y Step CharSize.Y
                    X = StartVec.X
                    Do Until X + .CharPosn.A[I].SrcWidth - 1 > StopVec.X
                        .CharPosn.A[I].SrcVec = Vec3I(X, Y, Z)
                        If Flags And VOXEL_FONT_FLIP_X Then .CharPosn.A[I].SrcVec.X = StopVec.X + StartVec.X - X - .CharPosn.A[I].SrcWidth + 1
                        If Flags And VOXEL_FONT_FLIP_Y Then .CharPosn.A[I].SrcVec.Y = StopVec.Y + StartVec.Y - Y - CharSize.Y + 1
                        If Flags And VOXEL_FONT_FLIP_Z Then .CharPosn.A[I].SrcVec.Z = StopVec.Z + StartVec.Z - Z - CharSize.Z + 1
                        If .CharPosn.A[I].SrcVec.X + .CharPosn.A[I].SrcWidth - 1 > .Extent.X Then .Extent.X = .CharPosn.A[I].SrcVec.X + .CharPosn.A[I].SrcWidth - 1
                        X += .CharPosn.A[I].SrcWidth
                        I += 1
                        If I >= NumChars Then Exit For, For
                    Loop
                Next Y
            Next Z
            If I < NumChars Then
                .CharPosn.ReDim_Preserve_ I - 1
                .LastChar = FirstChar + I - 1
            End If
        End If
    End With
    
    Return UBound(VoxFonts)
End Function

Function VoxNewContext(ScreenVolume As Vox_Volume = VOXEL_SCREEN) As Vox_Context
    If ScreenVolume = VOXEL_SCREEN Then ScreenVolume = VC->DefaultVol
    VoxContext.ReDim_Preserve_ VA_UBound(VoxContext.A) + 1
    
    If ScreenVolume < 0 Or ScreenVolume > UBound(InternalVoxModels) Then ScreenVolume = 0
    VC = @(VoxContext.A[VA_UBound(VoxContext.A)])
    VC->DefaultVol = ScreenVolume
    VC->CurVol = ScreenVolume
    With InternalVoxModels(ScreenVolume)
        Dim Max As Integer = .W
        If .H > Max Then Max = .H
        If .D > Max Then Max = .D
        
        VC->Camera.MidP = Vec3F(.W/2.0, .H/2.0, .D/2.0)
        VC->Camera.Location = VC->Camera.MidP - (Max+.W+.H+.D)/2*VC->Camera.ForeVect
    End With
    Return VA_UBound(VoxContext.A)
End Function

Function VoxLoadFile(ByVal FileName As ZString Ptr, Depth As Integer = 0, T As VoxVolumeType = Volume_OffScreen) As Vox_Volume
    Dim P As Any Ptr
    Function = VoxNewVolume(T)
    
    P = png_load(*FileName, PNG_TARGET_OPENGL)
    If P <> NULL Then
        With InternalVoxModels(VC->CurVol)
            png_dimensions(*FileName, .W, .H)
            
            If Depth <> 0 AndAlso .H Mod Depth <> 0 Then Depth = 0
            If Depth = 0 Then  'Load voXdepth from the file
                Dim As UByte B(FileLen(FileName)-1)
                Dim As Integer F = FreeFile
                Open *FileName For Binary As #F
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

Sub VoxSaveFile (ByVal FileName As ZString Ptr, V As Vox_Volume)
    If V = VOXEL_SCREEN Then V = VC->DefaultVol
    If V < 0 Or V > UBound(InternalVoxModels) Then Exit Sub
    Dim As Integer I, J, L, F = Any
    With InternalVoxModels(V)
        .Lock  'New Style Freebasic Image Buffer
        Dim As UInteger Ptr B = CAllocate(.ClientTex.UBound_ + 9, SizeOf(UInteger))
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
        DeAllocate B: B = NULL
        If P = NULL Then Exit Sub
        
        I = (((P[8] Shl 8 Or P[9]) Shl 8 Or P[10]) Shl 8 Or P[11]) + 20
        
        Dim As UByte voXd(19) = {0, 0, 0, 8, Asc("v"), Asc("o"), Asc("X"), Asc("d"), Asc("e"), Asc("p"), Asc("t"), Asc("h")}
        put_u32(@voXd(12), .D)
        Dim As UInteger crc = Any
	    crc = crc32(0, @voXd(4), 4)
	    crc = crc32(crc, @voXd(8), 8)
	    put_u32(@voXd(16), crc)
	    
	    If FileExists(*FileName) Then Kill *FileName
	    F = FreeFile
        Open *FileName For Binary As #F
            Put #F, , P[0], I
            Put #F, , voXd(0), 20  'Insert voXdepth Chunk
            Put #F, , P[I], L - I
        Close #F
        Delete P: P = NULL
    End With
End Sub

Sub VoxSetContext(C As Vox_Context = -1)
    If C > VA_UBound(VoxContext.A) Or C < -1 Then C = -1
    If C = -1 Then
        VC = @VC_MinusOne
       Else
        VC = @(VoxContext.A[C])
    End If
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

Sub VoxSetFont(Font As Vox_Font)
    If Font <= 0 Or Font > UBound(VoxFonts) Then Exit Sub
    VC->CurFont = Font
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

Sub VSet(ByVal V As Vec3I)
    VC->DrawPos2 = VC->DrawPos
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

Sub VSet(ByVal V As Vec3I, ByVal C As UInteger)
    VC->DrawPos2 = VC->DrawPos
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

#Macro ClipLine(CX)
    If V1.V(CX) < 0 Then
        If V2.V(CX) < 0 Then Exit Sub
        T1 = 2*(Max + ((B.V(CX)*2+1)*Max) \ (2*V.V(CX)))
        V1 = (B*T1 + A*(2*Max-T1) + Vec3I(Max, Max, Max))\(2*Max)
       Else
        If V2.V(CX) < 0 Then
            T2 = 2*(((A.V(CX)*2+1)*Max) \ (2*V.V(CX)))
            V2 = (B*T2 + A*(2*Max-T2) + Vec3I(Max, Max, Max))\(2*Max)
        End If
    End If
    If V1.V(CX) >= .Size(CX) Then
        If V2.V(CX) >= .Size(CX) Then Exit Sub
        T1 = 2*(Max - (((.Size(CX) - B.V(CX))*2-1)*Max - 1) \ (2*V.V(CX)))
        V1 = (B*T1 + A*(2*Max-T1) + Vec3I(Max, Max, Max))\(2*Max)
       Else
        If V2.V(CX) >= .Size(CX) Then
            T2 = 2*((((A.V(CX) - .Size(CX))*2+1)*Max + 1) \ (2*V.V(CX)))
            V2 = (B*T2 + A*(2*Max-T2) + Vec3I(Max, Max, Max))\(2*Max)
        End If
    End If
#EndMacro

Sub VoxLine(ByVal A As Vec3I, ByVal B As Vec3I)
    Dim As Vec3I V = A - B, V1 = A, V2 = B
    Dim Max As Integer = Abs(V.X)
    If Abs(V.Y) > Max Then Max = Abs(V.Y)
    If Abs(V.Z) > Max Then Max = Abs(V.Z)
    
    Dim As Integer T1 = 0, T2 = 2*Max
    VC->DrawPos2 = A
    VC->DrawPos = B
    With InternalVoxModels(VC->CurVol)
        ClipLine(0)
        ClipLine(1)
        ClipLine(2)
        If OutsideVolume(V2) Then Exit Sub
        
        If .LockedCount = 0 And .VolType <> Volume_Offscreen And glTexSubImage3D <> NULL Then
            glBindTexture GL_TEXTURE_3D, .Tex
            For T As Integer = T1 To T2 - 2 Step 2
                V = (B*T + A*(2*Max-T) + Vec3I(Max, Max, Max))\(2*Max)
                .ClientTex.A[V.X+.W*(V.Y+.H*V.Z)] = VC->CurColor
                glTexSubImage3D(GL_TEXTURE_3D, 0, V.X, V.Y, V.Z, 1, 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, @VC->CurColor)
            Next T
            .ClientTex.A[V2.X+.W*(V2.Y+.H*V2.Z)] = VC->CurColor
            glTexSubImage3D(GL_TEXTURE_3D, 0, V2.X, V2.Y, V2.Z, 1, 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, @VC->CurColor)
            glBindTexture GL_TEXTURE_3D, 0
           Else
            .Lock
            For T As Integer = T1 To T2 - 2 Step 2
                V = (B*T + A*(2*Max-T) + Vec3I(Max, Max, Max))\(2*Max)
                .ClientTex.A[V.X+.W*(V.Y+.H*V.Z)] = VC->CurColor
            Next T
            .ClientTex.A[V2.X+.W*(V2.Y+.H*V2.Z)] = VC->CurColor
            .UnLock
        End If
    End With
End Sub

Sub VoxLineTo(ByVal B As Vec3I)
    VoxLine VC->DrawPos, B
End Sub

#Macro LineScan(VA, VB, CX, CY, CZ, I, COMP)
    V = VA - VB
    Max = Abs(V.X)
    If Abs(V.Y) > Max Then Max = Abs(V.Y)
    If Abs(V.Z) > Max Then Max = Abs(V.Z)
    
    If OutsideVolume(VA) OrElse OutsideVolume(VB) Then
        For T = 0 To 2*Max - 2 Step 2
            V = (VA*(2*Max-T) + VB*T + Vec3I(Max, Max, Max))\(2*Max)
            If Edge(V.V(CY) - C.V(CY), I) COMP V.V(CZ) Then
                Edge(V.V(CY) - C.V(CY), I) = V.V(CZ)
                Edge(V.V(CY) - C.V(CY), I+2) = V.V(CX)
            End If
            #IfDef SCAN_PLOT
                If InsideVolume(V) Then .ClientTex.A[V.X+.W*(V.Y+.H*V.Z)] = VC->CurColor
            #EndIf
        Next T
        If Edge(VB.V(CY) - C.V(CY), I) COMP VB.V(CZ) Then
            Edge(VB.V(CY) - C.V(CY), I) = VB.V(CZ)
            Edge(VB.V(CY) - C.V(CY), I+2) = VB.V(CX)
        End If
        #IfDef SCAN_PLOT
            If InsideVolume(VB) Then .ClientTex.A[VB.X+.W*(VB.Y+.H*VB.Z)] = VC->CurColor
        #EndIf
       Else
        For T = 0 To 2*Max - 2 Step 2
            V = (VA*(2*Max-T) + VB*T + Vec3I(Max, Max, Max))\(2*Max)
            If Edge(V.V(CY) - C.V(CY), I) COMP V.V(CZ) Then
                Edge(V.V(CY) - C.V(CY), I) = V.V(CZ)
                Edge(V.V(CY) - C.V(CY), I+2) = V.V(CX)
            End If
            #IfDef SCAN_PLOT
                .ClientTex.A[V.X+.W*(V.Y+.H*V.Z)] = VC->CurColor
            #EndIf
        Next T
        If Edge(VB.V(CY) - C.V(CY), I) COMP VB.V(CZ) Then
            Edge(VB.V(CY) - C.V(CY), I) = VB.V(CZ)
            Edge(VB.V(CY) - C.V(CY), I+2) = VB.V(CX)
        End If
        #IfDef SCAN_PLOT
            .ClientTex.A[VB.X+.W*(VB.Y+.H*VB.Z)] = VC->CurColor
        #EndIf
    End If
#EndMacro

#Macro ScanFill(CX, CY, CZ)
    If MidIsLeft Then
        LineScan(A, C, CX, CY, CZ, 1, >)
        LineScan(B, A, CX, CY, CZ, 0, <)
        LineScan(B, C, CX, CY, CZ, 0, <)
       Else
        LineScan(C, A, CX, CY, CZ, 0, <)
        LineScan(A, B, CX, CY, CZ, 1, >)
        LineScan(C, B, CX, CY, CZ, 1, >)
    End If
    If N.V(CX) <> 0 Then
        If OutsideVolume(A) OrElse OutsideVolume(B) OrElse OutsideVolume(C) Then
            For T = 1 To UBound(Edge)-1 'Triangle needs Clipping
                V.V(CY) = C.V(CY)+T
                If V.V(CY) >= 0 And V.V(CY) < S.V(CY) Then
                    #IfDef SCAN_PLOT 'Inner Triangle Loop
                        U1 = IIf(Edge(T, 0) < 0, 0, Edge(T, 0)+1)
                        U2 = IIf(Edge(T, 1) >= S.V(CZ), S.V(CZ)-1, Edge(T, 1)-1)
                        For U = U1 To U2
                            V.V(CZ) = U
                            V.V(CX) = (2*(PC - V.V(CZ)*N.V(CZ) - V.V(CY)*N.V(CY))+3*N.V(CX))\(2*N.V(CX))-1
                            If V.V(CX) >= 0 And V.V(CX) < S.V(CX) Then .ClientTex.A[V.X+.W*(V.Y+.H*V.Z)] = VC->CurColor
                        Next U
                    #EndIf
                    'Detect and Fill Boundry Holes
                    If Edge(T, 0)+1 >= Edge(T, 1) Then
                        If Abs(Edge(T, 2) - Edge(T, 3)) > 1 Then
                            V.V(CZ) = Edge(T, 0)+1
                            If V.V(CZ) >= 0 And V.V(CZ) < S.V(CZ) Then 
                                V.V(CX) = FloorDivide(Edge(T, 2) + Edge(T, 3), 2)
                                If V.V(CX) >= 0 And V.V(CX) < S.V(CX) Then .ClientTex.A[V.X+.W*(V.Y+.H*V.Z)] = VC->CurColor
                            End If
                        End If
                       Else
                        If Edge(T, 1) >= 0 And Edge(T, 1) < .Size(CZ) Then
                            V.V(CZ) = Edge(T, 1)-1
                            V.V(CX) = (2*(PC - V.V(CZ)*N.V(CZ) - V.V(CY)*N.V(CY))+3*N.V(CX))\(2*N.V(CX))-1
                            If Abs(V.V(CX) - Edge(T, 3)) > 1 Then
                                V.V(CZ) += 1
                                V.V(CX) = (2*(PC - V.V(CZ)*N.V(CZ) - V.V(CY)*N.V(CY))+3*N.V(CX))\(2*N.V(CX))-1
                                If V.V(CX) >= 0 And V.V(CX) < S.V(CX) Then .ClientTex.A[V.X+.W*(V.Y+.H*V.Z)] = VC->CurColor
                            End If
                        End If
                        If Edge(T, 0) >= 0 And Edge(T, 0) < .Size(CZ) Then
                            V.V(CZ) = Edge(T, 0)+1
                            V.V(CX) = (2*(PC - V.V(CZ)*N.V(CZ) - V.V(CY)*N.V(CY))+3*N.V(CX))\(2*N.V(CX))-1
                            If Abs(V.V(CX) - Edge(T, 2)) > 1 Then
                                V.V(CZ) -= 1
                                V.V(CX) = (2*(PC - V.V(CZ)*N.V(CZ) - V.V(CY)*N.V(CY))+3*N.V(CX))\(2*N.V(CX))-1
                                If V.V(CX) >= 0 And V.V(CX) < S.V(CX) Then .ClientTex.A[V.X+.W*(V.Y+.H*V.Z)] = VC->CurColor
                            End If
                        End If
                    End If
                End If
            Next T
           Else 'Triangle needs no Clipping
            For T = 1 To UBound(Edge)-1
                V.V(CY) = C.V(CY)+T
                #IfDef SCAN_PLOT 'Inner Triangle Loop
                    For U = Edge(T, 0)+1 To Edge(T, 1)-1
                       V.V(CZ) = U
                       V.V(CX) = (2*(PC - V.V(CZ)*N.V(CZ) - V.V(CY)*N.V(CY))+N.V(CX))\(2*N.V(CX))
                       .ClientTex.A[V.X+.W*(V.Y+.H*V.Z)] = VC->CurColor
                    Next U
                #EndIf
                'Detect and Fill Boundry Holes
                If Edge(T, 0)+1 >= Edge(T, 1) Then
                    If Abs(Edge(T, 2) - Edge(T, 3)) > 1 Then
                        V.V(CZ) = Edge(T, 0)+1
                        V.V(CX) = FloorDivide(Edge(T, 2) + Edge(T, 3), 2)
                        .ClientTex.A[V.X+.W*(V.Y+.H*V.Z)] = VC->CurColor
                    End If
                   Else
                    V.V(CZ) = Edge(T, 1)-1
                    V.V(CX) = (2*(PC - V.V(CZ)*N.V(CZ) - V.V(CY)*N.V(CY))+N.V(CX))\(2*N.V(CX))
                    If Abs(V.V(CX) - Edge(T, 3)) > 1 Then
                        V.V(CZ) += 1
                        V.V(CX) = (2*(PC - V.V(CZ)*N.V(CZ) - V.V(CY)*N.V(CY))+N.V(CX))\(2*N.V(CX))
                        .ClientTex.A[V.X+.W*(V.Y+.H*V.Z)] = VC->CurColor
                    End If
                    V.V(CZ) = Edge(T, 0)+1
                    V.V(CX) = (2*(PC - V.V(CZ)*N.V(CZ) - V.V(CY)*N.V(CY))+N.V(CX))\(2*N.V(CX))
                    If Abs(V.V(CX) - Edge(T, 2)) > 1 Then
                        V.V(CZ) -= 1
                        V.V(CX) = (2*(PC - V.V(CZ)*N.V(CZ) - V.V(CY)*N.V(CY))+N.V(CX))\(2*N.V(CX))
                        .ClientTex.A[V.X+.W*(V.Y+.H*V.Z)] = VC->CurColor
                    End If
                End If
            Next T
        End If
    End If
#EndMacro

'S.X = tA.X + uB.X + (1-t-u)C.X
'S.Y = tA.Y + uB.Y + (1-t-u)C.Y
'Solve for t, u
'and plug into
'S' = tA' + uB' + (1-t-u)C'
'
'
'S.X = tA.X + uB.X + (1-t-u)C.X
'S.Y = tA.Y + uB.Y + (1-t-u)C.Y
'
'S.X-C.X = t(A.X-C.X) + u(B.X-C.X)
'S.Y-C.Y = t(A.Y-C.Y) + u(B.Y-C.Y)
'Pretend C is the origin (w/o log)
'
'S.X = tA.X + uB.X
'S.Y = tA.Y + uB.Y
'
'S.X*A.Y - S.Y*A.X = u(B.X*A.Y - B.Y*A.X)
'
'u = (S.X*A.Y - S.Y*A.X) / (B.X*A.Y - B.Y*A.X)
't = (S.X*B.Y - S.Y*B.X) / (B.X*A.Y - B.Y*A.X)

Sub VoxTriangle(ByVal A As Vec3I, ByVal B As Vec3I, ByVal C As Vec3I)
    Dim As Vec3I N = Cross(A - B, A - C), V
    VC->DrawPos2 = B
    VC->DrawPos = C
    
    Dim As Integer Max = Abs(N.X), Plane = 0, Max1, Max2, T, U, U1, U2, PC = N*A, MidIsLeft
    If Abs(N.Y) > Max Then Max = Abs(N.Y): Plane = 1
    If Abs(N.Z) > Max Then Plane = 2
    
    Plane = (Plane + 1) Mod 3
    If B.V(Plane) > A.V(Plane) Then Swap A, B
    If C.V(Plane) > A.V(Plane) Then Swap A, C
    If C.V(Plane) > B.V(Plane) Then Swap B, C
    
    Max1 = Abs(A.V(Plane) - B.V(Plane))
    Max2 = Abs(A.V(Plane) - C.V(Plane))
    Plane = (Plane + 1) Mod 3
    If Max2 > 0 Then
        MidIsLeft = B.V(Plane) < FloorDivide(A.V(Plane)*(2*Max2-2*Max1) + C.V(Plane)*2*Max1 + Max2, 2*Max2)
    End If
    Plane = (Plane + 1) Mod 3
    ReDim As Integer Edge(Max2, 3)
    
    With InternalVoxModels(VC->CurVol)
        Dim As Vec3I S = Vec3I(.W, .H, .D)
        For I As Integer = 0 To Max2
            Edge(I, 0) = -&H7FFFFFFF -1
            Edge(I, 1) = &H7FFFFFFF
        Next I
        .Lock
        
        'Perform primary plotting scan
        #Define SCAN_PLOT
        Select Case Plane
        Case 0
            ScanFill(0, 1, 2)
        Case 1
            ScanFill(1, 2, 0)
        Case 2
            ScanFill(2, 0, 1)
        End Select
        
        'Check for 'edge holes' in alternate scanline direction
        Plane = (Plane + 2) Mod 3
        If B.V(Plane) > A.V(Plane) Then Swap A, B
        If C.V(Plane) > A.V(Plane) Then Swap A, C
        If C.V(Plane) > B.V(Plane) Then Swap B, C
        
        Max1 = Abs(A.V(Plane) - B.V(Plane))
        Max2 = Abs(A.V(Plane) - C.V(Plane))
        Plane = (Plane + 2) Mod 3
        If Max2 > 0 Then
            MidIsLeft = B.V(Plane) < FloorDivide(A.V(Plane)*(2*Max2-2*Max1) + C.V(Plane)*2*Max1 + Max2, 2*Max2)
        End If
        Plane = (Plane + 2) Mod 3
        ReDim As Integer Edge(Max2, 3)
        
        For I As Integer = 0 To Max2
            Edge(I, 0) = -&H7FFFFFFF-1
            Edge(I, 1) = &H7FFFFFFF
        Next I
        
        #UnDef SCAN_PLOT
        Select Case Plane
        Case 0
            ScanFill(0, 2, 1)
        Case 1
            ScanFill(1, 0, 2)
        Case 2
            ScanFill(2, 1, 0)
        End Select
        
        .UnLock
    End With
End Sub

Sub VoxTriangleTo(ByVal B As Vec3I, ByVal C As Vec3I)
    VoxTriangle VC->DrawPos, B, C
End Sub

Sub VoxTriangleFanTo(ByVal C As Vec3I)
    VoxTriangle VC->DrawPos, VC->DrawPos2, C
End Sub

Sub VoxTriangleStripTo(ByVal C As Vec3I)
    VoxTriangle VC->DrawPos2, VC->DrawPos, C
End Sub

#Macro BlitLoop(VOXEL)
    Scope
        Dim As Integer Xd, Yd, Zd, I
        Dim As Integer J, K
        Dim As Vec3I S
        
        If 0 = VC->BlitPerm(0) Then K = 1
        If 1 = VC->BlitPerm(0) Then K = SrcVol->W
        If 2 = VC->BlitPerm(0) Then K = SrcVol->W * SrcVol->H
        If VC->BlitReflect And 1 Then K = -K
        
        S.V(VC->BlitPerm(2)) = SrcV.V(VC->BlitPerm(2))
        If VC->BlitReflect And 4 Then S.V(VC->BlitPerm(2)) += Size.V(VC->BlitPerm(2))-1
        For Zd = DestV.Z To DestV.Z + Size.V(VC->BlitPerm(2))-1
            S.V(VC->BlitPerm(1)) = SrcV.V(VC->BlitPerm(1))
            If VC->BlitReflect And 2 Then S.V(VC->BlitPerm(1)) += Size.V(VC->BlitPerm(1))-1
            For Yd = DestV.Y To DestV.Y + Size.V(VC->BlitPerm(1))-1
                I = DestV.X + .W*(Yd + .H*Zd)
                S.V(VC->BlitPerm(0)) = SrcV.V(VC->BlitPerm(0))
                If VC->BlitReflect And 1 Then S.V(VC->BlitPerm(0)) += Size.V(VC->BlitPerm(0))-1
                J = S.X + SrcVol->W*(S.Y + SrcVol->H*S.Z)
                For Xd = DestV.X To DestV.X + Size.V(VC->BlitPerm(0))-1
                    .ClientTex.A[I] = (VOXEL)
                    J += K
                    I += 1
                Next Xd
                S.V(VC->BlitPerm(1)) += IIf(VC->BlitReflect And 2, -1, 1)
            Next Yd
            S.V(VC->BlitPerm(2)) += IIf(VC->BlitReflect And 4, -1, 1)
        Next Zd
    End Scope
#EndMacro

Sub VoxBlit(ByVal DestV As Vec3I, ByVal SrcV As Vec3I, ByVal Size As Vec3I)
    If VC->CurVol = VC->SourceVol Or VC->SourceVol = -1 Then Exit Sub
    Dim As InternalVoxelVolume Ptr SrcVol = @InternalVoxModels(VC->SourceVol)
    
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
    
    'Clip against the source
    If SrcV.X + Size.X > SrcVol->W Then
        If VC->BlitReflect And 1 Then DestV.V(IP(0)) += Size.X - SrcVol->W + SrcV.X
        Size.X = SrcVol->W - SrcV.X
    End If
    If SrcV.Y + Size.Y > SrcVol->H Then
        If VC->BlitReflect And 2 Then DestV.V(IP(1)) += Size.Y - SrcVol->H + SrcV.Y
        Size.Y = SrcVol->H - SrcV.Y
    End If
    If SrcV.Z + Size.Z > SrcVol->D Then
        If VC->BlitReflect And 4 Then DestV.V(IP(2)) += Size.Z - SrcVol->D + SrcV.Z
        Size.Z = SrcVol->D - SrcV.Z
    End If
    
    If SrcV.X < 0 Then
        Size.X += SrcV.X
        If Not VC->BlitReflect And 1 Then DestV.V(IP(0)) -= SrcV.X
        SrcV.X = 0
    End If
    If SrcV.Y < 0 Then
        Size.Y += SrcV.Y
        If Not VC->BlitReflect And 2 Then DestV.V(IP(1)) -= SrcV.Y
        SrcV.Y = 0
    End If
    If SrcV.Z < 0 Then
        Size.Z += SrcV.Z
        If Not VC->BlitReflect And 4 Then DestV.V(IP(2)) -= SrcV.Z
        SrcV.Z = 0
    End If
    
    With InternalVoxModels(VC->CurVol)
        'Clip against the destination
        If DestV.X + Size.V(VC->BlitPerm(0)) > .W Then
            If VC->BlitReflect And 1 Then SrcV.V(VC->BlitPerm(0)) += Size.V(VC->BlitPerm(0)) - .W + DestV.X
            Size.V(VC->BlitPerm(0)) = .W - DestV.X
        End If
        If DestV.Y + Size.V(VC->BlitPerm(1)) > .H Then
            If VC->BlitReflect And 2 Then SrcV.V(VC->BlitPerm(1)) += Size.V(VC->BlitPerm(1)) - .H + DestV.Y
            Size.V(VC->BlitPerm(1)) = .H - DestV.Y
        End If
        If DestV.Z + Size.V(VC->BlitPerm(2)) > .D Then
            If VC->BlitReflect And 4 Then SrcV.V(VC->BlitPerm(2)) += Size.V(VC->BlitPerm(2)) - .D + DestV.Z
            Size.V(VC->BlitPerm(2)) = .D - DestV.Z
        End If
        
        If DestV.X < 0 Then
            Size.V(VC->BlitPerm(0)) += DestV.X
            If Not VC->BlitReflect And 1 Then SrcV.V(VC->BlitPerm(0)) -= DestV.X
            DestV.X = 0
        End If
        If DestV.Y < 0 Then
            Size.V(VC->BlitPerm(1)) += DestV.Y
            If Not VC->BlitReflect And 2 Then SrcV.V(VC->BlitPerm(1)) -= DestV.Y
            DestV.Y = 0
        End If
        If DestV.Z < 0 Then
            Size.V(VC->BlitPerm(2)) += DestV.Z
            If Not VC->BlitReflect And 4 Then SrcV.V(VC->BlitPerm(2)) -= DestV.Z
            DestV.Z = 0
        End If
        
        SrcVol->Lock
        .Lock
        
        BlitLoop(SrcVol->ClientTex.A[J])
        
        .UnLock
        SrcVol->UnLockNoUpdate
    End With
End Sub

#Macro BlitTextMacro()
    If VC->CurFont = -1 Then Exit Sub
    Dim As VoxelFont Ptr Font = @VoxFonts(VC->CurFont)
    If VC->CurVol = Font->FontVol Then Exit Sub
    
    If Length < 0 Then 'Determine length if unspecified
        Length = 0
        Do Until Text[Length] = 0
            Length += 1
        Loop
    End If
    
    Dim As Integer I, J, K, TextWidth = 0
    For I = 0 To Length-1
        J = Text[I]
        If J >= Font->FirstChar And J <= Font->LastChar Then
            J -= Font->FirstChar
            TextWidth += Font->CharPosn.A[J].DestWidth
        End If
    Next I
    
    Dim IP(2) As Byte = Any 'Inverse Permutation
    IP(VC->BlitPerm(0)) = 0
    IP(VC->BlitPerm(1)) = 1
    IP(VC->BlitPerm(2)) = 2
    
    Dim As InternalVoxelVolume Ptr SrcVol = @InternalVoxModels(Font->FontVol)
    Dim As Vec3I SrcVec, Size = Vec3I(TextWidth, Font->CharHeight, Font->CharDepth)
    
    'Verify the Volume is still large enough
    If SrcVol->W < Font->Extent.X Then Exit Sub
    If SrcVol->H < Font->Extent.Y Then Exit Sub
    If SrcVol->D < Font->Extent.Z Then Exit Sub
    
    'Which direction the characters are printed will depend on the reflection
    K = 1
    If VC->BlitReflect And (2^IP(0)) Then
        DestVec.V(IP(0)) += TextWidth
        K = -1
    End If
    
    With InternalVoxModels(VC->CurVol)
        'Clip against the destination components corrisponding to source Y and Z
        If DestVec.V(IP(1)) + Size.Y > .Size(IP(1)) Then
            If VC->BlitReflect And 1 Shl IP(1) Then SrcVec.Y += Size.Y - .Size(IP(1)) + DestVec.V(IP(1))
            Size.Y = .Size(IP(1)) - DestVec.V(IP(1))
        End If
        If DestVec.V(IP(2)) + Size.Z > .Size(IP(2)) Then
            If VC->BlitReflect And 1 Shl IP(2) Then SrcVec.Z += Size.Z - .Size(IP(2)) + DestVec.V(IP(2))
            Size.Z = .Size(IP(2)) - DestVec.V(IP(2))
        End If
        
        If DestVec.V(IP(1)) < 0 Then
            Size.Y += DestVec.V(IP(1))
            If Not VC->BlitReflect And 1 Shl IP(1) Then SrcVec.Y -= DestVec.V(IP(1))
            DestVec.V(IP(1)) = 0
        End If
        If DestVec.V(IP(2)) < 0 Then
            Size.Z += DestVec.V(IP(2))
            If Not VC->BlitReflect And 1 Shl IP(2) Then SrcVec.Z -= DestVec.V(IP(2))
            DestVec.V(IP(2)) = 0
        End If
        
        Dim As Vec3I SrcV = Any, DestV = DestVec
        
        SrcVol->Lock
        .Lock
        
        For I = 0 To Length-1 'Iterate all the characters
            J = Text[I]
            If J >= Font->FirstChar And J <= Font->LastChar Then
                J -= Font->FirstChar
                
                If K = -1 Then DestVec.V(IP(0)) -= Font->CharPosn.A[J].DestWidth
                
                SrcV = Font->CharPosn.A[J].SrcVec: DestV.V(IP(0)) = DestVec.V(IP(0)): Size.X = Font->CharPosn.A[J].SrcWidth
                SrcV.Y += SrcVec.Y
                SrcV.Z += SrcVec.Z
                
                'Clip via components corrisponding to Source X
                If DestV.V(IP(0)) + Size.X > .Size(IP(0)) Then
                    If VC->BlitReflect And 1 Shl IP(0) Then SrcV.X += Size.X - .Size(IP(0)) + DestV.V(IP(0))
                    Size.X = .Size(IP(0)) - DestV.V(IP(0))
                End If
                If DestV.V(IP(0)) < 0 Then
                    Size.X += DestV.V(IP(0))
                    If Not VC->BlitReflect And 1 Shl IP(0) Then SrcV.X -= DestV.V(IP(0))
                    DestV.V(IP(0)) = 0
                End If
                
                'Blit the Character
                BlitLoop(SrcVol->ClientTex.A[J] And VC->CurColor)
                
                If K = 1 Then DestVec.V(IP(0)) += Font->CharPosn.A[J].DestWidth
            End If
        Next I
        .UnLock
        SrcVol->UnLockNoUpdate
    End With
#EndMacro

Sub VoxBlitText(ByVal DestVec As Vec3I, ByVal Text As ZString Ptr, Length As Integer = -1)
    BlitTextMacro()
End Sub

Sub VoxBlitText(ByVal DestVec As Vec3I, ByVal Text As WString Ptr, Length As Integer = -1)
    BlitTextMacro()
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
    If ScreenW <> 0 And ScreenH <> 0 Then AspectProjectionView ScreenW, ScreenH
    If (Flags And VOXEL_NOMODELVIEW) = 0 Then VC->Camera.SetCamera
    If (Flags And VOXEL_NOLIGHT) = 0 Then SetUpLights
    
    If (Flags And VOXEL_NOGLSTATE) = 0 Then
        glCullFace GL_BACK
        glFrontFace GL_CCW
        glEnable GL_CULL_FACE
        glEnable GL_DEPTH_TEST
        glTexEnvf GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE
        glAlphaFunc GL_GREATER, 0.0
        glEnable GL_ALPHA_TEST
        
        glColor4ub 255, 255, 255, 255
        glEnable GL_TEXTURE_3D
    End If
End Sub

#Macro RenderVolumeMacro(VOLUME, PARAMS)
    Select Case VOLUME.VolType
    Case Volume_Dynamic
        glBindTexture GL_TEXTURE_3D, (VOLUME.Tex)
        VOLUME.RenderAll PARAMS
    Case Volume_Static
        glBindTexture GL_TEXTURE_3D, (VOLUME.Tex)
        VOLUME.Render PARAMS
    Case Volume_Offscreen
        If VOLUME.Tex = 0 Then glGenTextures(1, @(VOLUME.Tex))
        glBindTexture GL_TEXTURE_3D, (VOLUME.Tex)
        glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP
        glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP
        glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP
        
        glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_NEAREST
        glTexParameteri GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_NEAREST
        
        If glTexImage3D <> NULL Then
            glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA8, VOLUME.W, VOLUME.H, VOLUME.D, 0, GL_RGBA, GL_UNSIGNED_BYTE, VOLUME.ClientTex)
            VOLUME.RenderAll PARAMS
            glTexImage3D(GL_TEXTURE_3D, 0, GL_RGBA8, 0, 0, 0, 0, GL_RGBA, GL_UNSIGNED_BYTE, NULL)
        End If 
    End Select
#EndMacro

Sub VoxRenderVolume(Model As Vox_Volume)
    If Model = VOXEL_SCREEN Then Model = VC->DefaultVol
    If Model < 0 Or Model > UBound(InternalVoxModels) Then Exit Sub
    With InternalVoxModels(Model)
        RenderVolumeMacro(,)
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
        RenderVolumeMacro(, (A, B + Vec3I(1, 1, 1)))
        glBindTexture GL_TEXTURE_3D, 0
    End With
End Sub

#Macro RenderTextMacro()
    If VC->CurFont = -1 Then Exit Sub
    If Length < 0 Then
        Length = 0
        Do Until Text[Length] = 0
            Length += 1
        Loop
    End If
    
    With VoxFonts(VC->CurFont)
        Dim As InternalVoxelVolume Ptr SrcVol = @InternalVoxModels(.FontVol)
        Dim As Integer I = Any, J = Any, K = 0
        Dim As Vec3I SrcVec = Any
        
        'Verify the Volume is still large enough
        If SrcVol->W < .Extent.X Then Exit Sub
        If SrcVol->H < .Extent.Y Then Exit Sub
        If SrcVol->D < .Extent.Z Then Exit Sub
        
        For I = 0 To Length-1
            J = Text[I]
            
            If J >= .FirstChar And J <= .LastChar Then
                J -= .FirstChar
                
                SrcVec = .CharPosn.A[J].SrcVec
                glTranslated K-SrcVec.X, -SrcVec.Y, -SrcVec.Z
                
                RenderVolumeMacro((*SrcVol), (SrcVec, SrcVec+Vec3I(.CharPosn.A[J].SrcWidth, .CharHeight, .CharDepth)))
                
                glTranslated SrcVec.X-K, SrcVec.Y, SrcVec.Z
            End If
            K += .CharPosn.A[J].DestWidth
        Next I
        glBindTexture GL_TEXTURE_3D, 0
    End With
#EndMacro

Sub VoxRenderText(ByVal Text As ZString Ptr, Length As Integer = -1)
    RenderTextMacro()
End Sub

Sub VoxRenderText(ByVal Text As WString Ptr, Length As Integer = -1)
    RenderTextMacro()
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

Sub VoxScreenCenter(ByVal V As Vec3I)
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

Sub VoxGetScreenCamera(ByRef L As Vec3I, ByRef U As Vec3I, ByRef F As Vec3I)
    With VC->Camera
        L = Vec3I(.LeftVect.X*&H10000, .LeftVect.Y*&H10000, .LeftVect.Z*&H10000)
        U = Vec3I(.UpVect.X*&H10000, .UpVect.Y*&H10000, .UpVect.Z*&H10000)
        F = Vec3I(.ForeVect.X*&H10000, .ForeVect.Y*&H10000, .ForeVect.Z*&H10000)
    End With
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

Function VoxWallTest(ByRef VX As Double, ByRef VY As Double, ByRef VZ As Double, PlaneAxis As UInteger, PixX As Integer, PixY As Integer, ByRef MaxDist As Double = -1) As Integer
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
    Select Case PlaneAxis
    Case VOXEL_AXIS_X
        For X As Double = VX To VX + 1
            Temp = Ray * ((X - EyeP.X) / Ray.X) + EyeP
            Dist = Abs(MvMult(MvMat(), Temp))
            If MaxDist < 0 Or Dist < MaxDist Then
                MaxDist = Dist
                VX = X
                VY = Temp.Y
                VZ = Temp.Z
                Function = -1
            End If
        Next X
    Case VOXEL_AXIS_Y
        For Y As Double = VY To VY + 1
            Temp = Ray * ((Y - EyeP.Y) / Ray.Y) + EyeP
            Dist = Abs(MvMult(MvMat(), Temp))
            If MaxDist < 0 Or Dist < MaxDist Then
                MaxDist = Dist
                VX = Temp.X
                VY = Y
                VZ = Temp.Z
                Function = -1
            End If
        Next Y
    Case VOXEL_AXIS_Z
        For Z As Double = VZ To VZ + 1
            Temp = Ray * ((Z - EyeP.Z) / Ray.Z) + EyeP
            Dist = Abs(MvMult(MvMat(), Temp))
            If MaxDist < 0 Or Dist < MaxDist Then
                MaxDist = Dist
                VX = Temp.X
                VY = Temp.Y
                VZ = Z
                Function = -1
            End If
        Next Z
    End Select
End Function

Function VoxPoint(ByVal V As Vec3I) As UInteger
    Dim C As UInteger = Any
    With InternalVoxModels(VC->CurVol)
        If OutsideVolume(V) Then Return 0
        .Lock
        C = .ClientTex.A[V.X+.W*(V.Y+.H*V.Z)]
        .UnLockNoUpdate
        Return SwapRB(C)
    End With
End Function

Function VoxStep (ByVal Vec As Vec3I) As Vec3I
    Return VC->DrawPos + Vec
End Function

Function VoxStep (X As Integer, Y As Integer, Z As Integer) As Vec3I
    Return VC->DrawPos + Vec3I(X, Y, Z)
End Function

End Extern