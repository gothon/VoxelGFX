'/////////////////////////////////////
'|| VoxelGFX - Voxel Graphics Library
'||   Copyright (C) 2012 Alex Thomson
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

#Inclib "VoxelGFX"
#Pragma Once

#Define GetGLProcAddressCast(X) Cast(Function (ByRef Proc As Const ZString) As Any Ptr, X)

#Define VOXEL_SCREEN32 &H0005&
#Define VOXEL_SCREEN64 &H0006&
#Define VOXEL_SCREEN128 &H0007&
#Define VOXEL_SCREEN256 &H0008&
#Define VOXEL_SCREEN512 &H0009&

#Define VOXEL_NOCLEAR &H0001&
#Define VOXEL_NOLIGHT &H0002&
#Define VOXEL_NOMODELVIEW &H0004&
#Define VOXEL_NOGLSTATE &H0008&

#Define VOXEL_VIEWPORT_ONLY &H000F&
#Define VOXEL_CLEAR (VOXEL_VIEWPORT_ONLY XOr VOXEL_NOCLEAR)
#Define VOXEL_LIGHT (VOXEL_VIEWPORT_ONLY XOr VOXEL_NOLIGHT)
#Define VOXEL_MODELVIEW (VOXEL_VIEWPORT_ONLY XOr VOXEL_NOMODELVIEW)
#Define VOXEL_GLSTATE (VOXEL_VIEWPORT_ONLY XOr VOXEL_NOGLSTATE)

#Define VOXEL_AXIS_X &H0001&
#Define VOXEL_AXIS_Y &H0002&
#Define VOXEL_AXIS_Z &H0003&

#Define VOXEL_FONT_FLIP_X &H0001&
#Define VOXEL_FONT_FLIP_Y &H0002&
#Define VOXEL_FONT_FLIP_Z &H0004&

#Define VOXEL_SCREEN Cast(Vox_Volume, -1)

Extern "C++"

Enum VoxVolumeType
    Volume_Dynamic = 0
    Volume_Static = 1
    Volume_OffScreen = 2
    'Volume_Segmented = 3
    'Volume_Compressed = 4
End Enum

Type Vox_Volume As Long
Type Vox_Context As Long
Type Vox_Font As Long

' Integral 3D Vector
Type Vec3I
    Union
        Type
            As Long X, Y, Z
        End Type
        V(2) As Long
    End Union
    Declare Constructor()
    Declare Constructor(V As Const Vec3I)
    Declare Constructor(X As Long, Y As Long, Z As Long)
    
    Declare Operator += (Rhs As Vec3I)
    Declare Operator -= (Rhs As Vec3I)
    Declare Operator *= (Rhs As Long)
    Declare Operator \= (Rhs As Long)
    
    Declare Operator Cast () As String
End Type

Declare Operator -(Rhs As Vec3I) As Vec3I
Declare Operator Abs (Rhs As Vec3I) As Double

Declare Operator + (Lhs As Vec3I, Rhs As Vec3I) As Vec3I
Declare Operator - (Lhs As Vec3I, Rhs As Vec3I) As Vec3I

'Sclar Products
Declare Operator * (Lhs As Vec3I, Rhs As Long) As Vec3I
Declare Operator * (Lhs As Long, Rhs As Vec3I) As Vec3I
Declare Operator \ (Lhs As Vec3I, Rhs As Long) As Vec3I
'Dot Product
Declare Operator * (Lhs As Vec3I, Rhs As Vec3I) As Long

Declare Operator = (Lhs As Vec3I, Rhs As Vec3I) As Long
Declare Operator <> (Lhs As Vec3I, Rhs As Vec3I) As Long

'Set Up
Declare Sub VoxInit (GlExtFetch As Any Ptr = 0, Flags As ULong = 0)
Declare Sub VoxScreenRes Overload (ByVal Size As Vec3I, BackColor As ULong = 0)
Declare Sub VoxScreenRes (SizeX As Long, SizeY As Long, SizeZ As Long, BackColor As ULong = 0)
Declare Function VoxNewVolume Overload (T As VoxVolumeType = Volume_Dynamic) As Vox_Volume
Declare Function VoxNewVolume (ByVal Size As Vec3I, T As VoxVolumeType = Volume_Dynamic) As Vox_Volume
Declare Function VoxNewVolume (SizeX As Long, SizeY As Long, SizeZ As Long, T As VoxVolumeType = Volume_Dynamic) As Vox_Volume
Declare Sub VoxSizeVolume Overload (ByVal Size As Vec3I)
Declare Sub VoxSizeVolume (SizeX As Long, SizeY As Long, SizeZ As Long)
Declare Function VoxGetVolumeSize (Vol As Vox_Volume = -2) As Vec3I
Declare Sub VoxReloadVolumes
Declare Function VoxNewFont (Volume As Vox_Volume, ByVal CharSize As Vec3I, NumChars As Long, CharPosn As Vec3I Ptr = 0, CharWidth As Long Ptr = 0, DestWidth As Long Ptr = 0, FirstChar As Long = 0, ByVal StartVec As Vec3I = Vec3I(-1,-1,-1), ByVal StopVec As Vec3I = Vec3I(-1,-1,-1), Flags As ULong = 0) As Vox_Font
Declare Function VoxNewContext(ScreenVolume As Vox_Volume = 0) As Vox_Context

'File Save/Load
Declare Function VoxLoadFile (ByVal FileName As ZString Ptr, Depth As Long = 0, T As VoxVolumeType = Volume_OffScreen) As Vox_Volume
Declare Sub VoxSaveFile (ByVal FileName As ZString Ptr, Volume As Vox_Volume)
'Declare Function VoxLoadFileMem (ByVal Buffer As Any Ptr, ByVal BufferLen As Long, Depth As Long = 0, T As VoxVolumeType = Volume_OffScreen) As Vox_Volume
'Declare Function VoxSaveFileMem (ByRef BufferLen As Long, Volume As Vox_Volume) As Any Ptr
'Declare Sub VoxSaveSubVolumeFile(ByVal FileName As ZString Ptr, Volume As Vox_Volume, ByVal A As Vec3I, ByVal B As Vec3I)

'Drawing State
Declare Sub VoxSetContext(C As Vox_Context = -1)
Declare Sub VoxSetVolumeType(T As VoxVolumeType)
Declare Sub VoxSetColor(C As ULong)
Declare Sub VoxSetVolume(Volume As Vox_Volume = VOXEL_SCREEN)
Declare Sub VoxSetSource(Volume As Vox_Volume)
Declare Sub VoxSetFont(Font As Vox_Font)
'Declare Sub VoxEdgeSelection(E As Long)
Declare Sub VoxSetBlitDefault
Declare Sub VoxBlitRightRotate(Axis As ULong, ByVal Amount As Long = 1)
Declare Sub VoxBlitReflect(Axis As ULong)
'Declare Sub VoxSetBlitMode(Mode As ULong)

'Drawing
Declare Sub VoxVolumeLock()
Declare Function VoxVolumePtr() As ULong Ptr
Declare Sub VoxVolumeUnlock()
Declare Sub VoxCls()
Declare Sub VSet Overload (ByVal Voxel As Vec3I)
Declare Sub VSet(ByVal Voxel As Vec3I, Col As ULong)
Declare Sub VoxLine(ByVal A As Vec3I, ByVal B As Vec3I)
Declare Sub VoxLineTo(ByVal B As Vec3I)
Declare Sub VoxTriangle(ByVal A As Vec3I, ByVal B As Vec3I, ByVal C As Vec3I)
Declare Sub VoxTriangleTo(ByVal B As Vec3I, ByVal C As Vec3I)
Declare Sub VoxTriangleFanTo(ByVal C As Vec3I)
Declare Sub VoxTriangleStripTo(ByVal C As Vec3I)
'Declare Sub VoxRectSolid(ByVal A As Vec3I, ByVal B As Vec3I)
'Declare Sub VoxSrcCoords(ByVal A As Vec3I, ByVal B As Vec3I, ByVal C As Vec3I)
'Declare Sub VoxSrcCoords(ByVal A As Vec3I, ByVal B As Vec3I)
'Declare Sub VoxSrcCoords(ByVal A As Vec3I)
'Declare Sub VoxSrcCoordsTo(ByVal B As Vec3I, ByVal C As Vec3I)
'Declare Sub VoxSrcCoordsTo(ByVal C As Vec3I)
Declare Sub VoxBlit(ByVal DestV As Vec3I, ByVal SrcV As Vec3I, ByVal Size As Vec3I)
Declare Sub VoxBlitText Overload (ByVal DestVec As Vec3I, ByVal Text As ZString Ptr, Length As Long = -1)
Declare Sub VoxBlitText(ByVal DestVec As Vec3I, ByVal Text As WString Ptr, Length As Long = -1)

'Rendering
Declare Sub VoxRender(ScreenW As Long, ScreenH As Long, Flags As ULong = 0)
Declare Sub VoxGlRenderState(ScreenW As Long = 0, ScreenH As Long = 0, Flags As ULong = 0)
Declare Sub VoxRenderVolume(Volume As Vox_Volume)
Declare Sub VoxRenderSubVolume(Volume As Vox_Volume, ByVal A As Vec3I, ByVal B As Vec3I)
Declare Sub VoxRenderText Overload (ByVal Text As ZString Ptr, Length As Long = -1)
Declare Sub VoxRenderText(ByVal Text As WString Ptr, Length As Long = -1)

'Perspective Control
Declare Sub VoxScreenTurnRight(Angle As Double)
Declare Sub VoxScreenTurnDown(Angle As Double)
Declare Sub VoxScreenTurnCCW(Angle As Double)
Declare Sub VoxScreenMoveRight(Dist As Double)
Declare Sub VoxScreenMoveUp(Dist As Double)
Declare Sub VoxScreenMoveForward(Dist As Double)
Declare Sub VoxScreenCenter Overload (ByVal V As Vec3I)
Declare Sub VoxScreenCenter(X As Double, Y As Double, Z As Double)
Declare Sub VoxScreenDistance(Dist As Double)

'Reading
Declare Sub VoxGetScreenCamera(ByRef L As Vec3I, ByRef U As Vec3I, ByRef F As Vec3I)
Declare Function VoxCursorTest(ByRef V1 As Vec3I, ByRef V2 As Vec3I, PixX As Long, PixY As Long, ByRef MaxDist As Double = -1) As Long
Declare Function VoxSubCursorTest(ByRef V1 As Vec3I, ByRef V2 As Vec3I, ByVal A As Vec3I, ByVal B As Vec3I, PixX As Long, PixY As Long, ByRef MaxDist As Double = -1) As Long
Declare Function VoxWallTest(ByRef VX As Double, ByRef VY As Double, ByRef VZ As Double, PlaneAxis As ULong, PixX As Long, PixY As Long, ByRef MaxDist As Double = -1) As Long
Declare Function VoxPoint(ByVal Voxel As Vec3I) As ULong
Declare Function VoxStep Overload (ByVal Vec As Vec3I) As Vec3I
Declare Function VoxStep (X As Long, Y As Long, Z As Long) As Vec3I
'Declare Sub VoxGetCubeFaces(ByRef Verts() As Single, ByRef Quads() As Long)

End Extern