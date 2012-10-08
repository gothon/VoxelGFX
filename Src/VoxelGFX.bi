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

#Define VOXEL_AXIS_X &H0001&
#Define VOXEL_AXIS_Y &H0002&
#Define VOXEL_AXIS_Z &H0003&

#Define VOXEL_SCREEN Cast(Vox_Volume, -1)

Extern "C++"

Enum VoxVolumeType
    Volume_Dynamic = 0
    Volume_Static = 1
    Volume_OffScreen = 2
    'Volume_Segmented = 3
    'Volume_Compressed = 4
End Enum

Type Vox_Volume As Integer
Type Vox_Context As Integer

' Integral 3D Vector
Type Vec3I
    Union
        Type
            As Integer X, Y, Z
        End Type
        V(2) As Integer
    End Union
    Declare Constructor()
    Declare Constructor(V As Const Vec3I)
    Declare Constructor(X As Integer, Y As Integer, Z As Integer)
    
    Declare Operator += (Rhs As Vec3I)
    Declare Operator -= (Rhs As Vec3I)
    Declare Operator *= (Rhs As Integer)
    Declare Operator \= (Rhs As Integer)
    
    Declare Operator Cast () As String
End Type

Declare Operator -(Rhs As Vec3I) As Vec3I
Declare Operator Abs (Rhs As Vec3I) As Double

Declare Operator + (Lhs As Vec3I, Rhs As Vec3I) As Vec3I
Declare Operator - (Lhs As Vec3I, Rhs As Vec3I) As Vec3I

'Sclar Products
Declare Operator * (Lhs As Vec3I, Rhs As Integer) As Vec3I
Declare Operator * (Lhs As Integer, Rhs As Vec3I) As Vec3I
Declare Operator \ (Lhs As Vec3I, Rhs As Integer) As Vec3I
'Dot Product
Declare Operator * (Lhs As Vec3I, Rhs As Vec3I) As Integer

Declare Operator = (Lhs As Vec3I, Rhs As Vec3I) As Integer
Declare Operator <> (Lhs As Vec3I, Rhs As Vec3I) As Integer

'Set Up
Declare Sub VoxInit (GlExtFetch As Any Ptr = 0, Flags As UInteger = 0)
Declare Sub VoxScreenRes Overload (ByVal Size As Vec3I, BackColor As UInteger = 0)
Declare Sub VoxScreenRes (SizeX As Integer, SizeY As Integer, SizeZ As Integer, BackColor As UInteger = 0)
Declare Function VoxNewVolume Overload (T As VoxVolumeType = Volume_Dynamic) As Vox_Volume
Declare Function VoxNewVolume (ByVal Size As Vec3I, T As VoxVolumeType = Volume_Dynamic) As Vox_Volume
Declare Function VoxNewVolume (SizeX As Integer, SizeY As Integer, SizeZ As Integer, T As VoxVolumeType = Volume_Dynamic) As Vox_Volume
Declare Sub VoxSizeVolume Overload (ByVal Size As Vec3I)
Declare Sub VoxSizeVolume (SizeX As Integer, SizeY As Integer, SizeZ As Integer)
Declare Function VoxGetVolumeSize (Vol As Vox_Volume = -2) As Vec3I
Declare Sub VoxReloadVolumes
Declare Function VoxNewContext(ScreenVolume As Vox_Volume = 0) As Vox_Context

'File Save/Load
Declare Function VoxLoadFile (ByVal FileName As ZString Ptr, Depth As Integer = 0, T As VoxVolumeType = Volume_OffScreen) As Vox_Volume
Declare Sub VoxSaveFile (ByVal FileName As ZString Ptr, Vol As Vox_Volume)

'Drawing State
Declare Sub VoxSetContext(C As Vox_Context = -1)
Declare Sub VoxSetVolumeType(T As VoxVolumeType)
Declare Sub VoxSetColor(C As UInteger)
Declare Sub VoxSetVolume(Vol As Vox_Volume = VOXEL_SCREEN)
Declare Sub VoxSetSource(Vol As Vox_Volume)
'Declare Sub VoxEdgeSelection(E As Integer)
Declare Sub VoxSetBlitDefault
Declare Sub VoxBlitRightRotate(Axis As UInteger, ByVal Amount As Integer = 1)
Declare Sub VoxBlitReflect(Axis As UInteger)
'Declare Sub VoxSetBlitMode(Mode As UInteger)

'Drawing
Declare Sub VoxVolumeLock()
Declare Function VoxVolumePtr() As UInteger Ptr
Declare Sub VoxVolumeUnlock()
Declare Sub VoxCls()
Declare Sub VSet Overload (ByVal Voxel As Vec3I)
Declare Sub VSet(ByVal Voxel As Vec3I, Col As UInteger)
Declare Sub VoxLine(ByVal A As Vec3I, ByVal B As Vec3I)
Declare Sub VoxLineTo(ByVal B As Vec3I)
Declare Sub VoxTriangle(ByVal A As Vec3I, ByVal B As Vec3I, ByVal C As Vec3I)
Declare Sub VoxTriangleTo(ByVal B As Vec3I, ByVal C As Vec3I)
Declare Sub VoxTriangleFanTo(ByVal C As Vec3I)
Declare Sub VoxTriangleStripTo(ByVal C As Vec3I)
'Declare Sub VoxRectSolid(ByVal A As Vec3I, ByVal B As Vec3I)
'Declare Sub VoxSrcCoords(ByVal A As Vec3I, ByVal B As Vec3I, ByVal C As Vec3I)
'Declare Sub VoxSrcCoords(ByVal A As Vec3I, ByVal B As Vec3I)
'Declare Sub VoxSrcCoordsTo(ByVal B As Vec3I, ByVal C As Vec3I)
'Declare Sub VoxSrcCoordsTo(ByVal C As Vec3I)
Declare Sub VoxBlit(ByVal DestV As Vec3I, ByVal SrcV As Vec3I, ByVal Size As Vec3I)
'Declare Sub VoxBlitText(ByVal DestV As Vec3I, Str As ZString)

'Rendering
Declare Sub VoxRender(ScreenW As Integer, ScreenH As Integer, Flags As UInteger = 0)
Declare Sub VoxGlRenderState(ScreenW As Integer = 0, ScreenH As Integer = 0, Flags As UInteger = 0)
Declare Sub VoxRenderVolume(Vol As Vox_Volume)
Declare Sub VoxRenderSubVolume(Vol As Vox_Volume, ByVal A As Vec3I, ByVal B As Vec3I)
'Declare Sub VoxRenderText(ByVal DestV As Vec3I, Str As ZString)

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
Declare Function VoxCursorTest(ByRef V1 As Vec3I, ByRef V2 As Vec3I, PixX As Integer, PixY As Integer, ByRef MaxDist As Double = -1) As Integer
Declare Function VoxSubCursorTest(ByRef V1 As Vec3I, ByRef V2 As Vec3I, ByVal A As Vec3I, ByVal B As Vec3I, PixX As Integer, PixY As Integer, ByRef MaxDist As Double = -1) As Integer
Declare Function VoxWallTest(ByRef VX As Double, ByRef VY As Double, ByRef VZ As Double, PlaneAxis As UInteger, PixX As Integer, PixY As Integer, ByRef MaxDist As Double = -1) As Integer
Declare Function VoxPoint(ByVal Voxel As Vec3I) As UInteger
'Declare Sub VoxGetCubeFaces(ByRef Verts() As Single, ByRef Quads() As Integer)

End Extern