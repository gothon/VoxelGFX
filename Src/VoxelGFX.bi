#Inclib "VoxelGFX"
#Pragma Once

#Define GetGLProcAddressCast(X) Cast(Function(ByRef Proc As Const ZString) As Any Ptr, X)

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

Enum VoxVolumeType
    Volume_Dynamic = 0
    Volume_Static = 1
    Volume_OffScreen = 2
    'Volume_Segmented = 3
End Enum

Type Vox_Volume As Integer

' Integral 3D Vector
Type Vec3I Alias "Vec3I"
    Union
        Type
            As Integer X, Y, Z
        End Type
        V(2) As Integer
    End Union
    Declare Constructor()
    Declare Constructor(V As Vec3I)
    Declare Constructor(X As Integer, Y As Integer, Z As Integer)
    
    Declare Operator += (ByRef Rhs As Vec3I)
    Declare Operator -= (ByRef Rhs As Vec3I)
    Declare Operator *= (ByRef Rhs As Integer)
    Declare Operator \= (ByRef Rhs As Integer)
    
    Declare Operator Cast () As String
End Type

Declare Operator -(ByRef Rhs As Vec3I) As Vec3I
Declare Operator Abs (ByRef Rhs As Vec3I) As Double

Declare Operator + (ByRef Lhs As Vec3I, ByRef Rhs As Vec3I) As Vec3I
Declare Operator - (ByRef Lhs As Vec3I, ByRef Rhs As Vec3I) As Vec3I

'Sclar Products
Declare Operator * (ByRef Lhs As Vec3I, ByRef Rhs As Integer) As Vec3I
Declare Operator * (ByRef Lhs As Integer, ByRef Rhs As Vec3I) As Vec3I
Declare Operator \ (ByRef Lhs As Vec3I, ByRef Rhs As Integer) As Vec3I
'Dot Product
Declare Operator * (ByRef Lhs As Vec3I, ByRef Rhs As Vec3I) As Integer
'Cross Product
Declare Function Cross Overload (ByRef Lhs As Vec3I, ByRef Rhs As Vec3I) As Vec3I

Declare Operator = (ByRef Lhs As Vec3I, ByRef Rhs As Vec3I) As Integer
Declare Operator <> (ByRef Lhs As Vec3I, ByRef Rhs As Vec3I) As Integer

'Set Up
Declare Sub VoxInit Alias "VoxInit" (GlExtFetch As Function(ByRef Proc As Const ZString) As Any Ptr, Flags As UInteger = 0)
Declare Sub VoxScreenRes Overload Alias "VoxScreenRes" (Size As Vec3I, BackColor As UInteger = 0)
Declare Sub VoxScreenRes Alias "VoxScreenRes" (SizeX As Integer, SizeY As Integer, SizeZ As Integer, BackColor As UInteger = 0)
Declare Function VoxNewVolume Overload Alias "VoxNewVolume" (T As VoxVolumeType = 0) As Vox_Volume
Declare Function VoxNewVolume Alias "VoxNewVolume" (Size As Vec3I, T As VoxVolumeType = 0) As Vox_Volume
Declare Function VoxNewVolume Alias "VoxNewVolume" (SizeX As Integer, SizeY As Integer, SizeZ As Integer, T As VoxVolumeType = 0) As Vox_Volume
Declare Sub VoxSizeVolume Overload Alias "VoxSizeVolume" (Size As Vec3I)
Declare Sub VoxSizeVolume Alias "VoxSizeVolume" (SizeX As Integer, SizeY As Integer, SizeZ As Integer)
Declare Function VoxGetVolumeSize Alias "VoxGetVolumeSize" (V As Vox_Volume = -1) As Vec3I

'File Save/Load
Declare Function VoxLoadFile Alias "VoxLoadFile" (FileName As ZString, Depth As Integer = 0, T As VoxVolumeType = Volume_OffScreen) As Vox_Volume
Declare Sub VoxSaveFile Alias "VoxSaveFile" (FileName As ZString, V As Vox_Volume)

'Drawing State
Declare Sub VoxSetVolumeType(T As VoxVolumeType)
Declare Sub VoxSetColor(C As UInteger)
Declare Sub VoxSetVolume(V As Vox_Volume)
Declare Sub VoxSetSource(V As Vox_Volume)
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
Declare Sub VSet Overload (Voxel As Vec3I)
Declare Sub VSet(Voxel As Vec3I, Col As UInteger)
Declare Sub VoxLine(A As Vec3I, B As Vec3I)
Declare Sub VoxLineTo(B As Vec3I)
'Declare Sub VoxTriangle(A As Vec3I, B As Vec3I, C As Vec3I)
'Declare Sub VoxTriangleTo(B As Vec3I, C As Vec3I)
'Declare Sub VoxTriangleFanTo(C As Vec3I)
'Declare Sub VoxTriangleStripTo(C As Vec3I)
'Declare Sub VoxRectSolid(A As Vec3I, B As Vec3I)
'Declare Sub VoxSrcCoords(A As Vec3I, B As Vec3I, C As Vec3I)
'Declare Sub VoxSrcCoords(A As Vec3I, B As Vec3I)
'Declare Sub VoxSrcCoordsTo(B As Vec3I, C As Vec3I)
'Declare Sub VoxSrcCoordsTo(C As Vec3I)
Declare Sub VoxBlit(ByVal DestV As Vec3I, ByVal SrcV As Vec3I, ByVal Size As Vec3I)
'Declare Sub VoxBlitText(DestV As Vec3I, Str As ZString)

'Rendering
Declare Sub VoxRender(ScreenW As Integer, ScreenH As Integer, Flags As UInteger = 0)
Declare Sub VoxGlRenderState(ScreenW As Integer = 0, ScreenH As Integer = 0, Flags As UInteger = 0)
Declare Sub VoxRenderVolume(Model As Vox_Volume)
Declare Sub VoxRenderSubVolume(Model As Vox_Volume, ByVal A As Vec3I, ByVal B As Vec3I)
'Declare Sub VoxRenderText(DestV As Vec3I, Str As ZString)

'Perspective Control
Declare Sub VoxScreenTurnRight(Angle As Double)
Declare Sub VoxScreenTurnDown(Angle As Double)
Declare Sub VoxScreenTurnCCW(Angle As Double)
Declare Sub VoxScreenMoveRight(Dist As Double)
Declare Sub VoxScreenMoveUp(Dist As Double)
Declare Sub VoxScreenMoveForward(Dist As Double)
Declare Sub VoxScreenCenter Overload (V As Vec3I)
Declare Sub VoxScreenCenter(X As Double, Y As Double, Z As Double)
Declare Sub VoxScreenDistance(Dist As Double)

'Reading
Declare Function VoxCursorTest(ByRef V1 As Vec3I, ByRef V2 As Vec3I, PixX As Integer, PixY As Integer, ByRef MaxDist As Double = -1) As Integer
Declare Function VoxSubCursorTest(ByRef V1 As Vec3I, ByRef V2 As Vec3I, ByVal A As Vec3I, ByVal B As Vec3I, PixX As Integer, PixY As Integer, ByRef MaxDist As Double = -1) As Integer
Declare Function VoxPoint(Voxel As Vec3I) As UInteger