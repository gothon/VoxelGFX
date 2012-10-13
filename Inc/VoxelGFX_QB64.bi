TYPE Vec3I
    X AS LONG
    Y AS LONG
    Z AS LONG
END TYPE

CONST VOXEL_SCREEN32 = &H0005~&
CONST VOXEL_SCREEN64 = &H0006~&
CONST VOXEL_SCREEN128 = &H0007~&
CONST VOXEL_SCREEN256 = &H0008~&
CONST VOXEL_SCREEN512 = &H0009~&

CONST VOXEL_NOCLEAR = &H0001~&
CONST VOXEL_NOLIGHT = &H0002~&
CONST VOXEL_NOMODELVIEW = &H0004~&
CONST VOXEL_NOGLSTATE = &H0008~&

CONST VOXEL_VIEWPORT_ONLY = &H000~&
CONST VOXEL_CLEAR = (VOXEL_VIEWPORT_ONLY XOr VOXEL_NOCLEAR)
CONST VOXEL_LIGHT = (VOXEL_VIEWPORT_ONLY XOr VOXEL_NOLIGHT)
CONST VOXEL_MODELVIEW = (VOXEL_VIEWPORT_ONLY XOr VOXEL_NOMODELVIEW)
CONST VOXEL_GLSTATE = (VOXEL_VIEWPORT_ONLY XOr VOXEL_NOGLSTATE)

CONST VOXEL_AXIS_X = &H0001~&
CONST VOXEL_AXIS_Y = &H0002~&
CONST VOXEL_AXIS_Z = &H0003~&

CONST VOXEL_SCREEN = (-1&)

'Enum VoxVolumeType
CONST Volume_Dynamic = 0
CONST Volume_Static = 1
CONST Volume_OffScreen = 2
'End Enum

'Note: GL and GLU must be linked but will probably be linked to by
'      whatever OpenGL based library we are running on top of.
DECLARE LIBRARY "VoxelGFX\libVoxelGFX", "VoxelGFX\libfbpngs", "VoxelGFX\libfb" ', "sfml\libopengl32", "sfml\libglu32"
    ' Initilization
    SUB VoxInit (BYVAL GlExtFetch AS _OFFSET, BYVAL Flags AS _UNSIGNED LONG)
    SUB VoxScreenRes (BYVAL SizeX AS LONG, BYVAL SizeY AS LONG, BYVAL SizeZ AS LONG, BYVAL BackColor AS _UNSIGNED LONG)
    FUNCTION VoxNewVolume& (BYVAL SizeX AS LONG, BYVAL SizeY AS LONG, BYVAL SizeZ AS LONG, T AS LONG)
    SUB VoxSizeVolume (BYVAL SizeX AS LONG, BYVAL SizeY AS LONG, BYVAL SizeZ AS LONG)
    SUB VoxGetVolumeSize (Size AS Vec3I, BYVAL Volume AS LONG)
    SUB VoxReloadVolumes
    FUNCTION VoxNewContext& (BYVAL ScreenVolume AS LONG)
    
    ' Files
    FUNCTION VoxLoadFile& (FileName AS STRING, BYVAL VolType AS LONG)
    SUB VoxSaveFile (FileName AS STRING, BYVAL Volume AS LONG)
    
    ' States
    SUB VoxSetContext (BYVAL C AS LONG)
    SUB VoxSetVolumeType (BYVAL T AS LONG)
    SUB VoxSetColor (BYVAL C AS _UNSIGNED LONG)
    SUB VoxSetVolume (BYVAL Volume AS LONG)
    SUB VoxSetSource (BYVAL Volume AS LONG)
    SUB VoxSetBlitDefault
    SUB VoxBlitRightRotate (BYVAL Axis AS _UNSIGNED LONG, BYVAL Amount AS LONG)
    SUB VoxBlitReflect (Axis AS _UNSIGNED LONG)
    
    ' Drawing
    SUB VoxVolumeLock
    FUNCTION VoxVolumePtr&
    SUB VoxVolumeUnlock
    SUB VoxCls
    SUB VSet (BYVAL Col AS _UNSIGNED LONG)
    SUB VoxLine
    SUB VoxLineTo
    SUB VoxTriangle
    SUB VoxTriangleTo
    SUB VoxTriangleFanTo
    SUB VoxTriangleStripTo
    SUB VoxBlit
    
    ' Rendering
    SUB VoxRender (BYVAL ScreenW AS LONG, BYVAL ScreenH AS LONG, BYVAL Flags AS _UNSIGNED LONG)
    SUB VoxGlRenderState (BYVAL ScreenW AS LONG, BYVAL ScreenH AS LONG, BYVAL Flags AS _UNSIGNED LONG)
    SUB VoxRenderVolume (BYVAL Volume AS LONG)
    SUB VoxRenderSubVolume (BYVAL Volume AS LONG)
    
    ' Perspective
    SUB VoxScreenTurnRight (BYVAL Angle AS DOUBLE)
    SUB VoxScreenTurnDown (Angle AS DOUBLE)
    SUB VoxScreenTurnCCW (Angle AS DOUBLE)
    SUB VoxScreenMoveRight (Dist AS DOUBLE)
    SUB VoxScreenMoveUp (Dist AS DOUBLE)
    SUB VoxScreenMoveForward (Dist AS DOUBLE)
    SUB VoxScreenCenter (X AS DOUBLE, Y AS DOUBLE, Z AS DOUBLE)
    SUB VoxScreenDistance (Dist AS DOUBLE)
    
    ' Reading
    FUNCTION VoxCursorTest& (V1 AS Vec3I, V2 AS Vec3I, BYVAL PixX AS LONG, BYVAL PixY AS LONG, MaxDist AS DOUBLE)
    FUNCTION VoxSubCursorTest& (V1 AS Vec3I, V2 AS Vec3I, BYVAL PixX AS LONG, BYVAL PixY AS LONG, MaxDist AS DOUBLE)
    FUNCTION VoxWallTest& (VX AS DOUBLE, VY AS DOUBLE, VZ AS DOUBLE, BYVAL PlaneAxis AS _UNSIGNED LONG, BYVAL PixX AS LONG, BYVAL PixY AS LONG, MaxDist AS DOUBLE)
    FUNCTION VoxPoint~& (BYVAL X AS LONG, BYVAL Y AS LONG, BYVAL Z AS LONG)
    
    ' QB64 Wrapper
    SUB VoxInitGl (BYVAL Flags AS _UNSIGNED LONG)
    SUB V3I (BYVAL X AS LONG, BYVAL Y AS LONG, BYVAL Z AS LONG)
END DECLARE