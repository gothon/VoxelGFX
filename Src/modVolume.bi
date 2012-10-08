#Include "VoxelGFX.bi"
#Include "VarArray.bi"

#Include Once "GL/gl.bi"
#Include Once "GL/glu.bi"
#Include Once "GL/glext.bi"

Namespace InternalVoxelGFX

VA_DECLARE_WRAPPER(Byte)
VA_DECLARE_WRAPPER(UInteger)

Type InternalVoxelVolume
    As GLuint Tex, PBO
    Union
        Type
            As UInteger W, H, D
        End Type
        As UInteger Size(2)
    End Union
    As VoxVolumeType VolType
    As Integer LockedCount
    As VarArray_UInteger ClientTex
    As VarArray_Byte VisLayerX, VisLayerY, VisLayerZ
    Declare Sub Render()
    Declare Sub RenderAll()
    Declare Sub Render(V1 As Vec3I, V2 As Vec3I)
    Declare Sub RenderAll(V1 As Vec3I, V2 As Vec3I)
    Declare Sub DetermineLayerVisability()
    Declare Sub Lock()
    Declare Sub UnLock()
    Declare Sub UnLockNoUpdate()
End Type

Common Shared glTexImage3D As Sub(ByVal target As GLenum, ByVal level As GLint, ByVal internalFormat As GLint, ByVal width As GLsizei, ByVal height As GLsizei, ByVal depth As GLsizei, ByVal border As GLint, ByVal format As GLenum, ByVal type As GLenum, ByVal data As GLvoid Ptr)
Common Shared glTexSubImage3D As Sub(ByVal target As GLenum, ByVal level As GLint, ByVal xoffset As GLint, ByVal yoffset As GLint, ByVal zoffset As GLint, ByVal width As GLsizei, ByVal height As GLsizei, ByVal depth As GLsizei, ByVal format As GLenum, ByVal type As GLenum, ByVal data As GLvoid Ptr)

Common Shared glGenBuffers As Sub(ByVal n As GLsizei, ByVal buffers As GLuint Ptr)
Common Shared glBindBuffer As Sub(ByVal target As GLenum, ByVal buffer As GLuint)
Common Shared glBufferData As Sub(ByVal target As GLenum, ByVal size As GLsizeiptr, ByVal Data As GLvoid Ptr, ByVal usage As GLenum)
'Common Shared glDeleteBuffers As Sub(ByVal n As GLsizei, ByVal buffers As GLuint Ptr)

End Namespace 'InternalVoxelGFX