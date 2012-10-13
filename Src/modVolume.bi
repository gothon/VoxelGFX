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