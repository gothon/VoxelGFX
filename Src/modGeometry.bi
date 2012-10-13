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

#Include Once "GL/gl.bi"
#Include Once "GL/glu.bi"
#Include Once "GL/glext.bi"

#Ifndef Pi
#Define Pi 3.14159265358979
#EndIf 'Pi

Namespace InternalVoxelGFX

Type Vec3F
    Union
        V(2) As Double
        Type
            As Double X, Y, Z
        End Type
    End Union
    Declare Constructor()
    Declare Constructor(V As Vec3F)
    Declare Constructor(X As Double, Y As Double, Z As Double)
    
    Declare Operator += (ByRef Rhs As Vec3F)
    Declare Operator -= (ByRef Rhs As Vec3F)
    Declare Operator *= (ByRef Rhs As Double)
    Declare Operator /= (ByRef Rhs As Double)
    
    Declare Operator Cast () As String
End Type

Declare Operator -(ByRef Rhs As Vec3F) As Vec3F
Declare Operator Abs (ByRef Rhs As Vec3F) As Double
Declare Operator Int (ByRef Rhs As Vec3F) As Vec3F

Declare Operator + (ByRef Lhs As Vec3F, ByRef Rhs As Vec3F) As Vec3F
Declare Operator - (ByRef Lhs As Vec3F, ByRef Rhs As Vec3F) As Vec3F

'Sclar Products
Declare Operator * (ByRef Lhs As Vec3F, ByRef Rhs As Double) As Vec3F
Declare Operator * (ByRef Lhs As Double, ByRef Rhs As Vec3F) As Vec3F
Declare Operator / (ByRef Lhs As Vec3F, ByRef Rhs As Double) As Vec3F
'Dot Product
Declare Operator * (ByRef Lhs As Vec3F, ByRef Rhs As Vec3F) As Double
'Cross Product
Declare Function Cross Overload (ByRef Lhs As Vec3F, ByRef Rhs As Vec3F) As Vec3F

Declare Operator = (ByRef Lhs As Vec3F, ByRef Rhs As Vec3F) As Integer
Declare Operator <> (ByRef Lhs As Vec3F, ByRef Rhs As Vec3F) As Integer

Type Camera3D
    Dim As Vec3F Location = Vec3F(0, 0, 0), MidP = Vec3F(0, 0, 0)
    Dim As Vec3F ForeVect = Vec3F(0, 0, 1), LeftVect = Vec3F(-1, 0, 0), UpVect = Vec3F(0, 1, 0)
    
    Declare Constructor()
    
    Declare Sub SetCamera()
    Declare Sub TurnRight(Angle As Double)
    Declare Sub TurnDown(Angle As Double)
    Declare Sub SpinCounterClockwise(Angle As Double)
    Declare Sub CamRotate(Axis As Vec3F, Angle As Double)
End Type

Declare Sub AspectProjectionView(ScreenW As Integer, ScreenH As Integer, Near As Single = 1, Far As Single = 2048)
Declare Function DeProject(X As GLdouble, Y As GLdouble, Depth As GLdouble = 0.0) As Vec3F
Declare Function MvMult(MvMat() As GLdouble, V As Vec3F) As Vec3F

End Namespace 'InternalVoxelGFX