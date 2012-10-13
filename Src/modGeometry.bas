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

Namespace InternalVoxelGFX

'Vec3F
Constructor Vec3F()
    X = 0
    Y = 0
    Z = 0
End Constructor

Constructor Vec3F(V As Vec3F)
    X = V.X
    Y = V.Y
    Z = V.Z
End Constructor

Constructor Vec3F(X As Double, Y As Double, Z As Double)
    This.X = X
    This.Y = Y
    This.Z = Z
End Constructor

Operator Vec3F.+= (ByRef Rhs As Vec3F)
    X += Rhs.X
    Y += Rhs.Y
    Z += Rhs.Z
End Operator

Operator Vec3F.-= (ByRef Rhs As Vec3F)
    X -= Rhs.X
    Y -= Rhs.Y
    Z -= Rhs.Z
End Operator

Operator Vec3F.*= (ByRef Rhs As Double)
    X *= Rhs
    Y *= Rhs
    Z *= Rhs
End Operator

Operator Vec3F./= (ByRef Rhs As Double)
    X /= Rhs
    Y /= Rhs
    Z /= Rhs
End Operator

Operator Vec3F.Cast () As String
    Return "(" & X & ", " & Y & ", " & Z & ")"
End Operator

Operator -(ByRef Rhs As Vec3F) As Vec3F
    Return Type(-Rhs.X, -Rhs.Y, -Rhs.Z)
End Operator

Operator Abs(ByRef Rhs As Vec3F) As Double
    Return Sqr(Rhs.X*Rhs.X + Rhs.Y*Rhs.Y + Rhs.Z*Rhs.Z)
End Operator

Operator Int(ByRef Rhs As Vec3F) As Vec3F
    Return Type(Int(Rhs.X), Int(Rhs.Y), Int(Rhs.Z))
End Operator

Operator + (ByRef Lhs As Vec3F, ByRef Rhs As Vec3F) As Vec3F
    Return Type(Lhs.X + Rhs.X, Lhs.Y + Rhs.Y, Lhs.Z + Rhs.Z)
End Operator

Operator - (ByRef Lhs As Vec3F, ByRef Rhs As Vec3F) As Vec3F
    Return Type(Lhs.X - Rhs.X, Lhs.Y - Rhs.Y, Lhs.Z - Rhs.Z)
End Operator

'Scalar Products
Operator * (ByRef Lhs As Vec3F, ByRef Rhs As Double) As Vec3F
    Return Type(Lhs.X * Rhs, Lhs.Y * Rhs, Lhs.Z * Rhs)
End Operator
Operator * (ByRef Lhs As Double, ByRef Rhs As Vec3F) As Vec3F
    Return Type(Lhs * Rhs.X, Lhs * Rhs.Y, Lhs * Rhs.Z)
End Operator
Operator / (ByRef Lhs As Vec3F, ByRef Rhs As Double) As Vec3F
    Return Type(Lhs.X / Rhs, Lhs.Y / Rhs, Lhs.Z / Rhs)
End Operator
'Dot Product
Operator * (ByRef Lhs As Vec3F, ByRef Rhs As Vec3F) As Double
    Return Lhs.X*Rhs.X + Lhs.Y*Rhs.Y + Lhs.Z*Rhs.Z
End Operator
'Cross Product
Function Cross (ByRef Lhs As Vec3F, ByRef Rhs As Vec3F) As Vec3F
    Return Type(Lhs.Y*Rhs.Z - Lhs.Z*Rhs.Y, Lhs.Z*Rhs.X - Lhs.X*Rhs.Z, Lhs.X*Rhs.Y - Lhs.Y*Rhs.X)
End Function

Operator = (ByRef Lhs As Vec3F, ByRef Rhs As Vec3F) As Integer
    Return (Lhs.X = Rhs.X And Lhs.Y = Rhs.Y And Lhs.Z = Rhs.Z)
End Operator

Operator <> (ByRef Lhs As Vec3F, ByRef Rhs As Vec3F) As Integer
    Return (Lhs.X <> Rhs.X Or Lhs.Y <> Rhs.Y Or Lhs.Z <> Rhs.Z)
End Operator

' Rotation Functions
Function Modula(Numerator As Double, Denominator As Double) As Double
    Return Numerator - Int(Numerator / Denominator) * Denominator
End Function

Function PrinAngle(X As Double) As Double
    Dim As Single Theta = Modula(X + Pi, 2 * Pi) - Pi
    If Theta < -Pi Then Return Theta + 2 * Pi
    Return Theta
End Function

' Assuming Axis is a unit vector
Sub AxisRotate3D(ByRef V As Vec3F, ByRef MidP As Vec3F, ByRef Axis As Vec3F, CosA As Double, SinA As Double)
    Dim As Vec3F X, Y, Z
    
    V -= MidP
    
    Z = (V*Axis)*Axis
    X = V - Z
    Y = Cross(X, Axis)
    
    V = Z + X*CosA + Y*SinA
    
    V += MidP
End Sub

'Camera3D
Constructor Camera3D()
    MidP = Vec3F(0, 0, 0)
    Location = Vec3F(0, 0, 0)
    LeftVect = Vec3F(1, 0, 0)
    UpVect   = Vec3F(0, 1, 0)
    ForeVect = Vec3F(0, 0,-1)
End Constructor

Sub Camera3D.SetCamera()
    Dim As GLdouble MvMat(15) = {LeftVect.X, UpVect.X, -ForeVect.X, 0.0, _
                                LeftVect.Y, UpVect.Y, -ForeVect.Y, 0.0, _
                                LeftVect.Z, UpVect.Z, -ForeVect.Z, 0.0, _
                                -Location * LeftVect, -Location * UpVect, Location * ForeVect, 1.0}
    glMatrixMode GL_MODELVIEW
    glLoadMatrixd @MvMat(0)
End Sub

Sub Camera3D.TurnRight(Angle As Double)
    Dim V As Vec3F, CosA As Double, SinA As Double
    CosA = Cos(Angle)
    SinA = Sgn(PrinAngle(Angle))*Sqr(1 - CosA*CosA)
    
    V = ForeVect + Location
    AxisRotate3D V, MidP, UpVect, CosA, SinA
    AxisRotate3D Location, MidP, UpVect, CosA, SinA
    ForeVect = V - Location
    ForeVect /= Abs(ForeVect)
    LeftVect = Cross(ForeVect, UpVect)
End Sub

Sub Camera3D.TurnDown(Angle As Double)
    Dim V As Vec3F, CosA As Double, SinA As Double
    CosA = Cos(Angle)
    SinA = Sgn(PrinAngle(Angle))*Sqr(1 - CosA*CosA)
    
    V = ForeVect + Location
    AxisRotate3D V, MidP, LeftVect, CosA, SinA
    AxisRotate3D Location, MidP, LeftVect, CosA, SinA
    ForeVect = V - Location
    ForeVect /= Abs(ForeVect)
    UpVect = Cross(LeftVect, ForeVect)
End Sub

Sub Camera3D.SpinCounterClockwise(Angle As Double)
    Dim V As Vec3F, CosA As Double, SinA As Double
    CosA = Cos(Angle)
    SinA = Sgn(PrinAngle(Angle))*Sqr(1 - CosA*CosA)
    
    V = LeftVect + Location
    AxisRotate3D V, MidP, ForeVect, CosA, SinA
    AxisRotate3D Location, MidP, ForeVect, CosA, SinA
    LeftVect = V - Location
    LeftVect /= Abs(LeftVect)
    UpVect = Cross(LeftVect, ForeVect)
End Sub

Sub Camera3D.CamRotate(Axis As Vec3F, Angle As Double)
    Dim As Single CosA, SinA
    CosA = Cos(Angle)
    SinA = Sgn(PrinAngle(Angle))*Sqr(1 - CosA*CosA)
    
    Dim As Vec3F X, Y
    X = LeftVect + Location
    Y = UpVect + Location
    
    AxisRotate3D X, MidP, Axis, CosA, SinA
    AxisRotate3D Y, MidP, Axis, CosA, SinA
    AxisRotate3D Location, MidP, Axis, CosA, SinA
    
    LeftVect = X - Location
    UpVect = Y - Location
    LeftVect /= Abs(LeftVect)
    UpVect /= Abs(UpVect)
    ForeVect = Cross(UpVect, LeftVect)
End Sub

'MinFOV = Pi/4
Sub AspectProjectionView(ScreenW As Integer, ScreenH As Integer, Near As Single = 1, Far As Single = 2048)
    glViewport 0, 0, ScreenW, ScreenH
    glMatrixMode GL_PROJECTION
    glLoadIdentity
    If ScreenW > ScreenH Then
        gluPerspective 45.0, ScreenW/ScreenH, Near, Far
       Else
        gluPerspective 360 * Atn(Tan(Pi / 8) * ScreenH/ScreenW)/Pi, ScreenW/ScreenH, Near, Far
    End If
End Sub

Function DeProject(X As GLdouble, Y As GLdouble, Depth As GLdouble = 0.0) As Vec3F
    Dim ViewPort(3) As GLint
    Dim As GLdouble MvMat(15), ProjMat(15), Wx, Wy, Wz
    
    glGetIntegerv GL_VIEWPORT, @ViewPort(0)
    glGetDoublev GL_MODELVIEW_MATRIX, @MvMat(0)
    glGetDoublev GL_PROJECTION_MATRIX, @ProjMat(0)
    
    gluUnProject X, ViewPort(3) - Y - 1, Depth, @MvMat(0), @ProjMat(0), @ViewPort(0), @Wx, @Wy, @Wz
    Return Vec3F(Wx, Wy, Wz)
End Function

' Multplys a 3 vector by a 4x4 ModelView matrix
Function MvMult(MvMat() As GLdouble, V As Vec3F) As Vec3F
    Return Type(MvMat(0)*V.X + MvMat(4)*V.Y + MvMat(8)*V.Z + MvMat(12), _
                MvMat(1)*V.X + MvMat(5)*V.Y + MvMat(9)*V.Z + MvMat(13), _
                MvMat(2)*V.X + MvMat(6)*V.Y + MvMat(10)*V.Z + MvMat(14))
End Function

End Namespace 'InternalVoxelGFX