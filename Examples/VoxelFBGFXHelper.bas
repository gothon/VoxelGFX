#Include Once "VoxelGFX.bi"

#Define SwapRB(C) (CUInt(C) Shr 16 And &HFF Or (CUInt(C) And &HFF) Shl 16 Or CUInt(C) And &HFF00FF00)

Sub ConsolePrint(S As String)
    Dim As Integer F = FreeFile
    Open Cons For Output As #F
        Print #F, S
    Close #F
End Sub

Sub DrawFBGfxImage(Image As Any Ptr, V As Vec3I, Plane As Integer = 0)
    Dim Size As Vec3I = VoxGetVolumeSize
    Dim As Integer Pitch, W, H, ByPP, X, Y
    Dim Pixels As Any Ptr
    Dim Row As UInteger Ptr
    
    If 0 <> ImageInfo(Image, W, H, ByPP, Pitch, Pixels) Then Exit Sub
    If ByPP <> 4 Then Exit Sub
    
    VoxVolumeLock
    Dim Voxels As UInteger Ptr = VoxVolumePtr
    For Y = 0 To H-1
        Row = Pixels + (H-1-Y) * Pitch
        Select Case Plane
        Case 0
            For X = 0 To W-1
                Voxels[X+V.X + Size.X*(Y+V.Y + Size.Y*V.Z)] = SwapRB(Row[X])
            Next X
        Case 1
            For X = 0 To W-1
                Voxels[X+V.X + Size.X*(V.Y + Size.Y*(Y+V.Z))] = SwapRB(Row[X])
            Next X
        Case 2
            For X = 0 To W-1
                Voxels[V.X + Size.X*(Y+V.Y + Size.Y*(X+V.Z))] = SwapRB(Row[X])
            Next X
        End Select
    Next Y
    VoxVolumeUnlock
End Sub

Function GetFBGfxImage(V As Vec3I, W As Integer, H As Integer, Plane As Integer = 0) As Any Ptr
    Dim Size As Vec3I = VoxGetVolumeSize
    Dim As Integer Pitch, ByPP, X, Y
    Dim Pixels As Any Ptr
    Dim Row As UInteger Ptr
    
    Dim Image As Any Ptr = ImageCreate(W, H, 0, 32)
    Function = Image
    If Image = 0 Then Exit Function
    If 0 <> ImageInfo(Image, W, H, ByPP, Pitch, Pixels) Then Exit Function
    If ByPP <> 4 Then Exit Function
    
    VoxVolumeLock
    Dim Voxels As UInteger Ptr = VoxVolumePtr
    For Y = 0 To H-1
        Row = Pixels + (H-1-Y) * Pitch
        Select Case Plane
        Case 0
            For X = 0 To W-1
                Row[X] = SwapRB(Voxels[X+V.X + Size.X*(Y+V.Y + Size.Y*V.Z)])
            Next X
        Case 1
            For X = 0 To W-1
                Row[X] = SwapRB(Voxels[X+V.X + Size.X*(V.Y + Size.Y*(Y+V.Z))])
            Next X
        Case 2
            For X = 0 To W-1
                Row[X] = SwapRB(Voxels[V.X + Size.X*(Y+V.Y + Size.Y*(X+V.Z))])
            Next X
        End Select
    Next Y
    VoxVolumeUnlock
End Function

Sub VoxelPrint(S As String, V As Vec3I, C As UInteger = -1, Plane As Integer = 0)
    Dim Size As Vec3I = VoxGetVolumeSize - V
    Dim Im As Any Ptr
    VoxVolumeLock
    Select Case Plane
    Case 0: Im = GetFBGfxImage(V, Size.X, Size.Y, Plane)
    Case 1: Im = GetFBGfxImage(V, Size.X, Size.Z, Plane)
    Case 2: Im = GetFBGfxImage(V, Size.Z, Size.Y, Plane)
    End Select
    If Im = 0 Then Exit Sub
    Draw String Im, (0,0), S, C
    DrawFBGfxImage Im, V, Plane
    ImageDestroy Im
    VoxVolumeUnLock
End Sub