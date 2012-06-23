' /////////////////////////////////////
' || VarArray.bi
' || Variable Length Array Helper Macros
' || by Alex Thomson 2011-2012
' ||
' || Copyright (C) Alex Thomson.
' ||
' || This program is released under the terms of the GNU General Public License (GPL)
' || as published by the Free Software Foundation, GPL version 2 or any later version.
' \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

#Ifndef NULL
#Define NULL 0
#EndIf

#Ifndef VA_UBOUND
#Define VA_UBOUND(ARRAY) ARRAY##_UBound

#MACRO VA_EXTERN(ARRAY, VARTYPE)
    Extern ARRAY As VARTYPE Ptr
    Extern ARRAY##_UBound As Integer
#ENDMACRO

#MACRO VA_DIM(ARRAY, VARTYPE)
    Dim ARRAY As VARTYPE Ptr = NULL
    Dim ARRAY##_UBound As Integer = -1
#ENDMACRO

#MACRO VA_DIM_SHARED(ARRAY, VARTYPE)
    Dim Shared ARRAY As VARTYPE Ptr = NULL
    Dim Shared ARRAY##_UBound As Integer = -1
#ENDMACRO

#MACRO VARARRAY(ARRAY, VARTYPE)
    ARRAY As VARTYPE Ptr = NULL
    ARRAY##_UBound As Integer = -1
#ENDMACRO

' Same Effect as VA_ReDim(ARRAY, -1)
#MACRO VA_ERASE(ARRAY)
    If ARRAY <> NULL Then Delete[] ARRAY
    ARRAY = NULL
    ARRAY##_UBound = -1
#ENDMACRO

#MACRO VA_FREE(ARRAY) 'Depreciated
    VA_ERASE(ARRAY)
#ENDMACRO

#MACRO VA_LET(ARRAY_LHS, ARRAY_RHS)
    If ARRAY_LHS <> NULL Then Delete[] ARRAY_LHS
    ARRAY_LHS = New TypeOf(*(ARRAY_RHS))[ARRAY_RHS##_UBound + 1]
    ARRAY_LHS##_UBound = ARRAY_RHS##_UBound
    Scope
        For Temp_Array_Index As Integer = 0 To ARRAY_RHS##_UBound
            ARRAY_LHS[Temp_Array_Index] = ARRAY_RHS[Temp_Array_Index]
        Next Temp_Array_Index
    End Scope
#ENDMACRO

#MACRO VA_REDIM(ARRAY, NEW_UBOUND)
    If ARRAY <> NULL Then Delete[] ARRAY
    ARRAY##_UBound = (NEW_UBOUND)
    If NEW_UBOUND < 0 Then
        ARRAY = NULL
    Else
        ARRAY = New TypeOf(*(ARRAY))[ARRAY##_UBound + 1]
    End If
#ENDMACRO

#MACRO VA_REDIM_PRESERVE(ARRAY, NEW_UBOUND)
    If NEW_UBOUND < 0 Then
        ARRAY = NULL
    Else
        If ARRAY = NULL Then
            ARRAY = New TypeOf(*(ARRAY))[(NEW_UBOUND) + 1]
        Else
            Scope
                Dim Temp_Array_Ptr As TypeOf(ARRAY) = New TypeOf(*(ARRAY))[(NEW_UBOUND) + 1]
                Dim Temp_Array_Index As Integer
                If (NEW_UBOUND) > ARRAY##_UBound Then
                    For Temp_Array_Index = 0 To ARRAY##_UBound
                        Temp_Array_Ptr[Temp_Array_Index] = ARRAY[Temp_Array_Index]
                        'Swap Temp_Array_Ptr[Temp_Array_Index], ARRAY[Temp_Array_Index]
                    Next Temp_Array_Index
                Else
                    For Temp_Array_Index = 0 To (NEW_UBOUND)
                        Temp_Array_Ptr[Temp_Array_Index] = ARRAY[Temp_Array_Index]
                        'Swap Temp_Array_Ptr[Temp_Array_Index], ARRAY[Temp_Array_Index]
                    Next Temp_Array_Index
                End If
                Delete[] ARRAY
                ARRAY = Temp_Array_Ptr
            End Scope
        End If
    End If
    ARRAY##_UBound = (NEW_UBOUND)
#ENDMACRO

#MACRO VA_DECLARE_WRAPPER(VARTYPE)
    Type VARARRAY_##VARTYPE
        VARARRAY(A, VARTYPE)
        
        Declare Constructor
        Declare Constructor(New_UBound As Integer)
        Declare Constructor(Source As VARARRAY_##VARTYPE)
        Declare Destructor
        
        Declare Operator Let(Rhs As VARARRAY_##VARTYPE)
        Declare Operator Cast () As VARTYPE Ptr
        
        Declare Property P(ByVal Index As Integer) As VARTYPE Ptr
        Declare Property V(ByVal Index As Integer) As VARTYPE
        Declare Property V(ByVal Index As Integer, ByVal Rhs As VARTYPE)
        Declare Sub Erase_()
        Declare Sub ReDim_(ByVal New_UBound AS Integer)
        Declare Sub ReDim_Preserve_(ByVal New_UBound AS Integer)
        Declare Function UBound_() As Integer
        
        #IfDef VA_MAKE_FILEPUTGET
            #Define VARARRAY_##VARTYPE##_HAS_FILEPUTGET
            Declare Sub FilePut(FileNum As Integer)
            Declare Sub FileGet(FileNum As Integer)
        #EndIf
    End Type
#ENDMACRO

#MACRO VA_WRAPPER(VARTYPE)
    Constructor VARARRAY_##VARTYPE
        'No Initialization Needed
    End Constructor
    
    Constructor VARARRAY_##VARTYPE(New_UBound As Integer)
        VA_ReDim(A, New_UBound)
    End Constructor
    
    Constructor VARARRAY_##VARTYPE(Source As VARARRAY_##VARTYPE)
        VA_LET(A, Source.A)
    End Constructor

    Destructor VARARRAY_##VARTYPE
        VA_ERASE(A)
    End Destructor
    
    Operator VARARRAY_##VARTYPE.Let(Rhs As VARARRAY_##VARTYPE)
        VA_LET(A, Rhs.A)
    End Operator
    
    Operator VARARRAY_##VARTYPE.Cast () As VARTYPE Ptr
        Return A
    End Operator
    
    Property VARARRAY_##VARTYPE.P(ByVal Index As Integer) As VARTYPE Ptr
        Assert(Index >= 0)
        Assert(Index <= VA_UBound(A))
        Return @(A[Abs(Index Mod (VA_UBound(A)+1))])
    End Property
    
    Property VARARRAY_##VARTYPE.V(ByVal Index As Integer) As VARTYPE
        Assert(Index >= 0)
        Assert(Index <= VA_UBound(A))
        Return A[Abs(Index Mod (VA_UBound(A)+1))]
    End Property
    
    Property VARARRAY_##VARTYPE.V(ByVal Index As Integer, ByVal Rhs As VARTYPE)
        Assert(Index >= 0)
        Assert(Index <= VA_UBound(A))
        A[Abs(Index Mod (VA_UBound(A)+1))] = Rhs
    End Property
    
    Sub VARARRAY_##VARTYPE.Erase_()
        VA_ERASE(A)
    End Sub
    
    Sub VARARRAY_##VARTYPE.ReDim_(ByVal New_UBound As Integer)
        VA_ReDim(A, New_UBound)
    End Sub
    
    Sub VARARRAY_##VARTYPE.ReDim_Preserve_(ByVal New_UBound As Integer)
        VA_ReDim_Preserve(A, New_UBound)
    End Sub
    
    Function VARARRAY_##VARTYPE.UBound_() As Integer
        Return VA_UBound(A)
    End Function
    
    #IfDef VA_MAKE_FILEPUTGET
        Sub VARARRAY_##VARTYPE.FilePut(FileNum As Integer)
            Dim I As Integer
            Put #FileNum, , VA_UBound(A)
            For I = 0 To VA_UBound(A)
                #IfDef VARTYPE##_HAS_FILEPUTGET '_ZN11##VARTYPE##7FILEPUTEi@8
                    A[I].FilePut FileNum
                #Else
                    Put #FileNum, , A[I]
                    '#Print VARTYPE
                #EndIf
            Next I
        End Sub
        
        Sub VARARRAY_##VARTYPE.FileGet(FileNum As Integer)
            Dim I As Integer
            Get #FileNum, , VA_UBound(A)
            VA_ReDim(A, VA_UBound(A))
            For I = 0 To VA_UBound(A)
                #IfDef VARTYPE##_HAS_FILEPUTGET '_ZN11##VARTYPE##7FILEGETEi@8
                    A[I].FileGet FileNum
                #Else
                    Get #FileNum, , A[I]
                    '#Print VARTYPE
                #EndIf
            Next I
        End Sub
    #EndIf
#ENDMACRO

#MACRO VA_MAKE_WRAPPER(VARTYPE)
VA_DECLARE_WRAPPER(VARTYPE)
VA_WRAPPER(VARTYPE)
#ENDMACRO

#If __FB_DEBUG__ <> 0
#Define VA_USE(ARRAY, INDEX) (*ARRAY.P(INDEX))
#Else
#Define VA_USE(ARRAY, INDEX) ARRAY.A[INDEX]
#EndIf '__FB_DEBUG__ <> 0

#EndIf 'VA_UBOUND