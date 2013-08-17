
#Include Once "crt/string.bi"

#Ifndef VA_UBOUND
#Define VA_UBOUND(ARRAY) ARRAY##_UBound

#Define IsUnSignedIntType(X) ((X) = UInteger Or (X) = UByte Or (X) = UShort Or (X) = ULong Or (X) = ULongInt)
#Define IsSignedIntType(X) ((X) = Integer Or (X) = Byte Or (X) = Short Or (X) = Long Or (X) = LongInt)
#Define IsFloatType(X) ((X) = Single Or (X) = Double)
#Define IsStringType(X) ((X) = TypeOf(String) Or (X) = ZString Or (X) = TypeOf(WString))
#Define IsBaseType(X) (IsSignedIntType(X) Or IsUnSignedIntType(X) Or IsFloatType(X) Or IsStringType(X))

#MACRO VA_EXTERN(ARRAY, VARTYPE)
    Extern ARRAY As VARTYPE Ptr
    Extern ARRAY##_UBound As Integer
    Extern ARRAY##_AllocSpace As Integer = 0
#ENDMACRO

#MACRO VA_DIM(ARRAY, VARTYPE)
    Dim ARRAY As VARTYPE Ptr = NULL
    Dim ARRAY##_UBound As Integer = -1
    Dim ARRAY##_AllocSpace As Integer = 0
#ENDMACRO

#MACRO VA_DIM_SHARED(ARRAY, VARTYPE)
    Dim Shared ARRAY As VARTYPE Ptr = NULL
    Dim Shared ARRAY##_UBound As Integer = -1
    Dim Shared ARRAY##_AllocSpace As Integer = 0
#ENDMACRO

#MACRO VARARRAY(ARRAY, VARTYPE)
    ARRAY As VARTYPE Ptr = NULL
    ARRAY##_UBound As Integer = -1
    ARRAY##_AllocSpace As Integer = 0
#ENDMACRO

#Macro VAI_Destructor_CallLoop(ARRAY, START_VAL, STOP_VAL)
    #IfnDef VA_NO_DESTRUCTOR
        #If Not (IsBaseType(TypeOf(*ARRAY)) Or SizeOf(TypeOf(*ARRAY)) = SizeOf(Ptr))
            For Temp_Array_Index As Integer = 0 To ARRAY##_UBound
                ARRAY[Temp_Array_Index].Destructor
            Next Temp_Array_Index
        #EndIf
    #EndIf
#EndMacro

#Macro VAI_Constructor_CallLoop(ARRAY, START_VAL, STOP_VAL)
    #IfnDef VA_NO_CONSTRUCTOR
        #If Not (IsBaseType(TypeOf(*ARRAY)) Or SizeOf(TypeOf(*ARRAY)) = SizeOf(Ptr))
            For Temp_Array_Index As Integer = 0 To ARRAY##_UBound
                ARRAY[Temp_Array_Index].Constructor
            Next Temp_Array_Index
        #EndIf
    #EndIf
#EndMacro

' Same Effect as VA_ReDim(ARRAY, -1)
#MACRO VA_ERASE(ARRAY)
    If ARRAY <> NULL Then
        VAI_Destructor_CallLoop(ARRAY, 0, ARRAY##_UBound)
        DeAllocate(ARRAY)
        ARRAY = NULL
    End If
    ARRAY##_UBound = -1
    ARRAY##_AllocSpace = 0
#ENDMACRO

#MACRO VA_FREE(ARRAY) 'Depreciated
    VA_ERASE(ARRAY)
#ENDMACRO

#MACRO VA_LET(ARRAY_LHS, ARRAY_RHS)
    If ARRAY_LHS <> NULL Then
        VAI_Destructor_CallLoop(ARRAY_LHS, 0, ARRAY##_UBound)
        DeAllocate(ARRAY_LHS)
    End If
    
    ARRAY_LHS##_AllocSpace = ARRAY_RHS##_AllocSpace
    ARRAY_LHS = CAllocate(ARRAY_LHS##_AllocSpace, SizeOf(TypeOf(*ARRAY_RHS)))
    VAI_Constructor_CallLoop(ARRAY_LHS, 0, ARRAY_RHS##_UBound)
    
    ARRAY_LHS##_UBound = ARRAY_RHS##_UBound
    
    For Temp_Array_Index As Integer = 0 To ARRAY_RHS##_UBound
        ARRAY_LHS[Temp_Array_Index] = ARRAY_RHS[Temp_Array_Index]
    Next Temp_Array_Index
#ENDMACRO

#MACRO VA_REDIM(ARRAY, NEW_UBOUND)
    If ARRAY <> NULL Then
        VAI_Destructor_CallLoop(ARRAY, 0, ARRAY##_UBound)
        DeAllocate(ARRAY)
    End If
    ARRAY##_UBound = (NEW_UBOUND)
    If NEW_UBOUND < 0 Then
        ARRAY = NULL
        ARRAY##_AllocSpace = 0
    Else
        ARRAY##_AllocSpace = ARRAY##_UBound + 1
        ARRAY = CAllocate(ARRAY##_AllocSpace, SizeOf(TypeOf(*ARRAY)))
        VAI_Constructor_CallLoop(ARRAY, 0, ARRAY##_UBound)
    End If
#ENDMACRO

#MACRO VA_REDIM_PRESERVE(ARRAY, NEW_UBOUND)
    If NEW_UBOUND < 0 Then
        If ARRAY <> NULL Then
            VAI_Destructor_CallLoop(ARRAY, 0, ARRAY##_UBound)
            DeAllocate(ARRAY)
        End If
        ARRAY = NULL
        ARRAY##_AllocSpace = 0
    Else
        If ARRAY = NULL Then
            ARRAY##_AllocSpace = 2*((NEW_UBOUND) + 1)
            ARRAY = CAllocate(ARRAY##_AllocSpace, SizeOf(TypeOf(*ARRAY)))
            VAI_Constructor_CallLoop(ARRAY, 0, (NEW_UBOUND))
        Else
            If (NEW_UBOUND) > ARRAY##_UBound Then
                If (NEW_UBOUND) >= ARRAY##_AllocSpace Then
                    ARRAY##_AllocSpace = 2*((NEW_UBOUND) + 1)
                    ' Shrink first to eliminate the potential for redundant copying
                    ARRAY = ReAllocate(ARRAY, (ARRAY##_UBound + 1) * SizeOf(TypeOf(*ARRAY)))
                    ARRAY = ReAllocate(ARRAY, ARRAY##_AllocSpace * SizeOf(TypeOf(*ARRAY)))
                End If
                Clear ARRAY[ARRAY##_UBound + 1], 0, ((NEW_UBOUND) - ARRAY##_UBound) * SizeOf(TypeOf(*ARRAY))
                VAI_Constructor_CallLoop(ARRAY, ARRAY##_UBound + 1, (NEW_UBOUND))
            Else
                VAI_Destructor_CallLoop(ARRAY, (NEW_UBOUND) + 1, ARRAY##_UBound)
                If ARRAY##_AllocSpace > 2*((NEW_UBOUND) + 1) Then
                    ARRAY##_AllocSpace = 2*((NEW_UBOUND) + 1)
                    ARRAY = ReAllocate(ARRAY, ARRAY##_AllocSpace * SizeOf(TypeOf(*ARRAY)))
                End If
            End If
        End If
    End If
    ARRAY##_UBound = (NEW_UBOUND)
#ENDMACRO

'Note: These take time linear in the size of the tail of the array.
#Macro VA_Insert_Element(ARRAY, IndexToInsert)
    VA_ReDim_Preserve(ARRAY, VA_UBound(ARRAY) + 1)
    Scope
        Dim As Any Ptr Temp_Element_Ptr = Allocate(SizeOf(TypeOf(*ARRAY)))
        memcpy Temp_Element_Ptr, @ARRAY[VA_UBound(ARRAY)], SizeOf(TypeOf(*ARRAY))
        memmove @ARRAY[(IndexToInsert) + 1], @ARRAY[IndexToInsert], _
                (VA_UBound(ARRAY) - (IndexToInsert)) * SizeOf(TypeOf(*ARRAY))
        memcpy @ARRAY[IndexToInsert], Temp_Element_Ptr, SizeOf(TypeOf(*ARRAY))
        DeAllocate Temp_Element_Ptr
    End Scope
#EndMacro

#Macro VA_Remove_Element(ARRAY, IndexToRemove)
    Scope
        Dim As Any Ptr Temp_Element_Ptr = Allocate(SizeOf(TypeOf(*ARRAY)))
        memcpy Temp_Element_Ptr, @ARRAY[IndexToRemove], SizeOf(TypeOf(*ARRAY))
        memmove @ARRAY[IndexToRemove], @ARRAY[(IndexToRemove) + 1], _
                (VA_UBound(ARRAY) - (IndexToRemove)) * SizeOf(TypeOf(*ARRAY))
        memcpy @ARRAY[VA_UBound(ARRAY)], Temp_Element_Ptr, SizeOf(TypeOf(*ARRAY))
        DeAllocate Temp_Element_Ptr
    End Scope
    VA_ReDim_Preserve(ARRAY, VA_UBound(ARRAY) - 1)
#EndMacro

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
        Declare Property R(ByVal Index As Integer) ByRef As VARTYPE
        Declare Property V(ByVal Index As Integer, ByRef Rhs As VARTYPE)
        Declare Sub Erase_()
        Declare Sub ReDim_(ByVal New_UBound As Integer)
        Declare Sub ReDim_Preserve_(ByVal New_UBound As Integer)
        Declare Function UBound_() As Integer
        Declare Sub Insert(ByVal Index As Integer)
        Declare Sub Remove(ByVal Index As Integer)
        
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
        'Return @(A[Abs(Index Mod (VA_UBound(A)+1))])
        Return @(A[Index])
    End Property
    
    Property VARARRAY_##VARTYPE.R(ByVal Index As Integer) ByRef As VARTYPE
        Assert(Index >= 0)
        Assert(Index <= VA_UBound(A))
        'Return A[Abs(Index Mod (VA_UBound(A)+1))]
        Return A[Index]
    End Property
    
    Property VARARRAY_##VARTYPE.V(ByVal Index As Integer, ByRef Rhs As VARTYPE)
        Assert(Index >= 0)
        Assert(Index <= VA_UBound(A))
        'A[Abs(Index Mod (VA_UBound(A)+1))] = Rhs
        A[Index] = Rhs
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
    
    Sub VARARRAY_##VARTYPE.Insert(ByVal Index As Integer)
        Assert(Index >= 0)
        Assert(Index <= VA_UBound(A))
        VA_Insert_Element(A, Index)
    End Sub
    
    Sub VARARRAY_##VARTYPE.Remove(ByVal Index As Integer)
        Assert(Index >= 0)
        Assert(Index <= VA_UBound(A))
        VA_Remove_Element(A, Index)
    End Sub
    
    #IfDef VA_MAKE_FILEPUTGET
        Sub VARARRAY_##VARTYPE.FilePut(FileNum As Integer)
            Dim I As Integer
            Put #FileNum, , VA_UBound(A)
            For I = 0 To VA_UBound(A)
                #IfDef VARTYPE##_HAS_FILEPUTGET
                    A[I].FilePut FileNum
                #Else
                    Put #FileNum, , A[I]
                #EndIf
            Next I
        End Sub
        
        Sub VARARRAY_##VARTYPE.FileGet(FileNum As Integer)
            Dim I As Integer
            Get #FileNum, , VA_UBound(A)
            VA_ReDim(A, VA_UBound(A))
            For I = 0 To VA_UBound(A)
                #IfDef VARTYPE##_HAS_FILEPUTGET
                    A[I].FileGet FileNum
                #Else
                    Get #FileNum, , A[I]
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
#Define VA_USE(ARRAY, INDEX) (*(ARRAY.P(INDEX)))
#Else
#Define VA_USE(ARRAY, INDEX) ARRAY.A[INDEX]
#EndIf '__FB_DEBUG__ <> 0

#EndIf 'VA_UBOUND