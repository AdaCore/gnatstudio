separate (Src_Info.CPP)

--------------------
-- Sym_FD_Handler --
--------------------

procedure Sym_FD_Handler (Sym : FIL_Table)
is
   Target_Kind  : E_Kind;
   Decl_Info    : E_Declaration_Info_List;
   P            : Pair_Ptr;
   First_FD_Pos : Point := Invalid_Point;
   FD_Tab       : FD_Table;
   FD_Tab_Tmp   : FD_Table;

   function Less (A, B : Point) return Boolean;
   --  compares two positions within file

   function Less (A, B : Point) return Boolean is
   begin
      return (A.Line < B.Line)
         or else (A.Line = B.Line and then A.Column < B.Column);
   end Less;

   function Cmp_Prototypes (A, B : FD_Table) return Boolean;
   --  checks to see if function prototypes are the same

   function Cmp_Prototypes (A, B : FD_Table) return Boolean is
      use DB_Structures.Segment_Vector;
      Ptr_A : Segment_Vector.Node_Access := A.Arg_Types;
      Ptr_B : Segment_Vector.Node_Access := B.Arg_Types;
   begin
      while Ptr_A /= null and Ptr_B /= null loop
         if A.Buffer (Ptr_A.Data.First .. Ptr_A.Data.Last)
            /= B.Buffer (Ptr_B.Data.First .. Ptr_B.Data.Last) then
            Info ("differ!!!");
            return False;
         end if;
         Ptr_A := Ptr_A.Next;
         Ptr_B := Ptr_B.Next;
      end loop;

      if Ptr_A /= null or Ptr_B /= null then
         Info ("differ!!!");
         return False;
      end if;

      Info ("may differ!!!");
      return A.Buffer (A.Return_Type.First .. A.Return_Type.Last)
         = B.Buffer (B.Return_Type.First .. B.Return_Type.Last);
   end Cmp_Prototypes;

begin
   Info ("Sym_FD_Hanlder: """
         & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
         & """");

   --  Find this symbol
   FD_Tab := Find
     (SN_Table (FD),
      Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
      Sym.Start_Position,
      Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last));

   Set_Cursor
     (SN_Table (FD),
      By_Key,
      Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last) & Field_Sep,
      False);

   loop
      P := Get_Pair (SN_Table (FD), Next_By_Key);
      exit when P = null;
      FD_Tab_Tmp := Parse_Pair (P.all);
      Free (P);
      --  Update position of the first forward declaration
      if Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last)
         = FD_Tab_Tmp.Buffer (FD_Tab_Tmp.File_Name.First ..
                              FD_Tab_Tmp.File_Name.Last)
         and then Cmp_Prototypes (FD_Tab, FD_Tab_Tmp)
         and then ((First_FD_Pos = Invalid_Point)
         or else Less (FD_Tab_Tmp.Start_Position, First_FD_Pos)) then
         First_FD_Pos := FD_Tab_Tmp.Start_Position;
      end if;
      Free (FD_Tab_Tmp);
   end loop;

   pragma Assert (First_FD_Pos /= Invalid_Point, "DB inconsistency");

   if FD_Tab.Buffer (FD_Tab.Return_Type.First ..
                     FD_Tab.Return_Type.Last) = "void" then
      Target_Kind := Non_Generic_Procedure;
   else
      Target_Kind := Non_Generic_Function_Or_Operator;
   end if;
   Free (FD_Tab);

   --  create declaration (if it has not been already created)
   Insert_Declaration
     (Handler           => LI_Handler (Global_CPP_Handler),
      File              => Global_LI_File,
      List              => Global_LI_File_List,
      Symbol_Name       =>
        Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
      Source_Filename   =>
        Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
      Location          => First_FD_Pos,
      Kind              => Target_Kind,
      Scope             => Global_Scope,
      Declaration_Info  => Decl_Info);

   --  for all subsequent declarations, add reference to the first decl
   if Sym.Start_Position /= First_FD_Pos then
      Insert_Reference
        (Decl_Info,
         Global_LI_File,
         Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Sym.Start_Position,
         Reference);
   end if;
exception
   when DB_Error | Not_Found =>
      Fail ("unable to find function " &
            Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));
end Sym_FD_Handler;
