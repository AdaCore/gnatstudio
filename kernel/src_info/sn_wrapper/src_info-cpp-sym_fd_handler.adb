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
         and then Cmp_Prototypes
           (FD_Tab.Buffer,
            FD_Tab_Tmp.Buffer,
            FD_Tab.Arg_Types,
            FD_Tab_Tmp.Arg_Types,
            FD_Tab.Return_Type,
            FD_Tab_Tmp.Return_Type)
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
   if First_FD_Pos = Sym.Start_Position then
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
   else
      Decl_Info := Find_Declaration
         (Global_LI_File,
          Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
          "",
          Target_Kind,
          First_FD_Pos);
   end if;

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
