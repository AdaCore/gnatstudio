separate (Src_Info.CPP)

--------------------
-- Sym_MD_Handler --
--------------------

procedure Sym_MD_Handler (Sym : FIL_Table)
is
   Target_Kind  : E_Kind;
   Decl_Info    : E_Declaration_Info_List;
   P            : Pair_Ptr;
   First_MD_Pos : Point := Invalid_Point;
   MD_Tab       : MD_Table;
   MD_Tab_Tmp   : MD_Table;
   IsTemplate   : Boolean := False;
   use DB_Structures.Segment_Vector;

begin
   Info ("Sym_MD_Hanlder: """
         & Sym.Buffer (Sym.Class.First .. Sym.Class.Last) & "::"
         & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
         & """");

   --  Find this symbol
   MD_Tab := Find
     (SN_Table (MD),
      Sym.Buffer (Sym.Class.First .. Sym.Class.Last),
      Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
      Sym.Start_Position,
      Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last));

   Set_Cursor
     (SN_Table (MD),
      By_Key,
      Sym.Buffer (Sym.Class.First .. Sym.Class.Last) & Field_Sep
         & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
         & Field_Sep,
      False);

   loop
      P := Get_Pair (SN_Table (MD), Next_By_Key);
      exit when P = null;
      MD_Tab_Tmp := Parse_Pair (P.all);
      Free (P);
      --  Update position of the first forward declaration
      if Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last)
         = MD_Tab_Tmp.Buffer (MD_Tab_Tmp.File_Name.First ..
                              MD_Tab_Tmp.File_Name.Last)
         and then Cmp_Prototypes
           (MD_Tab.Buffer,
            MD_Tab_Tmp.Buffer,
            MD_Tab.Arg_Types,
            MD_Tab_Tmp.Arg_Types,
            MD_Tab.Return_Type,
            MD_Tab_Tmp.Return_Type)
         and then ((First_MD_Pos = Invalid_Point)
         or else Less (MD_Tab_Tmp.Start_Position, First_MD_Pos)) then
         First_MD_Pos := MD_Tab_Tmp.Start_Position;
      end if;
      Free (MD_Tab_Tmp);
   end loop;

   pragma Assert (First_MD_Pos /= Invalid_Point, "DB inconsistency");

   declare
      Class_Def    : CL_Table;
   begin -- check if this class is template
      Class_Def := Find
        (SN_Table (CL), Sym.Buffer (Sym.Class.First .. Sym.Class.Last));
      IsTemplate := Class_Def.Template_Parameters.First
         < Class_Def.Template_Parameters.Last;
      Free (Class_Def);
   exception
      when DB_Error | Not_Found =>
         null;
   end;

   if MD_Tab.Buffer (MD_Tab.Return_Type.First ..
                     MD_Tab.Return_Type.Last) = "void" then
      if IsTemplate then
         Target_Kind := Generic_Procedure;
      else
         Target_Kind := Non_Generic_Procedure;
      end if;
   else
      if IsTemplate then
         Target_Kind := Generic_Function_Or_Operator;
      else
         Target_Kind := Non_Generic_Function_Or_Operator;
      end if;
   end if;
   Free (MD_Tab);

   --  create declaration (if it has not been already created)
   if First_MD_Pos = Sym.Start_Position then
      Insert_Declaration
        (Handler           => LI_Handler (Global_CPP_Handler),
         File              => Global_LI_File,
         List              => Global_LI_File_List,
         Symbol_Name       =>
            Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Source_Filename   =>
            Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Location          => First_MD_Pos,
         Kind              => Target_Kind,
         Scope             => Global_Scope,
         Declaration_Info  => Decl_Info);
   else
      Decl_Info := Find_Declaration
        (Global_LI_File,
         Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Sym.Buffer (Sym.Class.First .. Sym.Class.Last),
         Target_Kind,
         First_MD_Pos);
   end if;

   --  for all subsequent declarations, add reference to the first decl
   if Sym.Start_Position /= First_MD_Pos then
      Insert_Reference
        (Decl_Info,
         Global_LI_File,
         Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Sym.Start_Position,
         Reference);
   end if;
exception
   when DB_Error | Not_Found =>
      Fail ("unable to find method " &
            Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));
end Sym_MD_Handler;
