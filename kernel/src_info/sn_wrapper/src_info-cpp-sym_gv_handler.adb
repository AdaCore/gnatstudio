separate (Src_Info.CPP)

--------------------
-- Sym_GV_Handler --
--------------------

procedure Sym_GV_Handler (Sym : FIL_Table)
is
   Desc              : CType_Description;
   Var               : GV_Table;
   Success           : Boolean;
   Decl_Info         : E_Declaration_Info_List;
   Attributes        : SN_Attributes;
   Scope             : E_Scope := Global_Scope;
begin
   Info ("Sym_GV_Handler: """
         & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
         & """");

   if not Is_Open (SN_Table (GV)) then
      --  GV table does not exist, nothing to do ...
      return;
   end if;

   --  Lookup variable type
   Var := Find (SN_Table (GV), Sym.Buffer
      (Sym.Identifier.First .. Sym.Identifier.Last));

   Type_Name_To_Kind (Var.Buffer
      (Var.Value_Type.First .. Var.Value_Type.Last), Desc, Success);

   if not Success then -- type not found
      --  ?? Is ot OK to set E_Kind to Unresolved_Entity for global variables
      --  with unknown type?
      Desc.Kind := Unresolved_Entity;
   end if;

   Attributes := SN_Attributes (Var.Attributes);

   if (Attributes and SN_STATIC) = SN_STATIC then
      Scope := Static_Local;
   end if;

   if Desc.Parent_Point = Invalid_Point then
      Insert_Declaration
        (Handler           => LI_Handler (Global_CPP_Handler),
         File              => Global_LI_File,
         List              => Global_LI_File_List,
         Symbol_Name       =>
           Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Source_Filename   =>
           Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Location          => Sym.Start_Position,
         Kind              => Type_To_Object (Desc.Kind),
         Scope             => Scope,
         Declaration_Info  => Decl_Info);
   else
      Insert_Declaration
        (Handler           => LI_Handler (Global_CPP_Handler),
         File              => Global_LI_File,
         List              => Global_LI_File_List,
         Symbol_Name       =>
           Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Source_Filename   =>
           Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Location          => Sym.Start_Position,
         Kind              => Type_To_Object (Desc.Kind),
         Scope             => Scope,
         Parent_Location   => Desc.Parent_Point,
         Parent_Filename   => Desc.Parent_Filename.all,
         Declaration_Info  => Decl_Info);

      --  add reference to the type of this variable
      if Desc.IsTemplate then
         --  template specialization
         Refer_Type
           (Var.Buffer (Var.Value_Type.First .. Var.Value_Type.Last),
            Desc.Parent_Point,
            Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Sym.Start_Position,
            Instantiation_Reference);
      else
         --  default reference kind
         Refer_Type
           (Var.Buffer (Var.Value_Type.First .. Var.Value_Type.Last),
            Desc.Parent_Point,
            Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Sym.Start_Position);
      end if;
   end if;

   Free (Var);
   Free (Desc);
exception
   when  DB_Error |   -- non-existent table
         Not_Found => -- no such variable
      null;           -- ignore error
end Sym_GV_Handler;
