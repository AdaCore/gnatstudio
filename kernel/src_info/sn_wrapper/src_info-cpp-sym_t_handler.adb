separate (Src_Info.CPP)

--------------------
-- Sym_T_Handler --
--------------------

procedure Sym_T_Handler (Sym : FIL_Table)
is
   Decl_Info : E_Declaration_Info_List;
   Desc       : CType_Description;
   Success    : Boolean;
   Identifier : String :=
     Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last);
begin

   Info ("Sym_T_Hanlder: """ & Identifier & """");

   if not Is_Open (SN_Table (T)) then
      --  .t table does not exist, nothing to do
      return;
   end if;

   --  find original type for given typedef
   Original_Type (Identifier, Desc, Success);

   if Success then
      --  we know E_Kind for original type
      --  Ancestor_Point and Ancestor_Filename has information about
      --  parent type (do not mess with Parent_xxx in CType_Description)

      if Desc.Ancestor_Point = Invalid_Point then
         --  unknown parent
         Insert_Declaration
           (Handler           => LI_Handler (Global_CPP_Handler),
            File              => Global_LI_File,
            List              => Global_LI_File_List,
            Symbol_Name       => Identifier,
            Source_Filename   =>
              Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Location          => Sym.Start_Position,
            Kind              => Desc.Kind,
            Scope             => Global_Scope,
            Declaration_Info  => Decl_Info);

      elsif Desc.Ancestor_Point = Predefined_Point then
         --  parent type is builtin: set parent location to predefined one
         --  ??? Builtin_Name is not used anywhere. We should
         --  use it (e.g. for a field like Predefined_Type_Name)
         Insert_Declaration
           (Handler           => LI_Handler (Global_CPP_Handler),
            File              => Global_LI_File,
            List              => Global_LI_File_List,
            Symbol_Name       => Identifier,
            Source_Filename   =>
              Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Location          => Sym.Start_Position,
            Parent_Location   => Predefined_Point,
            Kind              => Desc.Kind,
            Scope             => Global_Scope,
            Declaration_Info  => Decl_Info);

      else
         --  Set parent location to ancestor location
         Insert_Declaration
           (Handler           => LI_Handler (Global_CPP_Handler),
            File              => Global_LI_File,
            List              => Global_LI_File_List,
            Symbol_Name       => Identifier,
            Source_Filename   =>
              Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Location          => Sym.Start_Position,
            Parent_Filename   => Desc.Ancestor_Filename.all,
            Parent_Location   => Desc.Ancestor_Point,
            Kind              => Desc.Kind,
            Scope             => Global_Scope,
            Declaration_Info  => Decl_Info);

      end if;

   else

      --  could not get E_Kind for the original type
      Warn ("Typedef " & Identifier & ": could not be handled");

   end if;

   Free (Desc);

end Sym_T_Handler;
