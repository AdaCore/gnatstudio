separate (Src_Info.CPP)


------------------------
--  Fu_To_Cl_Handler  --
------------------------

procedure Fu_To_Cl_Handler (Ref : TO_Table)
is
   Class_Desc : CType_Description;
   Class_Def  : CL_Table;
   Success    : Boolean;
   Decl_Info  : E_Declaration_Info_List;
   Ref_Id     : constant String := Ref.Buffer
     (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
begin

   Info ("Fu_To_Cl_Handler: " & Ref_Id);

   Find_Class
     (Type_Name      => Ref_Id,
      Desc           => Class_Desc,
      Class_Def      => Class_Def,
      Success        => Success);

   if not Success then
      Warn ("Failed to find " & Ref_Id & " class");
      return;
   end if;

   if Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last) /=
         Class_Def.Buffer (Class_Def.File_Name.First ..
                                    Class_Def.File_Name.Last)
   then
      begin
         Decl_Info :=
           Find_Dependency_Declaration
             (File        => Global_LI_File,
              Symbol_Name => Class_Def.Buffer
                (Class_Def.Name.First .. Class_Def.Name.Last),
              Kind        => Record_Type,
              Location    => Class_Def.Start_Position,
              Filename    => Class_Def.Buffer
                (Class_Def.File_Name.First .. Class_Def.File_Name.Last));
      exception
         when Declaration_Not_Found =>
            Insert_Dependency_Declaration
              (Handler            => LI_Handler (Global_CPP_Handler),
               File               => Global_LI_File,
               List               => Global_LI_File_List,
               Symbol_Name        => Class_Def.Buffer
                 (Class_Def.Name.First .. Class_Def.Name.Last),
               Referred_Filename  => Class_Def.Buffer
                 (Class_Def.File_Name.First .. Class_Def.File_Name.Last),
               Source_Filename    => Ref.Buffer
                 (Ref.File_Name.First .. Ref.File_Name.Last),
               Location           => Class_Def.Start_Position,
               Kind               => Record_Type,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
      end;
   else
      begin
         Decl_Info :=
           Find_Declaration
             (File        => Global_LI_File,
              Symbol_Name => Class_Def.Buffer
                (Class_Def.Name.First .. Class_Def.Name.Last),
              Kind        => Record_Type,
              Location    => Class_Def.Start_Position);
      exception
         when Declaration_Not_Found =>
            Insert_Declaration
              (Handler            => LI_Handler (Global_CPP_Handler),
               File               => Global_LI_File,
               List               => Global_LI_File_List,
               Symbol_Name        => Class_Def.Buffer
                 (Class_Def.Name.First .. Class_Def.Name.Last),
               Source_Filename    => Ref.Buffer
                 (Ref.File_Name.First .. Ref.File_Name.Last),
               Location           => Class_Def.Start_Position,
               Kind               => Record_Type,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
      end;
   end if;
   Insert_Reference
     (File              => Global_LI_File,
      Declaration_Info  => Decl_Info,
      Source_Filename   => Ref.Buffer
        (Ref.File_Name.First .. Ref.File_Name.Last),
      Location          => Ref.Position,
      Kind              => Reference);
   Free (Class_Def);
   Free (Class_Desc);
end Fu_To_Cl_Handler;
