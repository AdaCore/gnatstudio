separate (Src_Info.CPP)

------------------------
--  Fu_To_Un_Handler  --
------------------------

procedure Fu_To_Un_Handler (Ref : TO_Table)
is
   Ref_Id : constant String := Ref.Buffer
     (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
   Union_Desc : CType_Description;
   Union_Def  : UN_Table;
   Success    : Boolean;
   Decl_Info  : E_Declaration_Info_List;
begin

   Info ("Fu_To_Un_Handler: '" & Ref_Id & "'");

   Find_Union
     (Type_Name      => Ref_Id,
      Desc           => Union_Desc,
      Union_Def      => Union_Def,
      Success        => Success);

   if not Success then
      Warn ("Union type " & Ref_Id & " is not found in SN DB");
      return;
   end if;

   if Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last) /=
         Union_Def.Buffer (Union_Def.File_Name.First ..
                                    Union_Def.File_Name.Last)
   then
      begin
         Decl_Info :=
           Find_Dependency_Declaration
             (File        => Global_LI_File,
              Symbol_Name => Union_Def.Buffer
                (Union_Def.Name.First .. Union_Def.Name.Last),
              Kind        => Record_Type,
              Location    => Union_Def.Start_Position,
              Filename    => Union_Def.Buffer
                (Union_Def.File_Name.First .. Union_Def.File_Name.Last));
      exception
         when Declaration_Not_Found =>
            Insert_Dependency_Declaration
              (Handler            => LI_Handler (Global_CPP_Handler),
               File               => Global_LI_File,
               List               => Global_LI_File_List,
               Symbol_Name        => Union_Def.Buffer
                 (Union_Def.Name.First .. Union_Def.Name.Last),
               Referred_Filename  => Union_Def.Buffer
                 (Union_Def.File_Name.First .. Union_Def.File_Name.Last),
               Source_Filename    => Ref.Buffer
                 (Ref.File_Name.First .. Ref.File_Name.Last),
               Location           => Union_Def.Start_Position,
               Kind               => Record_Type,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
      end;
   else
      begin
         Decl_Info :=
           Find_Declaration
             (File        => Global_LI_File,
              Symbol_Name => Union_Def.Buffer
                (Union_Def.Name.First .. Union_Def.Name.Last),
              Kind        => Record_Type,
              Location    => Union_Def.Start_Position);
      exception
         when Declaration_Not_Found =>
            Insert_Declaration
              (Handler            => LI_Handler (Global_CPP_Handler),
               File               => Global_LI_File,
               List               => Global_LI_File_List,
               Symbol_Name        => Union_Def.Buffer
                 (Union_Def.Name.First .. Union_Def.Name.Last),
               Source_Filename    => Ref.Buffer
                 (Ref.File_Name.First .. Ref.File_Name.Last),
               Location           => Union_Def.Start_Position,
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
   Free (Union_Def);
   Free (Union_Desc);

end Fu_To_Un_Handler;

