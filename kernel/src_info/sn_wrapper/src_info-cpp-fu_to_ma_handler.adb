separate (Src_Info.CPP)

------------------------
--  Fu_To_Ma_Handler  --
------------------------

procedure Fu_To_Ma_Handler (Ref : TO_Table) is
   Macro  : MA_Table;
   Ref_Id : constant String := Ref.Buffer
     (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
   Decl_Info : E_Declaration_Info_List;
begin
   if not Is_Open (SN_Table (MA)) then
      --  .ma table does not exist
      return;
   end if;

   Info ("Fu_To_Ma: " & Ref_Id);

   Macro := Find (SN_Table (MA), Ref_Id);

   if Macro.Buffer (Macro.File_Name.First .. Macro.File_Name.Last)
     = Get_LI_Filename (Global_LI_File)
   then
      begin
         --  look for declaration in current file
         Decl_Info := Find_Declaration
           (File        => Global_LI_File,
            Symbol_Name => Ref_Id,
            Location    => Macro.Start_Position);

      exception
         when Declaration_Not_Found =>
            Fail ("unable to find declaration for macro " & Ref_Id);
      end;
   else
      --  look for dependency declaration
      begin
         Decl_Info := Find_Dependency_Declaration
           (File                 => Global_LI_File,
            Symbol_Name          => Ref_Id,
            Filename             => Macro.Buffer
              (Macro.File_Name.First .. Macro.File_Name.Last),
            Location             => Macro.Start_Position);
      exception
         when Declaration_Not_Found => -- dep decl does not yet exist
            Insert_Dependency_Declaration
              (Handler           => LI_Handler (Global_CPP_Handler),
               File              => Global_LI_File,
               List              => Global_LI_File_List,
               Symbol_Name       => Ref_Id,
               Source_Filename   => Ref.Buffer
                 (Ref.File_Name.First .. Ref.File_Name.Last),
               Location          => Macro.Start_Position,
               Kind              => Unresolved_Entity,
               Scope             => Global_Scope,
               Referred_Filename => Macro.Buffer
                 (Macro.File_Name.First .. Macro.File_Name.Last),
               Declaration_Info  => Decl_Info);
      end;
   end if;

   Insert_Reference
     (Declaration_Info     => Decl_Info,
      File                 => Global_LI_File,
      Source_Filename      =>
        Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
      Location             => Ref.Position,
      Kind                 => Reference);

   Free (Macro);

exception
   when DB_Error | Not_Found =>
      Fail ("unable to find macro " & Ref_Id);

end Fu_To_Ma_Handler;

