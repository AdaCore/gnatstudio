separate (Src_Info.CPP)

------------------------
--  Fu_To_Ma_Handler  --
------------------------

procedure Fu_To_Ma_Handler (Ref : TO_Table) is
   Macro  : MA_Table;
   Ref_Id : String := Ref.Buffer
     (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);

   Type_Decl_Info : E_Declaration_Info_List;
begin
   if not Is_Open (SN_Table (MA)) then
      --  .ma table does not exist
      return;
   end if;

   Macro := Find (SN_Table (MA), Ref_Id);

   begin
      --  looking thru declarations in current file
      Type_Decl_Info := Find_Declaration
        (File        => Global_LI_File,
         Symbol_Name => Ref_Id,
         Location    => Macro.Start_Position);

      --  declaration found, insert reference
      Insert_Reference
        (Declaration_Info     => Type_Decl_Info,
         File                 => Global_LI_File,
         Source_Filename      =>
           Ref.Buffer (Macro.File_Name.First .. Macro.File_Name.Last),
         Location             => Ref.Position,
         Kind                 => Reference);

      Free (Macro);
      return;
   exception
      when Declaration_Not_Found => null; -- not found
   end;

   --  macro declaration not found in currect file
   begin
      null;
   exception
      when Declaration_Not_Found => null; -- not found
   end;

   Free (Macro);

exception
   when DB_Error | Not_Found  => null; -- ignore

end Fu_To_Ma_Handler;

