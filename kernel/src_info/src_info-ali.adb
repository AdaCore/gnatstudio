package body Src_Info.ALI is

   --------------------
   -- Parse_ALI_File --
   --------------------

   procedure Parse_ALI_File
     (Project       : Prj.Project_Id;
      List          : in out LI_File_List;
      Unit          : out LI_File_Ptr;
      ALI_Filename  : String;
      Success       : out Boolean)
   is
   begin
      --  ??? Will be implemented later.
      Unit := null;
      Success := False;
   end Parse_ALI_File;

end Src_Info.ALI;

