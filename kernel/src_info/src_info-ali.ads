with Prj;

package Src_Info.ALI is

   procedure Parse_ALI_File
      (Project       : Prj.Project_Id;
       List          : in out LI_File_List;
       Unit          : out LI_File_Ptr;
       ALI_Filename  : String;
       Success       : out Boolean);
   --  Parse the given ALI file and update the Unit Info list. Also returns
   --  a handle to the Unit Info corresponding to this ALI file to avoid
   --  searching right-back for this handle for immediate queries.
   --
   --  Defined in a child package instead of the Src_Info package itself,
   --  mainly because the implementation of such subprograms can be lengthy.

end Src_Info.ALI;
