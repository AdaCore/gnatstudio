with GNAT.Directory_Operations,
     GNAT.IO_Aux,
     GNAT.OS_Lib;
use  GNAT.Directory_Operations,
     GNAT.IO_Aux,
     GNAT.OS_Lib;

package body SN.Browse is

   procedure Browse (File_Name, DB_Directory : in String;
         Browser_Name : in String := "cbrowser";
         DBUtils_Path : in String := "../sn/bin") is
      Xref_File_Name      : String := DB_Directory & Directory_Separator
                             & File_Name & ".xref" & ASCII.Nul;
      Success             : Boolean;
      Args                : Argument_List_Access;
   begin
      --  check DB_Directory exists
      declare
         Dir_Handle : Dir_Type;
      begin
         Open (Dir_Handle, DB_Directory);
         --  exists!
         Close (Dir_Handle);
      exception
         -- the target directory does not exists, create it
         when Directory_Error =>
            Make_Dir (DB_Directory);
      end;

      --  unlink cross reference file, if any
      if File_Exists (Xref_File_Name) then
         Delete_File (Xref_File_Name'Address, Success);
         if not Success then
            raise Unlink_Failure;
         end if;
      end if;

      -- Execute browser
      Args := Argument_String_To_List (
          "-n " & DB_Directory & Directory_Separator & DB_File_Name
          & " -p " & DBUtils_Path & Directory_Separator & "dbimp"
          & " -x " & Xref_File_Name
          & " " & File_Name);
      Spawn (DBUtils_Path & Directory_Separator & Browser_Name,
             Args.all, Success);
      if not Success then
         raise Spawn_Failure;
      end if;
   end Browse;

   procedure Generate_Xrefs (DB_Directory : in String) is
   begin
      null;
   end Generate_Xrefs;

end SN.Browse;
