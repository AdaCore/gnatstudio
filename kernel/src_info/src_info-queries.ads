package Src_Info.Queries is

   --  provide services for all sorts of queries on the ALI files...

   type Query_Status is
     (Entity_Not_Found,
      Internal_Error,
      No_Body_Entity_Found,
      Success);
   --  The status returned by all query routines. Only upon success should
   --  return values returned be taken into consideration.

   procedure Find_Declaration_Or_Body
     (Lib_Info        : LI_File_Ptr;
      File_Name       : String;
      Entity_Name     : String;
      Line            : Positive;
      Column          : Positive;
      File_Name_Found : out String_Access;
      Start_Line      : out Positive;
      Start_Column    : out Positive;
      End_Line        : out Positive;
      End_Column      : out Positive;
      Status          : out Query_Status);
   --  Implement the Goto Declaration<->Body algorithm using the given
   --  Filename, Entity_Name, and Line/Column position.

end Src_Info.Queries;
