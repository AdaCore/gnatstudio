with Src_Info;
with Prj;
with Prj.Tree;
with Language_Handlers;

package Docgen.ALI_Utils is

   procedure Load_Project
     (Name : String;
      Handler      : access Language_Handlers.Language_Handler_Record'Class;
      Project_Tree : out Prj.Tree.Project_Node_Id;
      Project_View : out Prj.Project_Id);
   --  Load a project file

   function Predefined_Source_Path return String;
   function Predefined_Object_Path return String;
   --  Return the predefined source paths for the current compiler

   procedure Load_LI_File
     (Source_Info_List : in out Src_Info.LI_File_List;
      Handler          : Language_Handlers.Language_Handler;
      Project_View     : Prj.Project_Id;
      Source_Filename  : String;
      LI               : out Src_Info.LI_File_Ptr);
   --  Find, Load and Parse the LI file for the corresponding source
   --  file

   function Create_Lang_Handler return Language_Handlers.Language_Handler;
   --  Create a language handler

end Docgen.ALI_Utils;
