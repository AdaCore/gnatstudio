pragma Ada_2012;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with clang_c_Index_h;
with clang_c_CXString_h;

package clang_c_Documentation_h is

   type CXComment is record
      ASTNode : System.Address;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:38
      TranslationUnit : clang_c_Index_h.CXTranslationUnit;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:39
   end record
   with Convention => C_Pass_By_Copy;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:40

   --  skipped anonymous struct anon_44

   function clang_Cursor_getParsedComment (C : clang_c_Index_h.CXCursor) return CXComment  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:47
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Cursor_getParsedComment";

   type CXCommentKind is 
     (CXComment_Null,
      CXComment_Text,
      CXComment_InlineCommand,
      CXComment_HTMLStartTag,
      CXComment_HTMLEndTag,
      CXComment_Paragraph,
      CXComment_BlockCommand,
      CXComment_ParamCommand,
      CXComment_TParamCommand,
      CXComment_VerbatimBlockCommand,
      CXComment_VerbatimBlockLine,
      CXComment_VerbatimLine,
      CXComment_FullComment)
   with Convention => C;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:54

   type CXCommentInlineCommandRenderKind is 
     (CXCommentInlineCommandRenderKind_Normal,
      CXCommentInlineCommandRenderKind_Bold,
      CXCommentInlineCommandRenderKind_Monospaced,
      CXCommentInlineCommandRenderKind_Emphasized)
   with Convention => C;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:165

   type CXCommentParamPassDirection is 
     (CXCommentParamPassDirection_In,
      CXCommentParamPassDirection_Out,
      CXCommentParamPassDirection_InOut)
   with Convention => C;  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:191

   function clang_Comment_getKind (Comment : CXComment) return CXCommentKind  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:213
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Comment_getKind";

   function clang_Comment_getNumChildren (Comment : CXComment) return unsigned  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:220
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Comment_getNumChildren";

   function clang_Comment_getChild (Comment : CXComment; ChildIdx : unsigned) return CXComment  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:230
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Comment_getChild";

   function clang_Comment_isWhitespace (Comment : CXComment) return unsigned  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:241
   with Import => True, 
        Convention => C, 
        External_Name => "clang_Comment_isWhitespace";

   function clang_InlineContentComment_hasTrailingNewline (Comment : CXComment) return unsigned  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:249
   with Import => True, 
        Convention => C, 
        External_Name => "clang_InlineContentComment_hasTrailingNewline";

   function clang_TextComment_getText (Comment : CXComment) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:256
   with Import => True, 
        Convention => C, 
        External_Name => "clang_TextComment_getText";

   function clang_InlineCommandComment_getCommandName (Comment : CXComment) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:264
   with Import => True, 
        Convention => C, 
        External_Name => "clang_InlineCommandComment_getCommandName";

   function clang_InlineCommandComment_getRenderKind (Comment : CXComment) return CXCommentInlineCommandRenderKind  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:273
   with Import => True, 
        Convention => C, 
        External_Name => "clang_InlineCommandComment_getRenderKind";

   function clang_InlineCommandComment_getNumArgs (Comment : CXComment) return unsigned  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:281
   with Import => True, 
        Convention => C, 
        External_Name => "clang_InlineCommandComment_getNumArgs";

   function clang_InlineCommandComment_getArgText (Comment : CXComment; ArgIdx : unsigned) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:291
   with Import => True, 
        Convention => C, 
        External_Name => "clang_InlineCommandComment_getArgText";

   function clang_HTMLTagComment_getTagName (Comment : CXComment) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:300
   with Import => True, 
        Convention => C, 
        External_Name => "clang_HTMLTagComment_getTagName";

   function clang_HTMLStartTagComment_isSelfClosing (Comment : CXComment) return unsigned  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:308
   with Import => True, 
        Convention => C, 
        External_Name => "clang_HTMLStartTagComment_isSelfClosing";

   function clang_HTMLStartTag_getNumAttrs (Comment : CXComment) return unsigned  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:315
   with Import => True, 
        Convention => C, 
        External_Name => "clang_HTMLStartTag_getNumAttrs";

   function clang_HTMLStartTag_getAttrName (Comment : CXComment; AttrIdx : unsigned) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:325
   with Import => True, 
        Convention => C, 
        External_Name => "clang_HTMLStartTag_getAttrName";

   function clang_HTMLStartTag_getAttrValue (Comment : CXComment; AttrIdx : unsigned) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:335
   with Import => True, 
        Convention => C, 
        External_Name => "clang_HTMLStartTag_getAttrValue";

   function clang_BlockCommandComment_getCommandName (Comment : CXComment) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:343
   with Import => True, 
        Convention => C, 
        External_Name => "clang_BlockCommandComment_getCommandName";

   function clang_BlockCommandComment_getNumArgs (Comment : CXComment) return unsigned  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:351
   with Import => True, 
        Convention => C, 
        External_Name => "clang_BlockCommandComment_getNumArgs";

   function clang_BlockCommandComment_getArgText (Comment : CXComment; ArgIdx : unsigned) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:361
   with Import => True, 
        Convention => C, 
        External_Name => "clang_BlockCommandComment_getArgText";

   function clang_BlockCommandComment_getParagraph (Comment : CXComment) return CXComment  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:371
   with Import => True, 
        Convention => C, 
        External_Name => "clang_BlockCommandComment_getParagraph";

   function clang_ParamCommandComment_getParamName (Comment : CXComment) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:379
   with Import => True, 
        Convention => C, 
        External_Name => "clang_ParamCommandComment_getParamName";

   function clang_ParamCommandComment_isParamIndexValid (Comment : CXComment) return unsigned  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:389
   with Import => True, 
        Convention => C, 
        External_Name => "clang_ParamCommandComment_isParamIndexValid";

   function clang_ParamCommandComment_getParamIndex (Comment : CXComment) return unsigned  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:397
   with Import => True, 
        Convention => C, 
        External_Name => "clang_ParamCommandComment_getParamIndex";

   function clang_ParamCommandComment_isDirectionExplicit (Comment : CXComment) return unsigned  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:406
   with Import => True, 
        Convention => C, 
        External_Name => "clang_ParamCommandComment_isDirectionExplicit";

   function clang_ParamCommandComment_getDirection (Comment : CXComment) return CXCommentParamPassDirection  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:414
   with Import => True, 
        Convention => C, 
        External_Name => "clang_ParamCommandComment_getDirection";

   function clang_TParamCommandComment_getParamName (Comment : CXComment) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:423
   with Import => True, 
        Convention => C, 
        External_Name => "clang_TParamCommandComment_getParamName";

   function clang_TParamCommandComment_isParamPositionValid (Comment : CXComment) return unsigned  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:435
   with Import => True, 
        Convention => C, 
        External_Name => "clang_TParamCommandComment_isParamPositionValid";

   function clang_TParamCommandComment_getDepth (Comment : CXComment) return unsigned  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:451
   with Import => True, 
        Convention => C, 
        External_Name => "clang_TParamCommandComment_getDepth";

   function clang_TParamCommandComment_getIndex (Comment : CXComment; Depth : unsigned) return unsigned  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:472
   with Import => True, 
        Convention => C, 
        External_Name => "clang_TParamCommandComment_getIndex";

   function clang_VerbatimBlockLineComment_getText (Comment : CXComment) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:480
   with Import => True, 
        Convention => C, 
        External_Name => "clang_VerbatimBlockLineComment_getText";

   function clang_VerbatimLineComment_getText (Comment : CXComment) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:487
   with Import => True, 
        Convention => C, 
        External_Name => "clang_VerbatimLineComment_getText";

   function clang_HTMLTagComment_getAsString (Comment : CXComment) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:497
   with Import => True, 
        Convention => C, 
        External_Name => "clang_HTMLTagComment_getAsString";

   function clang_FullComment_getAsHTML (Comment : CXComment) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:530
   with Import => True, 
        Convention => C, 
        External_Name => "clang_FullComment_getAsHTML";

   function clang_FullComment_getAsXML (Comment : CXComment) return clang_c_CXString_h.CXString  -- /export/work/reznik/ancr/src/gps/libclang/cfe-7.0.0.src/include/clang-c/Documentation.h:542
   with Import => True, 
        Convention => C, 
        External_Name => "clang_FullComment_getAsXML";

end clang_c_Documentation_h;
