#include <windows.h>
#include <commdlg.h>
#include <stdlib.h>
#include <stdio.h>

/*
 * NativeWin32FileSelection
 *
 * opens a common file open dialog
 *
 * returns the selected file
 * user shall free the result
 */

static int
FileSelectionHook
  (HWND   hdlg,
   UINT   uiMsg,
   WPARAM wParam,
   LPARAM lParam)
{
  POINT* lpPos;
  RECT   rect;

  switch (uiMsg)
    {
      case WM_INITDIALOG:
        /* move the window to the proper position */

        lpPos = (POINT*) (((OPENFILENAME*) lParam)->lCustData);
        GetWindowRect ( GetParent (hdlg), &rect);
        lpPos->x -= (rect.right - rect.left) / 2;
        lpPos->y -= (rect.bottom - rect.top) / 2;
        SetWindowPos (GetParent (hdlg), HWND_TOP, 
                      lpPos->x, 
                      lpPos->y, 
                      0, 0, SWP_NOSIZE);
        return 0;

      default:
    }

  return 0;
}


static char*
NativeWin32FileSelection
  (const char*  title,
   const char*  basedir,
   const char*  filepattern,
   const char*  patternname,
   const char*  defaultname,
   unsigned int style)
{
  static OPENFILENAME ofn;
  static char         l_Filter [512];
  static char         l_Result [MAX_PATH];
  int                 l_index = 0;
  char*               res;

  static POINT        position;
  RECT                aw_rect;
  unsigned int        style_flag;
  HWND                active_window;

  switch (style)
    {
      case 2:
        /* position under mouse */

        GetCursorPos (&position);
        style_flag = OFN_ENABLEHOOK;
        break;

      case 1:
        /* position center of the active window */

        active_window = GetActiveWindow ();

        if (active_window)
          {
            GetWindowRect (active_window, &aw_rect);
            position.x = (aw_rect.left + aw_rect.right) / 2;
            position.y = (aw_rect.top + aw_rect.bottom) / 2;
            style_flag = OFN_ENABLEHOOK;
            break;
          }

        /* otherwise, default case */

      case 0:
      default:
        /* default position */

        style_flag = 0;
        break ;
    }

  if (patternname != 0)
    {
      l_index += strlen (patternname);
      strcpy (l_Filter, patternname);
      ++l_index;
    }

  if (filepattern != 0)
    {
      strcat (& (l_Filter [l_index]), filepattern);
      l_index += strlen (filepattern);
      l_Filter [l_index+1] = '\0';
    }

  if (defaultname)
    strcpy (l_Result, defaultname);
  else
    l_Result [0] = '\0';

  ofn.lStructSize       = sizeof (OPENFILENAME);
  ofn.hwndOwner         = NULL;
  ofn.hInstance         = NULL;
  ofn.lpstrFilter       = l_Filter;
  ofn.lpstrCustomFilter = NULL;
  ofn.nMaxCustFilter    = 0;
  ofn.nFilterIndex      = 0;
  ofn.lpstrFile         = l_Result;
  ofn.nMaxFile          = MAX_PATH;
  ofn.lpstrFileTitle    = NULL;
  ofn.nMaxFileTitle     = 0;
  ofn.lpstrInitialDir   = basedir;
  ofn.lpstrTitle        = title;
  ofn.Flags             = OFN_CREATEPROMPT | OFN_EXPLORER | style_flag;
  ofn.nFileOffset       = 0;
  ofn.nFileExtension    = 0;
  ofn.lpstrDefExt       = "ads";
  ofn.lCustData         = &position;
  ofn.lpfnHook          = FileSelectionHook;
  ofn.lpTemplateName    = NULL;

  GetOpenFileName (&ofn);

  /* copy the result into a well sized string */

  res = malloc (sizeof (char) * (strlen (l_Result) + 1));
  strcpy (res, l_Result);
  return res;
}

/*
 *style :
 *    0 : nothing
 *    1 : center
 *    2 : under mouse
 */

char*
NativeFileSelection
  (const char*  title,
   const char*  basedir,
   const char*  filepattern,
   const char*  patternname,
   const char*  defaultname,
   unsigned int style)
{
  return NativeWin32FileSelection
    (title, basedir, filepattern, patternname, defaultname, style);
}
