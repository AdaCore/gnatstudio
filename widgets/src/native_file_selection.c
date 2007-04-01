/*********************************************************************
 *                               G P S                               *
 *                                                                   *
 *                      Copyright (C) 2002-2007                      *
 *                              AdaCore                              *
 *                                                                   *
 * GPS is free  software;  you can redistribute it and/or modify  it *
 * under the terms of the GNU General Public License as published by *
 * the Free Software Foundation; either version 2 of the License, or *
 * (at your option) any later version.                               *
 *                                                                   *
 * This program is  distributed in the hope that it will be  useful, *
 * but  WITHOUT ANY WARRANTY;  without even the  implied warranty of *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *
 * General Public License for more details. You should have received *
 * a copy of the GNU General Public License along with this program; *
 * if not,  write to the  Free Software Foundation, Inc.,  59 Temple *
 * Place - Suite 330, Boston, MA 02111-1307, USA.                    *
 *********************************************************************/

/*
 * Native file selection dialogs
 */

#ifdef _WIN32

#include <windows.h>
#include <commdlg.h>
#include <shlobj.h>
#include <stdlib.h>
#include <stdio.h>

/*
 * NativeFileSelection
 *
 * open a common file open dialog
 *
 * return the selected file
 * user shall free the result
*/

#define OPEN_FILE 0
#define SAVE_FILE 1
#define PATTERN_SEP ';'

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
        SetWindowPos (GetParent (hdlg), HWND_TOP,
                      lpPos->x,
                      lpPos->y,
                      0, 0, SWP_NOSIZE);
        return 0;
	break;

      default:
	break;
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
   int          style,
   int          kind)
{
  static OPENFILENAME ofn;
  static char         l_Filter [512];
  static char         l_Result [MAX_PATH];
  int                 l_index = 0;
  char*               res;
  BOOL                ret;

  static POINT        position;
  RECT                aw_rect;
  unsigned int        style_flag;
  HWND                active_window = GetActiveWindow ();

  switch (style)
    {
      case 2:
        /* position under mouse */

        GetCursorPos (&position);
        style_flag = OFN_ENABLEHOOK;
        break;

      case 1:
        /* position center of the active window */

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

  {
    int pi = 0, fi = 0;
    int name_found;
    int start;

    while (filepattern [fi] != '\0')
      {
	/* Copy one pattern name */

	name_found = 0;

	while (patternname [pi] != '\0' && patternname [pi] != PATTERN_SEP)
	  {
	    name_found = 1;
	    l_Filter [l_index++] = patternname [pi++];
	  }

	if (name_found)
	  l_Filter [l_index++] = '\0';

	/* Copy the corresponding file pattern */

	start = l_index;

	while (filepattern [fi] != '\0' && filepattern [fi] != PATTERN_SEP)
	  {
	    if (filepattern [fi] == ',')
	      l_Filter [l_index++] = ';';
	    else if (filepattern [fi] != '{' && filepattern [fi] != '}')
	      l_Filter [l_index++] = filepattern [fi];
	    fi++;
	  }
	l_Filter [l_index++] = '\0';

	/* Copy again the file pattern if name was not found */

	if (!name_found)
	  {
	    int k, stop;

	    stop = l_index;

	    for (k=start; k<stop; k++)
	      l_Filter  [l_index++] = l_Filter [k];
	  }

	if (patternname [pi] == PATTERN_SEP) pi++;
	if (filepattern [fi] == PATTERN_SEP) fi++;
      }

    /* Then append the final \0 */

    l_Filter [l_index++] = '\0';
  }

  if (defaultname)
    strcpy (l_Result, defaultname);
  else
    l_Result [0] = '\0';

  ZeroMemory(&ofn, sizeof(ofn));

  ofn.lStructSize       = sizeof (OPENFILENAME);
  ofn.hwndOwner         = active_window;
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
  ofn.nFileOffset       = 0;
  ofn.nFileExtension    = 0;
  ofn.lpstrDefExt       = NULL;
  ofn.lCustData         = (LPARAM) &position;
  ofn.lpfnHook          = (LPOFNHOOKPROC) FileSelectionHook;
  ofn.lpTemplateName    = NULL;

  if (kind == OPEN_FILE)
    {
      ofn.Flags = OFN_HIDEREADONLY | OFN_EXPLORER |
                  OFN_FILEMUSTEXIST | OFN_PATHMUSTEXIST | style_flag;
      ret = GetOpenFileName (&ofn);
    }
  else
    {
      /* ??? Would be nice to take advantage of READONLY and CREATEPROMPT */
      ofn.Flags = OFN_HIDEREADONLY | OFN_EXPLORER | style_flag;
      ret = GetSaveFileName (&ofn);
    }

  if (!ret)
    l_Result [0] = '\0';

  /* copy the result into a well sized string */

  res = malloc (sizeof (char) * (strlen (l_Result) + 1));
  strcpy (res, l_Result);
  return res;
}

/*
 * style:
 *    0 : nothing
 *    1 : center
 *    2 : under mouse
 *
 * kind:
 *    0 : open
 *    1 : save
 *    2 : unspecified
 */

char*
NativeFileSelection
  (const char*  title,
   const char*  basedir,
   const char*  filepattern,
   const char*  patternname,
   const char*  defaultname,
   int          style,
   int          kind)
{
  return NativeWin32FileSelection
    (title, basedir, filepattern, patternname, defaultname, 0, kind);
}

#ifndef BFFM_SETEXPANDED
#define BFFM_SETEXPANDED (WM_USER + 106)
#endif

static int CALLBACK
BrowseHook
  (HWND hwnd,
   UINT uMsg,
   LPARAM lParam,
   LPARAM lpData)
{
  switch (uMsg)
    {
      case BFFM_INITIALIZED:
	SendMessage (hwnd, BFFM_SETEXPANDED, TRUE, lpData);
	break;

      default:
        break;
    }
}

char*
NativeDirSelection
  (const char* title,
   const char* basedir)
{
  TCHAR displayName [MAX_PATH];
  TCHAR path [MAX_PATH];
  static WCHAR wbasedir [MAX_PATH];
  BROWSEINFO BI = { 0 };
  LPITEMIDLIST pIdList;
  IShellFolder *psf = NULL;
  char *res;

  MultiByteToWideChar (CP_UTF8, 0, basedir, -1, wbasedir, MAX_PATH);

  BI.hwndOwner = GetActiveWindow ();
  BI.lpszTitle = title;
  BI.pszDisplayName = displayName;
  BI.ulFlags = BIF_USENEWUI;
  BI.lpfn = (BFFCALLBACK) BrowseHook;
  BI.lParam = (LPARAM) wbasedir;

  pIdList = SHBrowseForFolder (&BI);

  if (pIdList != 0)
    {
      IMalloc *imalloc = 0;

      if (! SHGetPathFromIDList (pIdList, path))
        path [0] = '\0';

#if 0   /* ??? Does not compile with mingw */
      if (SUCCEEDED (SHGetMalloc (&imalloc)))
        {
          imalloc->Free (pIdList);
          imalloc->Release ();
        }
#endif
    }
  else
    path [0] = '\0';

  res = malloc (sizeof (char) * (strlen (path) + 1));
  strcpy (res, path);
  return res;
}

int
NativeFileSelectionSupported ()
{
  return 1;
}

#else

char*
NativeFileSelection
  (const char*  title,
   const char*  basedir,
   const char*  filepattern,
   const char*  patternname,
   const char*  defaultname,
   int          style,
   int          kind)
{
  return (char *)0;
}

char*
NativeDirSelection
  (const char* title,
   const char* basedir)
{
  return (char *)0;
}

int
NativeFileSelectionSupported ()
{
  return 0;
}

#endif
