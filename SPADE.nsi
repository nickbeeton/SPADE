; SPADE.nsi

;--------------------------------

; The name of the installer
Name "SPADE"

; The file to write
OutFile "SPADE v0.1.1.exe"

LicenseText "License page"
LicenseData "license.txt"

; The default installation directory
InstallDir $PROGRAMFILES64\R\R-3.0.3

; Request application privileges for Windows Vista
RequestExecutionLevel user

;--------------------------------

; Pages

Page license
Page components
Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

;--------------------------------

; The stuff to install
Section "SPADE v0.1.1 (required)" ;No components page, name is not important

  SectionIn RO

  ; Set output path to the installation directory.
  SetOutPath $INSTDIR

  ; *** NOTE: OVERWRITES CURRENT USER PATH VARIABLE!!! ***
  ExecWait 'setx.exe path "$INSTDIR\gtk32\bin;$INSTDIR\gtk64\bin"'
  ; Run it twice to make sure it sticks (this is surprisingly necessary)
  ExecWait 'setx.exe path "$INSTDIR\gtk32\bin;$INSTDIR\gtk64\bin"'

  ; Put files there
  File /r *

  FileOpen $9 $INSTDIR\SPADE\SPADE.bat w
  FileWrite $9 'cd "$INSTDIR\SPADE"$\r$\n'
  FileWrite $9 '"$INSTDIR\bin\x64\Rgui.exe"$\r$\n'
  FileClose $9

  WriteUninstaller "uninstall.exe"

SectionEnd ; end the section


Section "Start Menu Shortcut"

  CreateDirectory "$SMPROGRAMS\SPADE"
  CreateShortCut "$SMPROGRAMS\SPADE\SPADE.lnk" "$INSTDIR\SPADE\SPADE.bat"
  CreateShortCut "$SMPROGRAMS\SPADE\Uninstall.lnk" "$INSTDIR\uninstall.exe"
  
SectionEnd


Section "Desktop Shortcut"

  CreateShortCut "$DESKTOP\SPADE.lnk" "$INSTDIR\SPADE\SPADE.bat"
  
SectionEnd


Section "Uninstall"

  RMDir /r /REBOOTOK $INSTDIR

SectionEnd