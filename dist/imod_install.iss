[Setup]
AppPublisher=BL3DEMC, University of Colorado
AppPublisherURL=http://bio3d.colorado.edu/imod/
AppSupportURL=http://bio3d.colorado.edu/imod/
UsePreviousAppDir=no
DisableDirPage=yes
AppName=IMOD
AppVerName=IMOD version 4.5.7 for Win64
DefaultDirName={code:CreateRootDir}\usr\local
ChangesEnvironment=yes
AlwaysShowDirOnReadyPage=yes
CreateUninstallRegKey=no
Uninstallable=no
UpdateUninstallLogAppName=no
PrivilegesRequired=none
OutputBaseFilename=imod_4.5.7_win64

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Files]
Source: "imod_4.5.7_win64.csh"; DestDir: "{app}"; Flags: deleteafterinstall ignoreversion; Afterinstall: RunImodInstaller

[Registry]
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: string; ValueName: "IMOD_DIR"; ValueData: "{app}\IMOD"; Check: checkOKMultiUser
Root: HKCU; Subkey: "Environment"; ValueType: string; ValueName: "IMOD_DIR"; ValueData: "{app}\IMOD"; Check: checkOKSingleUser
Root: HKCU; Subkey: "Environment"; ValueType: string; ValueName: "HOME"; ValueData: "{code:getRootDir}\home\{username}"; Check: checkOK

[Run]
Filename: "{cmd}"; Parameters: "/C PATH={code:getRootDir}\bin;%PATH% && rm -fr {app}\IMODtempDir"; WorkingDir: "{app}"; StatusMsg: "Removing IMODtempDir if necessary..."

[Code]
var
  Failed: Boolean;
  RootDir:  String;
  SingleUser: Boolean;
  
function checkOK(): Boolean;
begin
  Result := not Failed;
end;

function checkOKMultiUser(): Boolean;
begin
  Result := (not Failed) and (not SingleUser) and (IsAdminLoggedOn());
end;

function isSingleUser(): Boolean;
begin
  if SingleUser then
    Result := True
  else
    Result := not IsAdminLoggedOn();
end;

function checkOKSingleUser(): Boolean;
begin
  if Failed then
    Result := False
  else
    Result := isSingleUser();
end;

function getRootDir(Param: String): String;
begin
  Result := RootDir;
end;

procedure RunImodInstaller();
//Runs the IMOD .csh installed and sets Failed to true if it fails
var
  command: String;
  parameter: String;
  workingDir: String;
  returnCode: Integer;
begin
  //Run installer
  command := ExpandConstant('{cmd}');
  parameter := ExpandConstant('/C PATH={code:GetRootDir}\bin;%PATH% && tcsh -f imod_4.5.7_win64.csh -yes');
  workingDir := ExpandConstant('{app}');
  if Exec(command, parameter, workingDir, SW_SHOW, ewWaitUntilTerminated, returnCode) then
  begin
    if returnCode <> 0 then
    begin
      Failed := True;
      MsgBox('IMOD Install has failed!.  IMOD will not be installed ' +
                     '(ignore messages to the contrary from this ' +
                     'installer).  Please see http://bio3d.colorado.edu/imod for ' +
                     'alternatives.  command=' + command + ',parameter=' +
                     parameter + ',workingDir=' + workingDir + ',returnCode=' +
                     IntToStr(returnCode), mbCriticalError, MB_OK);
    end;
  end
  else
  begin
    Failed := True;
    MsgBox('IMOD Install has failed!.  IMOD will not be installed ' +
                     '(ignore messages to the contrary from this ' +
                     'installer).  Please see http://bio3d.colorado.edu/imod for ' +
                     'alternatives.  command=' + command + ',parameter=' +
                     parameter + ',workingDir=' + workingDir + ',returnCode=' +
                     IntToStr(returnCode), mbCriticalError, MB_OK);
  end;
end;

function setRootDir(cygwin: String): String;
var
  cygwinBin:  String;
begin
  cygwin := Trim(cygwin);
  if DirExists(cygwin) then
  begin
    cygwinBin := AddBackslash(cygwin) + 'bin';
    if DirExists(cygwinBin) then
    begin
      RootDir := cygwin;
      Result := cygwin;
    end
    else
    begin
      MsgBox('Cygwin has not been installed correctly.  Please ' +
             'install Cygwin before installing IMOD.  See ' +
             'http://bio3d.colorado.edu/imod for instructions.  cygwin=' + cygwin +
             ',cygwinBin=' + cygwinBin, mbCriticalError, MB_OK);
      Abort();
    end;
  end
  else
  begin
    MsgBox('Cygwin has not been installed correctly.  Please ' +
           'install Cygwin before installing IMOD.  See ' +
           'http://bio3d.colorado.edu/imod for instructions.  cygwin=' + cygwin, mbCriticalError, MB_OK);
    Abort();
  end;
end;

procedure validateTcsh(root: String);
//Sets Failed to True if tcsh is not in cygwin
begin
  if not fileExists(AddBackslash(root) + '\bin\tcsh.exe') then
    begin
      MsgBox('WARNING:  Your Cygwin does not include tcsh.  IMOD will not install without it.  ' +
      'To install tcsh, you need to run Cygwin Setup (from www.cygwin.com) - select the '+
      '"Keep" radio button in the Select Packages panel, and select tcsh from the Shells package category.',
      mbCriticalError, MB_OK);
      Abort();
    end
end;

function CreateRootDir(Param: String): String;
//Sets RootDir.  Checks for a valid Cygwin.
//Sets Failed and throws an exception if there is a failure.
//Returns the RootDir.
var
  cygwin: String;
  root: String;
begin
  if (not isWin64()) then
    begin
      MsgBox('Not a 64-bit computer.  Unable to install', mbCriticalError, MB_OK);
      Abort();
    end;
  SingleUser := False
  //New Cygwin location
  if RegQueryStringValue(HKEY_LOCAL_MACHINE, 'SOFTWARE\Cygwin\setup',
                         'rootdir', cygwin) and (cygwin <> '') then
     begin
       root := setRootDir(cygwin);
       validateTcsh(root);
       Result := root;
     end
  else
    if RegQueryStringValue(HKEY_CURRENT_USER, 'Software\Cygwin\setup',
                           'rootdir', cygwin) and (cygwin <> '') then
      begin
        SingleUser := True;
        if MsgBox('Cygwin has been installed for this user only.  This means ' +
                  'that IMOD will also be installed for this user only.  Continue?', mbConfirmation, MB_YESNO) = IDNO then
          Abort();
        root := setRootDir(cygwin);
        validateTcsh(root);
        Result := root;
      end
    else
      //Old Cygwin location
      if RegQueryStringValue(HKEY_LOCAL_MACHINE, 'SOFTWARE\Cygnus Solutions\Cygwin\mounts v2\/',
                             'native', cygwin) and (cygwin <> '') then
        begin
          root := setRootDir(cygwin);
          validateTcsh(root);
          Result := root;
        end
      else
        if RegQueryStringValue(HKEY_CURRENT_USER, 'Software\Cygnus Solutions\Cygwin\mounts v2\/',
                               'native', cygwin) and (cygwin <> '') then
          begin
            SingleUser := True;
            if MsgBox('Cygwin has been installed for this user only.  This means ' +
                      'that IMOD will also be installed for this user only.  Continue?', mbConfirmation, MB_YESNO) = IDNO then
              Abort();
            root := setRootDir(cygwin);
            validateTcsh(root);
            Result := root;
          end
        else
          begin
            MsgBox('Cygwin has not been installed correctly.  Please ' +
                   'install Cygwin before installing IMOD.  See ' +
                   'http://bio3d.colorado.edu/imod for instructions.', mbCriticalError, MB_OK);
            Abort();
          end;
end;

procedure addSingleUserImodDir(const environmentVariable: String; const value: String);
begin
  RegWriteStringValue(HKEY_CURRENT_USER, 'Environment', environmentVariable, value);
end;

procedure AddImodDir();
var
environmentVariable: String;
value: String;
begin
  environmentVariable := 'IMOD_DIR';
  value := ExpandConstant('{app}\IMOD');
  if isSingleUser() then
    addSingleUserImodDir(environmentVariable, value)
  else
    RegWriteStringValue(HKEY_LOCAL_MACHINE, 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment', environmentVariable, value);
end;

procedure ModPath();
var
	pathValue:	String;
	newPath:	String;
	prevPosition:	Integer;
	position:	Integer;
	found:	Boolean;
	done:  Boolean;
	p:	Integer;
	hkey: Integer;
	keyPath: String;
begin
  //Modify the Path environment variables - add the Cygwin bin to the front of
  //it, if its not already there.
  //Not supporting uninstalling and not support Windows 9x.
  if (not IsUninstaller()) and (UsingWinNT()) then
  begin
	  newpath := ExpandConstant('{code:GetRootDir}\bin');
	  //Get the current Path value
	  if isSingleUser() then
	  begin
      hkey := HKEY_CURRENT_USER;
      keyPath := 'Environment';
    end
	  else
	  begin
	    hkey := HKEY_LOCAL_MACHINE;
      keyPath := 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment';
	  end;
  	RegQueryStringValue(hkey, keyPath, 'Path', pathValue);
  	pathValue := Trim(pathValue);
  	// Put a ';' on the end of pathValue to make it easier to move through the string.
  	if Copy(pathValue, Length(pathValue), 1) <> ';' then
  	  pathValue := pathValue + ';';
  	position := Pos(';', pathValue);
  	prevPosition := 0;
  	found := False;
  	done := False;
  	//Look at each path in pathValue
  	while ((not found) and (not done)) do
    begin
  		// Check if the current path matches newPath
  		if CompareText(newpath, Copy(pathValue, prevPosition + 1, position - prevPosition - 1)) = 0 then
  			found := True
      else
      begin
  		  //Go to the next path
  		  //Copy doesn't mind if the count parameter is too big.
  		  p := Pos(';', Copy(pathValue, position + 1, Length(pathValue)));
  		  if p = 0 then
  		    done := True
  		  else
        begin
  		    prevPosition := position
  		    // Move to the next positon.
  		    position := position + p;
  		  end;
  		end;
  	end;
  	//Add new directory path to pathValue and write to the registry
  	if not found then
    begin
  	  //Remove trailing ';'
  	  pathValue := Copy(pathValue, 1, Length(pathValue) -1);
  	  //Place the cygwin bin path at the beginning of the path value
  	  if Copy(pathValue, 1, 1) <> ';' then
  	    pathValue := ';' + pathValue;
  		pathValue := newPath + pathValue;
  		RegWriteStringValue(hkey, keyPath, 'Path', pathValue);
  	end;
  end;
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssInstall then
  begin
    Failed := False;
  end
	else if CurStep = ssPostInstall then
    if checkOKMultiUser() then
      AddImodDir();
	  if checkOK() then
		  ModPath()
end;

