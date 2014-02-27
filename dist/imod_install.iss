[Setup]
AppPublisher=BL3DEMC, University of Colorado
AppPublisherURL=http://bio3d.colorado.edu/imod/
AppSupportURL=http://bio3d.colorado.edu/imod/
UsePreviousAppDir=no
;Turns off user's ability to change install directory
DisableDirPage=yes
AppName=IMOD
#ifdef Win64
#ifdef Cuda
AppVerName=IMOD version {#ImodVersion} for win64 with CUDA{#Cuda}
#else
AppVerName=IMOD version {#ImodVersion} for win64
#endif
#else
AppVerName=IMOD version {#ImodVersion}
#endif
DefaultDirName={code:getAppDir}
ChangesEnvironment=yes
AlwaysShowDirOnReadyPage=yes
CreateUninstallRegKey=no
Uninstallable=no
UpdateUninstallLogAppName=no
PrivilegesRequired=none
;Function calls do not work with OutputBaseFilename because it is used for packaging the installable.
#ifdef Win64
#ifdef Cuda
OutputBaseFilename=imod_{#ImodVersion}_win64_CUDA{#Cuda}
#else
OutputBaseFilename=imod_{#ImodVersion}_win64
#endif
#else
OutputBaseFilename=imod_{#ImodVersion}_win
#endif


[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"


[Files]
;Must be hard coded.
Source: installIMOD; DestDir: "{app}"; Flags: deleteafterinstall ignoreversion
#ifdef Win64
#ifdef Cuda
Source: "imod_{#ImodVersion}_win64_CUDA{#Cuda}.tar.gz"; DestDir: "{app}"; Flags: deleteafterinstall ignoreversion; Afterinstall: installIMOD
#else
Source: "imod_{#ImodVersion}_win64.tar.gz"; DestDir: "{app}"; Flags: deleteafterinstall ignoreversion; Afterinstall: installIMOD
#endif
#else
Source: "imod_{#ImodVersion}_win.tar.gz"; DestDir: "{app}"; Flags: deleteafterinstall ignoreversion; Afterinstall: installIMOD
#endif


[Registry]
;All installations
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: string; ValueName: "IMOD_DIR"; ValueData: "{app}\IMOD"; Check: (not isFailed) and IsAdminLoggedOn
Root: HKCU; Subkey: "Environment"; ValueType: string; ValueName: "HOME"; ValueData: "{code:getHome}"; Check: not isFailed
;Cygwin installations
Root: HKCU; Subkey: "Environment"; ValueType: string; ValueName: "IMOD_DIR"; ValueData: "{app}\IMOD"; Check: (not isFailed) and isCygwin and (not IsAdminLoggedOn)
;Windows-only installations - admin privledges only
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: string; ValueName: "IMOD_PLUGIN_DIR"; ValueData: "{app}\IMOD\lib\imodplug"; Check: (not isFailed) and (not isCygwin) and IsAdminLoggedOn
Root: HKLM; Subkey: "SYSTEM\CurrentControlSet\Control\Session Manager\Environment"; ValueType: string; ValueName: "IMOD_CALIB_DIR"; ValueData: "C:\ProgramData"; Check: (not isFailed) and (not isCygwin) and IsAdminLoggedOn and DirExists('C:\ProgramData')
;Cygwin installations - the combination of single user and Windows-only does not work.
Root: HKCU; Subkey: "Environment"; ValueType: string; ValueName: "IMOD_DIR"; ValueData: "{app}\IMOD"; Check: (not isFailed) and isCygwin and (not IsAdminLoggedOn)


[Run]
Filename: "{cmd}"; Parameters: "/C move {code:getQuotedAppDir}\installIMOD.log {code:getQuotedAppDir}\IMOD"; WorkingDir: "{app}"; StatusMsg: "Moving installIMOD.log to IMOD..."; Check: (not isFailed)


[Code]
var
  Failed: Boolean;
  Cygwin: Boolean;
  Psutil: Boolean;
  BadCygwin: Boolean;
  AppDir: String;
  CygwinDir: String;
  PythonDir: String;
  
function setupCygwin(const rootKey: Integer; const cygwinKeyName: String; const pathName: String): Boolean;
//If a installation of Cygwin that contains Python is found, set global variables and return true.
var
  path: String;
  bin: String;
begin
  Result := False;
  if RegQueryStringValue(rootKey, cygwinKeyName, pathName, path) and (path <> '') then begin
    path := trim(path);
    if DirExists(path) then begin
      bin := AddBackslash(path) + 'bin';
      if DirExists(bin) then begin
        if fileExists(bin + '\python.exe') then begin
          AppDir := AddBackslash(path) + 'usr\local';
          CygwinDir := path;
          PythonDir := bin;
          Cygwin := true;
          Result := True;
          BadCygwin := False;
        end else begin
          BadCygwin := True;
        end;
      end;
    end;
  end;
end;

function setupWindows(const rootKey: Integer): Boolean;
//If a compatible version of Python is found, set the global variables and return true.
var
  versionArray: TArrayOfString;
  version: String;
  index: Integer;
  path: String;
begin
  Result := False;
  Psutil := False;
  if RegGetSubkeyNames(rootKey, 'Software\Python\PythonCore', versionArray) then begin
    for index := 0 to GetArrayLength(versionArray)-1 do begin
      version := versionArray[index];
      //Take either the first valid python with psutil or, if none of them have psutil,
      //will take the last valid one.
      if RegQueryStringValue(rootKey, 'Software\Python\PythonCore\' + version + '\InstallPath',
                       '', path) then begin
        if DirExists(path) and fileExists(AddBackslash(path) + 'python.exe') then begin
          AppDir := 'C:\Program Files'
          CygwinDir := '';
          PythonDir := path;
          Cygwin := False;
          Result := True;
          if DirExists(AddBackslash(path) + 'Lib\site-packages\psutil') then begin
            Psutil := True;
            break;
          end;
        end;
      end;
    end;
  end;
end;

function InitializeSetup(): Boolean;
//Initializes the global variables.
//Check 64-bit status, checks whether the right packages are installed, and sets the global variables.
//Return false if 64-bit status is wrong, or the right packages are not installed.
//If false is returned, the installer terminates.
begin
  Failed := False;
  BadCygwin := False;
  Result := True;
#ifdef Win64
  //Make sure this computer is a 64-bit computer.
  if (not isWin64()) then begin
    MsgBox('Not a 64-bit computer.  Unable to install', mbCriticalError, MB_OK);
    Result := False;
  end else begin
#endif
    //find out if Cygwin is installed
    if not setupCygwin(HKEY_LOCAL_MACHINE_64, 'SOFTWARE\Cygwin\setup', 'rootdir') then begin
      if not setupCygwin(HKEY_LOCAL_MACHINE_32, 'SOFTWARE\Cygwin\setup', 'rootdir') then begin
        if not setupCygwin(HKEY_CURRENT_USER_64, 'SOFTWARE\Cygwin\setup', 'rootdir') then begin  
          if not setupCygwin(HKEY_CURRENT_USER_32, 'SOFTWARE\Cygwin\setup', 'rootdir') then begin
            if not setupCygwin(HKEY_LOCAL_MACHINE_32, 'SOFTWARE\Cygnus Solutions\Cygwin\mounts v2\/', 'native') then begin                 
              if not setupCygwin(HKEY_LOCAL_MACHINE_64, 'SOFTWARE\Cygnus Solutions\Cygwin\mounts v2\/', 'native') then begin
                if not setupCygwin(HKEY_CURRENT_USER_32, 'SOFTWARE\Cygnus Solutions\Cygwin\mounts v2\/', 'native') then begin
                  if not setupCygwin(HKEY_CURRENT_USER_64, 'SOFTWARE\Cygnus Solutions\Cygwin\mounts v2\/', 'native') then begin
                    //find out if Windows Python is installed.  A single-user Windows-only install will not work because a non-admin
                    //account cannot write to Program Files.
                    if not setupWindows(HKEY_LOCAL_MACHINE_32) then begin
                      if not setupWindows(HKEY_LOCAL_MACHINE_64) then begin
                        MsgBox('A compatible Python version has not been installed on this computer.  Unable to install.', mbCriticalError, MB_OK);
                        Result := False;
                      end;
                    end;                               
                  end;              
                end;                    
              end; 
            end;
          end;
        end;
      end;
    end;
#ifdef Win64
  end;
#endif
  //If Cygwin is installed but does not not have Python installed, and there is a valid Windows Python installed,
  //ask the user before going with the Windows-only installation.
  if BadCygwin and (AppDir <> '') and (MsgBox('There is no Python installed in Cygwin.  ' +
                    'If you proceed with this'#13#10'intallation, ' +
                    'some IMOD programs will not be runnable from Cygwin terminals.'#13#10#13#10
                    'To use IMOD with a Cygwin terminal, ' +
                    'exit this installer and use the Cygwin installer to intall Python.'#13#10#13#10
                    'Exit?', mbConfirmation, MB_YESNO) = IDYES) then begin
    MsgBox('IMOD had not been installed.  Exiting installer.', mbInformation, MB_OK);
    Result := False;
  end;
end;

function getAppDir(const dummy: String): String;
begin
  Result := AppDir;
end;

function getQuotedAppDir(const dummy: String): String;
begin
  Result := '"' + AppDir + '"';
end;

function isFailed(): Boolean;
begin
  Result := Failed;
end;

function isCygwin(): Boolean;
begin
  Result := Cygwin;
end;

function getPythonDir(const dummy: String): String;
begin
  Result := PythonDir;
end;

function getCygwinDir(const dummy: String): String;
begin
  Result := CygwinDir;
end;

procedure installIMOD();
//Run installIMOD.
var
  command: String;
  outputBaseFilename: String;
  skip: String;
  returnCode: Integer;
  //Runs the installIMOD script.  Sets Failed if installIMOD failed.
begin
  if Cygwin then begin
    skip := '';
  end else begin
    skip := '-skip ';
  end;
#ifdef Win64
#ifdef Cuda
  outputBaseFilename := 'imod_{#ImodVersion}_win64_CUDA{#Cuda}'
#else
  outputBaseFilename := 'imod_{#ImodVersion}_win64'
#endif
#else
  outputBaseFilename := 'imod_{#ImodVersion}_win'
#endif
  command := '/C PATH=' + PythonDir + ';%PATH% && echo Installing IMOD.......... && python installIMOD -yes ' + skip + outputBaseFilename + '.tar.gz > installIMOD.log 2>&1';
  if Exec(ExpandConstant('{cmd}'), command, ExpandConstant('{app}'), SW_SHOW, ewWaitUntilTerminated, returnCode) then begin
    if returnCode <> 0 then begin
      Failed := True;
      MsgBox('IMOD Install has failed!.  IMOD will not be installed (ignore messages to the contrary from this ' +
             'installer).  Error messages are in installIMOD.log in ' + ExpandConstant('{app}') +
             '.  Please see http://bio3d.colorado.edu/imod for alternatives.  Working directory=' +
             ExpandConstant('{app}') + ',command=' + command + ',returnCode=' + IntToStr(returnCode), mbCriticalError, MB_OK);
    end;
  end else begin
    Failed := True;
    MsgBox('IMOD Install has failed!.  IMOD will not be installed (ignore messages to the contrary from this ' +
           'installer).  Please see http://bio3d.colorado.edu/imod for alternatives.  Working directory=' +
           ExpandConstant('{app}') + ',command=' + command + ',returnCode=' + IntToStr(returnCode), mbCriticalError, MB_OK);
  end;
end;

function getHome(const dummy: String): String;
//Return the value for the HOME variable.
var
  version: TWindowsVersion;
begin
  if Cygwin then begin
    //Cygwin
    Result := ExpandConstant(CygwinDir + '\home\{username}');
  end else begin
    GetWindowsVersionEx(version);
    if version.Major >= 6 then begin
      //Vista and on
      Result := ExpandConstant('C:\Users\{username}');
    end else begin
      //XP and earlier
      Result := ExpandConstant('C:\Documents and Settings\{username}');
    end;
  end;
end;

procedure addToSysPath(const path: String; var sysPath: String);
//Add a path to the PATH environment variable value.
var
  addPath: Boolean;
  index: Integer;
begin
  addPath := True;
  //Don't add the path if it already exists
  index := Pos(path, sysPath);
  if index <> 0 then begin
    index := index + Length(path) + 1;
    if (index <> Length(sysPath)) and (sysPath[index] <> ';') then begin
      addPath := False;
    end;
  end;
  if addPath then begin
    sysPath := path + ';' + sysPath;
  end;
end;

procedure CurStepChanged(const CurStep: TSetupStep);
var
  sysPath: String;
  hkey: Integer;
	subkey: String;
begin
	if CurStep = ssPostInstall then begin
    //Modify the PATH environment variable.
	  if not Failed then begin
      if not IsAdminLoggedOn() then begin
        hkey := HKEY_CURRENT_USER;
        subkey := 'Environment';
      end else begin
        hkey := HKEY_LOCAL_MACHINE;
        subkey := 'SYSTEM\CurrentControlSet\Control\Session Manager\Environment';
	    end;
      RegQueryStringValue(hkey, subkey, 'Path', sysPath);
  	  sysPath := Trim(sysPath);
		  addToSysPath(AppDir + '\IMOD\bin', sysPath);
      if Cygwin then begin
        addToSysPath(CygwinDir + '\bin', sysPath);
      end else begin
        addToSysPath(PythonDir, sysPath);
      end;
      RegWriteStringValue(hkey, subkey, 'Path', sysPath);
      if (not Cygwin) and (not Psutil) then begin
        //warn about missing psutil
        MsgBox('WARNING: the module psutil does not appear to be installed in ' + PythonDir,
               mbInformation, MB_OK);
      end;
    end;
  end;
end;
