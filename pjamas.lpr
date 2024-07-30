program pjamas;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, StrUtils, CustApp,
  Generics.Collections,
  fphttpclient, opensslsockets,
  fpjson, jsonparser,
  paszlib, zipper,
  JSONPrettyPrint
  { you can add units after this };

type

  TDependency = class public
    Name: string;
    CleanName: string;
    Version: string;
    PackagePath: string;

    procedure BuildDownloadURL(var URL, DesiredFileName, DesiredExtension: string); virtual; abstract;
    procedure Install(const Path, InstallDir: string); virtual; abstract;
    function GetPackagePath(var Path: string): boolean; virtual; abstract;
  end;

  TGithubDependency = class (TDependency) public
    constructor Create(aName, aVersion: string);
    procedure BuildDownloadURL(var URL, DesiredFileName, DesiredExtension: string); override;
    procedure Install(const Path, InstallDir: string); override;
    function GetPackagePath(var Path: string): boolean; override;
  end;

  TLockedDependency = record
    Name: string;
    ExactVersion: string;
    LastURL: string;
  end;

  TCompiler = (compilerPas2JS, compilerFPC);

  TDependencyDict = specialize TDictionary<string, string>;
  TDependencyObjectDict = specialize TDictionary<string, TDependency>;

  TPackage = class;

  TPackageQueue = specialize TQueue<TPackage>;

  TDependencyPackageDict = specialize TDictionary<string, TPackage>;

  TPackage = class public
    Name: string;
    Version: string;
    Compiler: TCompiler;
    PackagesDir: string;
    UnitsDirs: TStringList;
    Path: string;
    Filename: string;
    DependencyDict: TDependencyDict;
    DependencyObjects: TDependencyObjectDict;
    DependencyPackages: TDependencyPackageDict;
    UsedBy: TPackage;
    Dependency: TDependency;
    Patches: TDependencyPackageDict;

    constructor Create;

    procedure LoadFromJSON(const JSONString: string);
    procedure LoadFromFile(const ADir, AFilename: string);
    procedure DownloadDependencies;
    procedure RefreshDependenciesObjects;
    procedure MakeUpData(const ADir: string);
    function SaveToJSON(): string;
    procedure SaveToFile(const AFilename: string);
  end;

    //Dependencies: TDependency;
    //LockedDependencies: TLockedDependency;

  TCommandProc = procedure (paramIndex: integer) of object;

  TCommandRec = record
    name: string;
    description: string;
    proc: TCommandProc;
  end;

  TCommandDict = specialize TDictionary<string, TCommandRec>;

  TRedirectHandler = class public
    procedure HandleRedirect(Sender: TObject; const ASrc: string; var Redirection: string);
  end;

  { TPjamasOptions }

  TPjamasOptions = record
    AllowCreate: boolean;
    DoDownloads: boolean;
    DoWritePackage: boolean;
    DoEchoUnits: boolean;
    ForceDownload: boolean;
  end;

  { TPjamasApplication }

  TPjamasApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    commands: TCommandDict;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

    procedure AddCommand(names: TStringArray; description: string; proc: TCommandProc);
    procedure CmdBuild(paramIndex: integer);
    procedure CmdGet(paramIndex: integer);
    procedure CmdInstalled(paramIndex: integer);
    procedure CmdRemove(paramIndex: integer);
    procedure CmdUnits(paramIndex: integer);
    procedure ExecuteCommand(cmd: string; paramIndex: integer);
    procedure LoadRootProject;
    procedure MainRoutine;
  end;


const
  ShortOpts = 'hfc';
  LongOpts : array of string = ('help', 'force-download', 'create');
  Descriptions : array of string = (
    'Display help',
    'Force download of dependencies again',
    'Allow pjamas to create a pjamas.json if one was not found'
  );
  DownloadDirName = 'downloads';
  InstalledDirName = 'installed';
  PjamasFile = 'pjamas.json';
  DefaultPackageDir = 'pjamas-packages';


var
  Application: TPjamasApplication;
  RootPackage: TPackage;
  CurrentPackage: TPackage;
  PackageQueue: TPackageQueue;
  UnitDirs: TStringList;
  CommandParams: TStringArray;
  Options: TPjamasOptions;


procedure TRedirectHandler.HandleRedirect(Sender: TObject; const ASrc: string; var Redirection: string);
begin
  WriteLn('RRR Redirect detected!');
  WriteLn('New Location: ', ASrc);
// If you want to follow the redirection automatically, set Redirection to true
// If you want to handle it manually, set Redirection to false
// Redirection := true;
end;


function DownloadDestination(const DepFileName, DepExt: string): string;
begin
  Result := Format('%s/%s/%s.%s', [
    RootPackage.PackagesDir,
    DownloadDirName,
    DepFileName,
    DepExt
  ]);
end;


function InstallDestination(const DepFileName: string): string;
begin
  Result := Format('%s/%s/%s', [
    RootPackage.PackagesDir,
    InstalledDirName,
    DepFileName
  ]);
end;


procedure DownloadFile(const URL, Destination: string);
var
  HTTPClient: TFPHTTPClient;
  FileStream: TFileStream;
  Header: string;
begin
  writeln('DownloadFile: ', URL);
  HTTPClient := TFPHTTPClient.Create(nil);
  HTTPClient.AllowRedirect := true;
  //HTTPClient.OnRedirect := @RedirectHandler.HandleRedirect;
  try
    FileStream := TFileStream.Create(Destination, fmCreate);
    try
      HTTPClient.Get(URL, FileStream);
      WriteLn('File downloaded successfully to ', Destination);
    finally
      //WriteLn('Response Headers:');
      for Header in HTTPClient.ResponseHeaders do
      begin
        //WriteLn(Header);
      end;
      FileStream.Free;
    end;
  finally
    HTTPClient.Free;
    //RedirectHandler.Free;
  end;
end;


function ReadFileToString(const FileName: string): string;
var
  FileStream: TFileStream = nil;
  StringStream: TStringStream;
  e: Exception;
begin
  try
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    StringStream := TStringStream.Create('');
    try
      StringStream.CopyFrom(FileStream, FileStream.Size);
      Result := StringStream.DataString;
    finally
      StringStream.Free;
    end;
  except
    Result := ''
  end;
  if FileStream <> nil then FileStream.Free;
end;


procedure UnzipFile(const ZipFileName, OutputDir: string);
var
  UnZipper: TUnZipper;
begin
  UnZipper := TUnZipper.Create;
  try
    UnZipper.FileName := ZipFileName;
    UnZipper.OutputPath := OutputDir;
    UnZipper.Examine;
    UnZipper.UnZipAllFiles;
  finally
    UnZipper.Free;
  end;
end;


{ TGithubDependency }


constructor TGithubDependency.Create(aName, aVersion: string);
begin
  Name := aName;
  Version := aVersion;
  CleanName := ReplaceStr(aName, '/', '-');
end;


procedure TGithubDependency.BuildDownloadURL(var URL, DesiredFileName, DesiredExtension: string);
begin
  DesiredFileName := Format('%s@%s', [CleanName, Version]);
  DesiredExtension := 'zip';
  if (Version = 'master') or (Version = 'main') then
    URL := Format('https://%s/archive/refs/heads/%s.zip', [Name, Version])
  else
    URL := Format('https://%s/archive/refs/tags/%s.zip', [Name, Version]);
end;


procedure TGithubDependency.Install(const Path, InstallDir: string);
begin
  Mkdir (InstallDir);
  UnzipFile (Path, InstallDir);
end;


function TGithubDependency.GetPackagePath(var Path: string): boolean;
var
  InstallDest: string;
  Res: TSearchRec;
  It: integer;
  MainDir: string;
begin
  InstallDest := InstallDestination(Format('%s@%s', [CleanName, Version]));
  It := FindFirst(IncludeTrailingPathDelimiter(InstallDest) + '*', faDirectory, Res);

  try
    while It = 0 do
    begin
      if (Res.Name<>'.') and (Res.Name<>'..') then
        if Length(MainDir)=0 then begin
          MainDir := Res.Name;
          break;
        end;
      It := FindNext(Res);
    end;
  finally
    FindClose(Res);
  end;

  if Length(MainDir) = 0 then
    exit(false);

  Path := Format('%s/%s', [InstallDest, MainDir]);
  PackagePath := Path;
  exit(true);
end;


{ TPackage }


constructor TPackage.Create();
begin
  inherited;
  DependencyDict := TDependencyDict.Create;
  DependencyObjects := TDependencyObjectDict.Create;
  DependencyPackages := TDependencyPackageDict.Create;
  UnitsDirs := TStringList.Create;
  Patches := TDependencyPackageDict.Create;
end;


procedure TPackage.LoadFromJSON(const JSONString: string);
var
  JSONData: TJSONData;
  JSONObject, DepsObject: TJSONObject;
  JSONArray: TJSONArray;
  I: Integer;
  Key: string;
  Patch: TPackage;

  function ParseCompiler(const s: string): TCompiler;
  begin
    result := compilerPas2JS;
  end;

begin
  JSONData := GetJSON(JSONString);
  try
    if JSONData.JSONType = jtObject then
    begin
      JSONObject := TJSONObject(JSONData);
      if JSONObject.Find('Name', jtString) <> nil then
        Name := JSONObject.Get('Name', '');
      if JSONObject.Find('Version', jtString) <> nil then
        Version := JSONObject.Get('Version', '');
      if JSONObject.Find('Compiler', jtString) <> nil then
        Compiler := ParseCompiler(JSONObject.Get('Compiler', ''));
      if JSONObject.Find('PackagesDir', jtString) <> nil then
        PackagesDir := JSONObject.Get('PackagesDir', '');

      if Length(PackagesDir) = 0 then
        PackagesDir := 'pjamas-packages';

      if JSONObject.Find('UnitsDirs', jtArray) <> nil then
      begin
        JSONArray := JSONObject.Arrays['UnitsDirs'];
        for I := 0 to JSONArray.Count - 1 do
        begin
          UnitsDirs.Add(JSONArray.Strings[I]);
        end;
      end;

      if JSONObject.Find('Dependencies', jtObject) <> nil then
      begin
        DepsObject := TJSONObject(JSONObject.Objects['Dependencies']);
        for I := 0 to DepsObject.Count - 1 do
        begin
          DependencyDict.Add(DepsObject.Names[I], DepsObject.Items[I].AsString);
        end;
      end;

      if JSONObject.Find('Patches', jtObject) <> nil then
      begin
        DepsObject := TJSONObject(JSONObject.Objects['Patches']);
        for I := 0 to DepsObject.Count - 1 do
        begin
          Patch := TPackage.Create;
          Patch.LoadFromJSON(DepsObject.Items[I].AsJSON);
          Patches.Add(DepsObject.Names[I], Patch);
        end;
      end;
    end
    else
      raise Exception.Create('Invalid JSON format: Expected an object');
  finally
    JSONData.Free;
  end;
end;


procedure TPackage.LoadFromFile(const ADir, AFilename: string);
var
  Contents: string;
begin
  Contents := ReadFileToString(AFilename);
  if Length(Contents) > 0 then LoadFromJSON(Contents);
  Path := ADir;
  Filename := AFilename;
end;


function TPackage.SaveToJSON(): string;
var
  JSONObject: TJSONObject;
  JSONArray: TJSONArray;
  DepsObject: TJSONObject;
  DepPair: specialize TPair<string, string>;
  I: Integer;

  function CompilerToString(com: TCompiler): string;
  begin
    case com of
      compilerFPC : Result := 'FPC';
      compilerPas2JS : Result := 'Pas2JS';
    end;
  end;

begin
  JSONObject := TJSONObject.Create;
  try
    JSONObject.Add('Name', Name);
    JSONObject.Add('Version', Version);
    JSONObject.Add('Compiler', CompilerToString(Compiler));
    JSONObject.Add('PackagesDir', PackagesDir);

    JSONArray := TJSONArray.Create;
    for I:=0 to UnitsDirs.Count - 1 do JSONArray.Add(UnitsDirs[I]);
    JSONObject.Add('UnitsDirs', JSONArray);

    DepsObject := TJSONObject.Create;
    for DepPair in DependencyDict do DepsObject.Add(DepPair.Key, DepPair.Value);
    JSONObject.Add('Dependencies', DepsObject);

    Result := ToPrettyJSON(JSONObject);
    writeln(Result);
  finally
    JSONObject.Free;
  end
end;


procedure TPackage.SaveToFile(const AFilename: string);
var
  FileStream: TFileStream;
  Bytes: TBytes;
begin
  FileStream := TFileStream.Create(AFilename, fmCreate);
  try
    Bytes := TEncoding.UTF8.GetBytes(SaveToJSON);
    FileStream.Write(Bytes[0], Length(Bytes));
  finally
    FileStream.Free;
  end;
end;


procedure TPackage.MakeUpData(const ADir: string);
var
  Rec: TSearchRec;
  It: integer;
  PatchPkg: TPackage;
  UnitDir: string;
begin
  Path := ADir;

  if UsedBy <> nil then
  begin
    if UsedBy.Patches.TryGetValue(Dependency.Name, PatchPkg) then
      for UnitDir in PatchPkg.UnitsDirs do
        UnitsDirs.Add(Dependency.PackagePath + '/' + UnitDir);
  end;

  It := FindFirst(IncludeTrailingPathDelimiter(Path) + '*', faAnyFile, Rec);
  try
    while It = 0 do
    begin
      if (Rec.Name<>'.') and (Rec.Name<>'..') then
      begin
        if EndsText('.pas', Rec.Name) then
        begin
          UnitsDirs.Add(Path);
          break;
        end;
      end;
      It := FindNext(Rec);
    end;
  finally
    FindClose(Rec);
  end;
end;


procedure TPackage.RefreshDependenciesObjects;
var
  Key: String;

  procedure ParseDependency(dName, dVersion: string);
  begin
    if AnsiPos('github.com', dName) <> 0 then
      DependencyObjects.Add(dName, TGithubDependency.Create(dName, dVersion))
    else
      raise Exception.CreateFmt('unknown dependency type: %s', [dName]);
  end;

begin
  for Key in DependencyDict.Keys do
  begin
    ParseDependency(Key, DependencyDict[Key]);
  end;
end;


procedure TPackage.DownloadDependencies;
var
  Dep: specialize TPair<string, TDependency>;
  url: string;
  DesiredFileName: string;
  DesiredExtension: string;
  FileDest: string;
  InstallDest: string;
  PkgPath: string;
  PkgFile: string;
  Pkg: TPackage;
begin
  for Dep in DependencyObjects do
  begin
    Dep.Value.BuildDownloadURL(url, DesiredFileName, DesiredExtension);

    FileDest := DownloadDestination(DesiredFileName, DesiredExtension);
    InstallDest := InstallDestination(DesiredFileName);

    if Options.DoDownloads then
    begin
      if Options.ForceDownload or (not FileExists(FileDest)) then
      begin
        DownloadFile(url, FileDest);
      end;

      if Options.ForceDownload or (not DirectoryExists(InstallDest)) then
      begin
        Dep.Value.Install(FileDest, InstallDest);
      end;
    end;

    if Dep.Value.GetPackagePath(PkgPath) then
    begin
      PkgFile := Format('%s/%s', [PkgPath, PjamasFile]);
      Pkg := TPackage.Create;
      Pkg.UsedBy := Self;
      Pkg.Dependency := Dep.Value;

      if FileExists(PkgFile) then
        Pkg.LoadFromFile(PkgPath, PkgFile)
      else
        Pkg.MakeUpData(PkgPath);

      PackageQueue.Enqueue(Pkg);
    end else
    begin
      if not FileExists(FileDest) then
        writeln('warning: dependency ', Dep.Value.Name, ' is not downloaded, use "pjamas get" to download');
    end;
  end;
end;


{ TPjamasApplication }


procedure TPjamasApplication.AddCommand(names: TStringArray; description: string; proc: TCommandProc);
var
  cname: string;
  rec: TCommandRec;
begin
  for cname in names do
  begin
    rec.name := cname;
    rec.description := description;
    rec.proc := proc;
    commands.Add(cname, rec);
  end;
end;


procedure TPjamasApplication.CmdBuild(paramIndex: integer);
begin
  // @TODO
end;


procedure TPjamasApplication.CmdGet(paramIndex: integer);
var
  Dep: string;
  Version: string;
begin
  Options.DoDownloads := true;
  if paramIndex <= High(CommandParams) then
  begin
    Dep := CommandParams[paramIndex];
    Version := 'master';

    if paramIndex+1 <= High(CommandParams) then
      Version := CommandParams[paramIndex+1];

    RootPackage.DependencyDict.AddOrSetValue(Dep, Version);

    Options.DoWritePackage := true;
  end;
end;


procedure TPjamasApplication.CmdInstalled(paramIndex: integer);
begin
end;


procedure TPjamasApplication.CmdRemove(paramIndex: integer);
var
  Dep: string;
begin
  if paramIndex <= High(CommandParams) then
  begin
    Dep := CommandParams[paramIndex];
    RootPackage.DependencyDict.Remove(Dep);
    Options.DoWritePackage := true;
  end;
end;


procedure TPjamasApplication.CmdUnits(paramIndex: integer);
begin
  Options.DoDownloads := false;
  Options.DoWritePackage := false;
  Options.DoEchoUnits := true;
end;


procedure TPjamasApplication.ExecuteCommand(cmd: string; paramIndex: integer);
var
  rec: TCommandRec;
begin
  if commands.TryGetValue(cmd, rec) then
    rec.proc(paramIndex)
  else
    ShowException(Exception.Create(Format('Command not found: %s', [cmd])));
end;


procedure TPjamasApplication.LoadRootProject;
begin
  RootPackage := TPackage.Create;
  RootPackage.LoadFromFile('.', PjamasFile);
end;


procedure TPjamasApplication.MainRoutine;
var
  UnitDir: string;
begin
  UnitDirs := TStringList.Create;
  PackageQueue := TPackageQueue.Create;

  if not FileExists(PjamasFile) then
  begin
    if not Options.AllowCreate then
    begin
      ShowException(Exception.Create('not in a pjamas package'));
      Terminate;
      Exit;
    end;
  end;

  if not DirectoryExists(RootPackage.PackagesDir) then
    Mkdir(RootPackage.PackagesDir);

  if not DirectoryExists(IncludeTrailingPathDelimiter(RootPackage.PackagesDir)+DownloadDirName) then
    Mkdir(IncludeTrailingPathDelimiter(RootPackage.PackagesDir)+DownloadDirName);

  if not DirectoryExists(IncludeTrailingPathDelimiter(RootPackage.PackagesDir)+InstalledDirName) then
    Mkdir(IncludeTrailingPathDelimiter(RootPackage.PackagesDir)+InstalledDirName);

  PackageQueue.Enqueue(RootPackage);
  repeat
    CurrentPackage := PackageQueue.Dequeue;
    CurrentPackage.RefreshDependenciesObjects;
    CurrentPackage.DownloadDependencies;
    for UnitDir in CurrentPackage.UnitsDirs do
      UnitDirs.Add(UnitDir);
  until PackageQueue.Count = 0;
end;


procedure TPjamasApplication.DoRun;
var
  ErrorMsg: String;
  UnitDir: string;
begin
  AddCommand(
    ['get'],
    'Add dependency to project',
    @CmdGet
  );
  AddCommand(
    ['installed'],
    'List installed dependencies',
    @CmdInstalled
  );
  AddCommand(
    ['remove'],
    'Remove dependency from project',
    @CmdRemove
  );
  AddCommand(
    ['units'],
    'Return units directories used by this project (this string should be fed into fpc/pas2js options)',
    @CmdUnits
  );

  // quick check parameters
  ErrorMsg := CheckOptions(ShortOpts, LongOpts);

  if ErrorMsg<>'' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  { add your program here }

  Options.ForceDownload := HasOption('f', 'force-download');
  Options.AllowCreate := HasOption('c', 'create');

  LoadRootProject;

  CommandParams := GetNonOptions(ShortOpts, LongOpts);
  if Length(CommandParams) > 0 then
    ExecuteCommand(CommandParams[0], 1);

  MainRoutine;

  if Options.DoWritePackage then
  begin
    RootPackage.SaveToFile(PjamasFile);
  end;

  if Options.DoEchoUnits then
  begin
    for UnitDir in UnitDirs do
    begin
      write('-Fu', UnitDir, ' ');
    end;
    writeln('');
  end;

  // stop program loop
  Terminate;
end;


constructor TPjamasApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
  commands := TCommandDict.create;
end;


destructor TPjamasApplication.Destroy;
begin
  inherited Destroy;
end;


procedure TPjamasApplication.WriteHelp;
var
  optIndex : integer;
  pair : specialize TPair<string, TCommandRec>;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' [-', ShortOpts, '] [Commands]');

  for optIndex := Low(ShortOpts) to High(ShortOpts) do
    writeln('  -', ShortOpts[1+optIndex], ' --', LongOpts[optIndex], ' => ', Descriptions[optIndex]);

  writeln('Commands: ');

  for pair in commands do
    writeln('  ', pair.key, ' - ', pair.value.description);
end;


begin
  Application:=TPjamasApplication.Create(nil);
  Application.Title:='My Application';
  Application.Run;
  Application.Free;
end.
