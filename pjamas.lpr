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
  paszlib, zipper
  { you can add units after this };

type

  TDependency = class public
    Name: string;
    CleanName: string;
    Version: string;

    procedure BuildDownloadURL(var URL, DesiredFileName, DesiredExtension: string); virtual; abstract;
    procedure Install(const Path, InstallDir: string); virtual; abstract;
    procedure GetPackagePath(var Path: string); virtual; abstract;
  end;

  TGithubDependency = class (TDependency) public
    constructor Create(aName, aVersion: string);
    procedure BuildDownloadURL(var URL, DesiredFileName, DesiredExtension: string); override;
    procedure Install(const Path, InstallDir: string); override;
    procedure GetPackagePath(var Path: string); override;
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
    DownloadedPackagesPath: string;
    UnitsDirectories: TStringList;
    Path: string;
    Filename: string;
    DependencyDict: TDependencyDict;
    DependencyObjects: TDependencyObjectDict;
    DependencyPackages: TDependencyPackageDict;

    constructor Create;

    procedure LoadFromJSON(const JSONString: string);
    procedure LoadFromFile(const ADir, AFilename: string);
    procedure DownloadDependencies;
    procedure MakeUpData(const ADir: string);
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
    procedure CmdDownloadGithubRepo(paramIndex: integer);
    procedure CmdDownloadAnyRepo(paramIndex: integer);
    procedure ExecuteCommand(cmd: string; paramIndex: integer);
  end;


const
  ShortOpts = 'hf';
  LongOpts : array of string = ('help', 'force-download');
  Descriptions : array of string = (
    'Display help',
    'Download dependencies globally'
  );
  DownloadDirName = 'downloads';
  InstalledDirName = 'installed';


var
  Application: TPjamasApplication;
  RootPackage: TPackage;
  CurrentPackage: TPackage;
  PackageQueue: TPackageQueue;
  UnitDirs: TStringList;
  ForceDownload: boolean;


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
    RootPackage.DownloadedPackagesPath,
    DownloadDirName,
    DepFileName,
    DepExt
  ]);
end;


function InstallDestination(const DepFileName: string): string;
begin
  Result := Format('%s/%s/%s', [
    RootPackage.DownloadedPackagesPath,
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
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    StringStream := TStringStream.Create('');
    try
      StringStream.CopyFrom(FileStream, FileStream.Size);
      Result := StringStream.DataString;
    finally
      StringStream.Free;
    end;
  finally
    FileStream.Free;
  end;
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


procedure TGithubDependency.GetPackagePath(var Path: string);
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
    raise Exception.CreateFmt('malformed github package: %s', [InstallDest]);

  Path := Format('%s/%s', [InstallDest, MainDir]);
end;


{ TPackage }


constructor TPackage.Create();
begin
  inherited;
  DependencyDict := TDependencyDict.Create;
  DependencyObjects := TDependencyObjectDict.Create;
  DependencyPackages := TDependencyPackageDict.Create;
  UnitsDirectories := TStringList.Create;
end;


procedure TPackage.LoadFromJSON(const JSONString: string);
var
  JSONData: TJSONData;
  JSONObject, DepsObject: TJSONObject;
  JSONArray: TJSONArray;
  I: Integer;
  Key: string;

  function ParseCompiler(const s: string): TCompiler;
  begin
    result := compilerPas2JS;
  end;

  procedure ParseDependency(dName, dVersion: string);
  begin
    if AnsiPos('github.com', dName) <> 0 then
      DependencyObjects.Add(dName, TGithubDependency.Create(dName, dVersion))
    else
      raise Exception.CreateFmt('unknown dependency type: %s', [dName]);
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
      if JSONObject.Find('Version', jtString) <> nil then
        Compiler := ParseCompiler(JSONObject.Get('Compiler', ''));
      if JSONObject.Find('DownloadedPackagesPath', jtString) <> nil then
        DownloadedPackagesPath := JSONObject.Get('DownloadedPackagesPath', '');
      if JSONObject.Find('UnitsDirectories', jtArray) <> nil then
      begin
        JSONArray := JSONObject.Arrays['UnitsDirectories'];
        for I := 0 to JSONArray.Count - 1 do
        begin
          UnitsDirectories.Add(JSONArray.Strings[I]);
        end;
      end;
      if JSONObject.Find('Dependencies', jtObject) <> nil then
      begin
        DepsObject := TJSONObject(JSONObject.Objects['Dependencies']);
        for I := 0 to DepsObject.Count - 1 do
        begin
          DependencyDict.Add(DepsObject.Names[I], DepsObject.Items[I].AsString);
        end;

        for Key in DependencyDict.Keys do
        begin
          ParseDependency(Key, DependencyDict[Key]);
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
begin
  LoadFromJSON(ReadFileToString(AFilename));
  Path := ADir;
  Filename := AFilename;
end;


procedure TPackage.MakeUpData(const ADir: string);
var
  Rec: TSearchRec;
  It: integer;
begin
  Path:=ADir;
  It := FindFirst(IncludeTrailingPathDelimiter(Path) + '*', faAnyFile, Rec);
  try
    while It = 0 do
    begin
      if (Rec.Name<>'.') and (Rec.Name<>'..') then
      begin
        if EndsText('.pas', Rec.Name) then
        begin
          UnitsDirectories.Add(Path);
          break;
        end;
      end;
      It := FindNext(Rec);
    end;
  finally
    FindClose(Rec);
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

    if ForceDownload or (not FileExists(FileDest)) then
    begin
      DownloadFile(url, FileDest);
    end;

    if ForceDownload or (not DirectoryExists(InstallDest)) then
    begin
      Dep.Value.Install(FileDest, InstallDest);
    end;

    Dep.Value.GetPackagePath(PkgPath);

    PkgFile := Format('%s/pjamas.json', [PkgPath]);
    Pkg := TPackage.Create;

    if FileExists(PkgFile) then
      Pkg.LoadFromFile(PkgPath, PkgFile)
    else
      Pkg.MakeUpData(PkgPath);

    PackageQueue.Enqueue(Pkg);
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


procedure TPjamasApplication.CmdDownloadGithubRepo(paramIndex: integer);
begin
  writeln('DownloadGithubRepo');
  //RooTPackage.AddDependency(Arg1, Arg2);
  //RooTPackage.SaveToJSON();
end;


procedure TPjamasApplication.CmdDownloadAnyRepo(paramIndex: integer);
begin
  writeln('DownloadAnyRepo');
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


procedure TPjamasApplication.DoRun;
var
  ErrorMsg: String;
  nonOptions: TStringArray;
  UnitDir: string;
begin
  AddCommand(['build'], 'Build the Pas2JS project', @CmdDownloadGithubRepo);
  AddCommand(['init'], 'Initialize project', @CmdDownloadGithubRepo);
  AddCommand(['gh', 'github'], 'Download dependency from github repo', @CmdDownloadGithubRepo);
  AddCommand(['get'], 'Download dependency from url', @CmdDownloadAnyRepo);
  AddCommand(['install'], 'Download dependency from index', @CmdDownloadAnyRepo);
  AddCommand(['list'], 'List packages from index', @CmdDownloadAnyRepo);
  AddCommand(['remove'], 'Remove dependency from project', @CmdDownloadAnyRepo);
  AddCommand(['units'], 'Return units directories used by this project (this string should be fed into pas2js options)', @CmdDownloadAnyRepo);

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

  ForceDownload := HasOption('f', 'force-download');

  nonOptions := GetNonOptions(ShortOpts, LongOpts);

  UnitDirs := TStringList.Create;
  PackageQueue := TPackageQueue.Create;
  RootPackage := TPackage.Create;

  if not FileExists('pjamas.json') then
  begin
    ShowException(Exception.Create('not in a pjamas package'));
    Terminate;
    Exit;
  end;

  RootPackage.LoadFromFile('.', 'pjamas.json');

  if not DirectoryExists(RootPackage.DownloadedPackagesPath) then
    Mkdir(RootPackage.DownloadedPackagesPath);

  if not DirectoryExists(IncludeTrailingPathDelimiter(RootPackage.DownloadedPackagesPath)+DownloadDirName) then
    Mkdir(IncludeTrailingPathDelimiter(RootPackage.DownloadedPackagesPath)+DownloadDirName);

  if not DirectoryExists(IncludeTrailingPathDelimiter(RootPackage.DownloadedPackagesPath)+InstalledDirName) then
    Mkdir(IncludeTrailingPathDelimiter(RootPackage.DownloadedPackagesPath)+InstalledDirName);

  PackageQueue.Enqueue(RootPackage);
  repeat
    CurrentPackage := PackageQueue.Dequeue;
    CurrentPackage.DownloadDependencies;
    for UnitDir in CurrentPackage.UnitsDirectories do
    begin
      UnitDirs.Add(UnitDir);
    end;
  until PackageQueue.Count = 0;

  for UnitDir in UnitDirs do
  begin
    write('-Fu', UnitDir, ' ');
  end;
  writeln('');

  // stop program loop
  Terminate;
end;


constructor TPjamasApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
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

  for optIndex := low(ShortOpts) to high(ShortOpts) do
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
