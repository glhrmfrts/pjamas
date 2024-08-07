{
  MIT License

  Copyright (c) 2024 Guilherme Freitas Nemeth

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in all
  copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  SOFTWARE.
}

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
    PackagePath: string;

    procedure BuildDownloadURL(var URL, DesiredFileName, DesiredExtension: string); virtual; abstract;
    procedure Install(const Path, InstallDir: string); virtual; abstract;
    function GetPackagePath(var Path: string): boolean; virtual; abstract;
  end;

  { TGithubDependency - A public github repository }

  TGithubDependency = class (TDependency) public
    constructor Create(aName, aVersion: string);
    procedure BuildDownloadURL(var URL, DesiredFileName, DesiredExtension: string); override;
    procedure Install(const Path, InstallDir: string); override;
    function GetPackagePath(var Path: string): boolean; override;
  end;

  { TLocalDependency - A local folder or zip archive }

  TLocalDependency = class (TDependency) public
    IsDirectory: boolean;
    constructor Create(aName, aVersion: string);
    procedure BuildDownloadURL(var URL, DesiredFileName, DesiredExtension: string); override;
    procedure Install(const Path, InstallDir: string); override;
    function GetPackagePath(var Path: string): boolean; override;
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
    CompilerOptions: TStringList;
    PackagesPath: string;
    MainUnit: string;
    UnitPath: TStringList;
    RecursiveUnitPath: TStringList;
    IncludePath: TStringList;
    RecursiveIncludePath: TStringList;
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
    DoEchoOptions: boolean;
    DoEchoMainUnit: boolean;
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
    procedure CmdAll(paramIndex: integer);
    procedure CmdBuild(paramIndex: integer);
    procedure CmdGet(paramIndex: integer);
    procedure CmdHelpJSON(paramIndex: integer);
    procedure CmdRemove(paramIndex: integer);
    procedure CmdUnits(paramIndex: integer);
    procedure ExecuteCommand(cmd: string; paramIndex: integer);
    procedure LoadRootProject;
    procedure MainRoutine;
  end;

  TDocJSONField = record
    Name: string;
    Typ: string;
    Desc: string;
    Note: string;
    Required: boolean;
  end;

  TDocJSONFieldArray = array of TDocJSONField;


const
  ShortOpts = 'hfco';
  LongOpts : array of string = ('help', 'force-download', 'create', 'options');
  Descriptions : array of string = (
    'Display help',
    'Force download of dependencies again',
    'Allow pjamas to create a pjamas.json if one was not found',
    'When running "units", return compiler options as well'
  );
  DownloadDirName = 'downloads';
  InstalledDirName = 'installed';
  PjamasFile = 'pjamas.json';
  DefaultPackageDir = 'pjamas-packages';
  PascalCodeExtensions : array of string = ('.pas', '.pp');
  DocJSONFields : TDocJSONFieldArray = (
    (
      Name: 'Name';
      Typ: 'string';
      Desc: 'Name of the project';
    ),
    (
      Name: 'Version';
      Typ: 'string';
      Desc: 'Version of this project (use semver.org)';
    ),
    (
      Name: 'Compiler';
      Typ: 'string';
      Desc: 'Compiler to use ("FPC" or "Pas2JS")';
    ),
    (
      Name: 'CompilerOptions';
      Typ: 'string';
      Desc: 'Space-separated compiler flags and options';
    ),
    (
      Name: 'PackagesPath';
      Typ: 'string';
      Desc: 'Where to store the downloaded packages (default: pjamas-packages)';
    ),
    (
      Name: 'UnitPath';
      Typ: 'array of string';
      Desc: 'Directories where Pascal source units are located for this project';
      Note: 'Prefix with "recursive:" to recursively search for units';
    ),
    (
      Name: 'Dependencies';
      Typ: 'object (string => string)';
      Desc: 'An object where each key is a Dependency and each value is its version';
      Note: 'For git repositories this is usually a tag name. Branches other than "main" or "master" need "branch:" prefix. Commits need "commit:" prefix.';
    ),
    (
      Name: 'Patches';
      Typ: 'object (string => Package)';
      Desc: 'Add entries to this object to patch any dependency package with additional properties';
      Note: 'This is useful for providing pjamas options for packages that do not include pjamas.json';
    )
  );


var
  Application: TPjamasApplication;
  RootPackage: TPackage;
  CurrentPackage: TPackage;
  PackageQueue: TPackageQueue;
  AllUnitDirs: TStringList;
  AllIncludeDirs: TStringList;
  CommandParams: TStringArray;
  Options: TPjamasOptions;
  CompilerOptions: TStringList;


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
    RootPackage.PackagesPath,
    DownloadDirName,
    DepFileName,
    DepExt
  ]);
end;


function InstallDestination(const DepFileName: string): string;
begin
  Result := Format('%s/%s/%s', [
    RootPackage.PackagesPath,
    InstalledDirName,
    DepFileName
  ]);
end;


function CopyFile(const SrcFileName, DstFileName: String): Boolean;
var
  Src, Dst: File;
  Buf: array of Byte;
  ReadBytes: Int64;
begin
  Assign(Src, SrcFileName);
{$PUSH}{$I-}
  Reset(Src, 1);
{$POP}
  if IOResult <> 0 then
    Exit(False);

  Assign(Dst, DstFileName);
{$PUSH}{$I-}
  Rewrite(Dst, 1);
{$POP}
  if IOResult <> 0 then begin
    Close(Src);
    Exit(False);
  end;

  SetLength(Buf, 64 * 1024 * 1024);
  while not Eof(Src) do begin
{$PUSH}{$I-}
    BlockRead(Src, Buf[0], Length(Buf), ReadBytes);
{$POP}
    if IOResult <> 0 then begin
      Close(Src);
      Close(Dst);
      Exit(False);
    end;

{$PUSH}{$I-}
    BlockWrite(Dst, Buf[0], ReadBytes);
{$POP}
    if IOResult <> 0 then begin
      Close(Src);
      Close(Dst);
      Exit(False);
    end;
  end;

  Close(Src);
  Close(Dst);
  Exit(True);
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


procedure DownloadOrCopyFile(const URL, Destination: string);
var
  Ext: string;
begin
  Ext := ExtractFileExt(Destination);
  if AnsiPos('local:', URL) <> 0 then
    if Ext = 'dir' then
      exit
    else
      CopyFile(ReplaceStr(URL, 'local:', ''), Destination)
  else
    DownloadFile(URL, Destination);
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


procedure AddRecursiveDir(var Target: TStringList; Dir: string);
var
  It: integer;
  Res: TSearchRec;
  AddThis: boolean = false;
  Ext: string;
  CodeExt: string;
begin
  It := FindFirst(IncludeTrailingPathDelimiter(Dir) + '*', faAnyFile, Res);
  try
    while It = 0 do
    begin
      if (Res.Name<>'.') and (Res.Name<>'..') then
        if ((Res.Attr and faDirectory)<>0) then
          AddRecursiveDir(Target, ConcatPaths([Dir, Res.Name]))
        else
        begin
          Ext := ExtractFileExt(Res.Name);
          for CodeExt in PascalCodeExtensions do
            if Ext = CodeExt then
            begin
              AddThis := true;
              break;
            end;
        end;
      It := FindNext(Res);
    end;
  finally
    FindClose(Res);
  end;
  if AddThis then Target.Add(Dir);
end;


procedure CopyDirRecursive(const SourceDir, TargetDir: string);
var
  SearchRec: TSearchRec;
  SourcePath, TargetPath: string;
begin
  if not DirectoryExists(TargetDir) then
    MkDir(TargetDir);

  if FindFirst(SourceDir + PathDelim + '*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
      begin
        SourcePath := SourceDir + PathDelim + SearchRec.Name;
        TargetPath := TargetDir + PathDelim + SearchRec.Name;

        if (SearchRec.Attr and faDirectory) <> 0 then
        begin
          // Recursively copy directories
          CopyDirRecursive(SourcePath, TargetPath);
        end
        else
        begin
          // Copy files
          if not CopyFile(SourcePath, TargetPath) then
            Writeln('Failed to copy file: ', SourcePath);
        end;
      end;
    until FindNext(SearchRec) <> 0;

    FindClose(SearchRec);
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


{ TLocalDependency }


constructor TLocalDependency.Create(aName, aVersion: string);
begin
  Name := aName;
  Version := aVersion;
  CleanName := ReplaceStr(aName, '/', '-');
  CleanName := ReplaceStr(CleanName, ':', '-');
  IsDirectory := DirectoryExists(aVersion);
end;


procedure TLocalDependency.BuildDownloadURL(var URL, DesiredFileName, DesiredExtension: string);
var
  OriginalExt: string;
begin
  if IsDirectory then
    OriginalExt := 'dir'
  else if FileExists(Version) then begin
    OriginalExt := ReplaceStr(ExtractFileExt(Version), '.', '');
  end
  else
    raise Exception.CreateFmt('Dependency "%s" file/directory not found: %s', [Name, Version]);

  DesiredFileName := Format('%s', [CleanName]);
  DesiredExtension := OriginalExt;
  URL := Format('local:%s', [Version]);
end;


procedure TLocalDependency.Install(const Path, InstallDir: string);
begin
  Mkdir (InstallDir);
  if IsDirectory then
    CopyDirRecursive (Path, InstallDir)
  else
    UnzipFile (Path, InstallDir);
end;


function TLocalDependency.GetPackagePath(var Path: string): boolean;
var
  InstallDest: string;
  Res: TSearchRec;
  It: integer;
  MainDir: string;
begin
  InstallDest := InstallDestination(Format('%s', [CleanName]));
  Path := InstallDest;
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
  CompilerOptions := TStringList.Create;
  UnitPath := TStringList.Create;
  RecursiveUnitPath := TStringList.Create;
  IncludePath := TStringList.Create;
  RecursiveIncludePath := TStringList.Create;
  Patches := TDependencyPackageDict.Create;
end;


procedure TPackage.LoadFromJSON(const JSONString: string);
var
  JSONData: TJSONData;
  JSONObject, DepsObject: TJSONObject;
  JSONArray: TJSONArray;
  I: Integer;
  Key: string;
  Dir: string;
  Patch: TPackage;
  ReplaceFlags: TReplaceFlags;
  ReplaceCount: integer = 0;

  function ParseCompiler(const s: string): TCompiler;
  begin
    result := compilerPas2JS;
  end;

  procedure ParsePath(const K: string; var Paths: TStringList; var RecursivePaths: TStringList);
  var
    I: integer;
  begin
    if JSONObject.Find(K, jtArray) <> nil then
    begin
      JSONArray := JSONObject.Arrays[K];
      for I := 0 to JSONArray.Count - 1 do
      begin
        Dir := JSONArray.Strings[I];
        if StartsStr('recursive:', Dir) then
          RecursivePaths.Add(StringReplace(Dir, 'recursive:', '', ReplaceFlags, ReplaceCount))
        else
          Paths.Add(JSONArray.Strings[I]);
      end;
    end;
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

      if JSONObject.Find('CompilerOptions', jtString) <> nil then
        CompilerOptions.SetStrings(SplitString(JSONObject.Get('CompilerOptions', ''), ' '));

      if JSONObject.Find('PackagesPath', jtString) <> nil then
        PackagesPath := JSONObject.Get('PackagesPath', '');

      if JSONObject.Find('MainUnit', jtString) <> nil then
        MainUnit := JSONObject.Get('MainUnit', '');

      if Length(PackagesPath) = 0 then
        PackagesPath := 'pjamas-packages';

      ParsePath('UnitPath', UnitPath, RecursiveUnitPath);
      ParsePath('IncludePath', IncludePath, RecursiveIncludePath);

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
  FormatOptions: TFormatOptions;

  function CompilerToString(com: TCompiler): string;
  begin
    case com of
      compilerFPC : Result := 'FPC';
      compilerPas2JS : Result := 'Pas2JS';
    end;
  end;

  function JoinStrings(ls: TStringList; sep: string): string;
  var
    I: integer;
    Item: string;
  begin
    Result := '';
    for I := 0 to ls.Count - 1 do
    begin
      if Length(ls[I]) > 0 then
      begin
        Result := Result + ls[I];
        if I < ls.Count - 1 then Result := Result + sep;
      end;
    end;
  end;

begin
  JSONObject := TJSONObject.Create;
  try
    if Length(Name) > 0 then JSONObject.Add('Name', Name);
    if Length(Version) > 0 then JSONObject.Add('Version', Version);
    if Length(PackagesPath) > 0 then JSONObject.Add('PackagesPath', PackagesPath);
    if Length(MainUnit) > 0 then JSONObject.Add('MainUnit', MainUnit);

    JSONObject.Add('Compiler', CompilerToString(Compiler));

    if CompilerOptions.Count > 0 then JSONObject.Add('CompilerOptions', JoinStrings(CompilerOptions, ' '));

    JSONArray := TJSONArray.Create;
    for I:=0 to UnitPath.Count - 1 do JSONArray.Add(UnitPath[I]);
    for I:=0 to RecursiveUnitPath.Count - 1 do JSONArray.Add('recursive:'+RecursiveUnitPath[I]);
    JSONObject.Add('UnitPath', JSONArray);

    if (IncludePath.Count > 0) or (RecursiveIncludePath.Count > 0) then begin
      JSONArray := TJSONArray.Create;
      for I:=0 to IncludePath.Count - 1 do JSONArray.Add(IncludePath[I]);
      for I:=0 to RecursiveIncludePath.Count - 1 do JSONArray.Add('recursive:'+RecursiveIncludePath[I]);
      JSONObject.Add('IncludePath', JSONArray);
    end;

    DepsObject := TJSONObject.Create;
    for DepPair in DependencyDict do DepsObject.Add(DepPair.Key, DepPair.Value);
    JSONObject.Add('Dependencies', DepsObject);

    Result := JSONObject.FormatJSON(FormatOptions, 2);
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
    begin
      for UnitDir in PatchPkg.UnitPath do
        UnitPath.Add(Dependency.PackagePath + '/' + UnitDir);
      for UnitDir in PatchPkg.IncludePath do
        IncludePath.Add(Dependency.PackagePath + '/' + UnitDir);
    end;
  end;

  It := FindFirst(IncludeTrailingPathDelimiter(Path) + '*', faAnyFile, Rec);
  try
    while It = 0 do
    begin
      if (Rec.Name<>'.') and (Rec.Name<>'..') then
      begin
        if EndsText('.pas', Rec.Name) then
        begin
          UnitPath.Add(Path);
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
    // @TODO: improve this checks
    if AnsiPos('github.com', dName) <> 0 then
      DependencyObjects.Add(dName, TGithubDependency.Create(dName, dVersion))
    else if AnsiPos('local:', dName) <> 0 then
      DependencyObjects.Add(dName, TLocalDependency.Create(dName, dVersion))
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
        DownloadOrCopyFile(url, FileDest);
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


procedure TPjamasApplication.CmdAll(paramIndex: integer);
begin
  Options.DoEchoOptions := true;
  Options.DoEchoUnits := true;
  Options.DoEchoMainUnit := true;
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


procedure TPjamasApplication.CmdHelpJSON(paramIndex: integer);
var
  Field: TDocJSONField;
begin
  for Field in DocJSONFields do
  begin
    writeln('Name: ', Field.Name);
    writeln('Type: ', Field.Typ);
    writeln('Desc: ', Field.Desc);
    if Length(Field.Note) > 0 then
      writeln('Note: ', Field.Note);
    if Field.Required then
      writeln('Required: true');

    writeln('');
  end;
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
  Opt: string;
begin
  CompilerOptions := TStringList.Create;
  AllUnitDirs := TStringList.Create;
  AllIncludeDirs := TStringList.Create;
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

  if not DirectoryExists(RootPackage.PackagesPath) then
    Mkdir(RootPackage.PackagesPath);

  if not DirectoryExists(IncludeTrailingPathDelimiter(RootPackage.PackagesPath)+DownloadDirName) then
    Mkdir(IncludeTrailingPathDelimiter(RootPackage.PackagesPath)+DownloadDirName);

  if not DirectoryExists(IncludeTrailingPathDelimiter(RootPackage.PackagesPath)+InstalledDirName) then
    Mkdir(IncludeTrailingPathDelimiter(RootPackage.PackagesPath)+InstalledDirName);

  PackageQueue.Enqueue(RootPackage);
  repeat
    CurrentPackage := PackageQueue.Dequeue;
    CurrentPackage.RefreshDependenciesObjects;
    CurrentPackage.DownloadDependencies;

    for UnitDir in CurrentPackage.UnitPath do
      AllUnitDirs.Add(UnitDir);

    for UnitDir in CurrentPackage.IncludePath do
      AllIncludeDirs.Add(UnitDir);

    for UnitDir in CurrentPackage.RecursiveUnitPath do
      AddRecursiveDir(AllUnitDirs, UnitDir);

    for UnitDir in CurrentPackage.RecursiveIncludePath do
      AddRecursiveDir(AllIncludeDirs, UnitDir);

    { Only allow compiler options from root package for now }
    if CurrentPackage = RootPackage then
      for Opt in CurrentPackage.CompilerOptions do
        if Length(Opt) > 0 then
          CompilerOptions.Add(Opt);
  until PackageQueue.Count = 0;
end;


procedure TPjamasApplication.DoRun;
var
  ErrorMsg: String;
  UnitDir: string;
  Opt: string;
begin
  AddCommand(
    ['all'],
    'Return the complete command-line that should be fed into FPC/Pas2JS',
    @CmdAll
  );
  AddCommand(
    ['get'],
    'Add dependency to project',
    @CmdGet
  );
  AddCommand(
    ['help-json'],
    'Provide help for pjamas.json fields',
    @CmdHelpJSON
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
  Options.DoEchoOptions := HasOption('o', 'options');

  LoadRootProject;

  CommandParams := GetNonOptions(ShortOpts, LongOpts);
  if Length(CommandParams) > 0 then
    ExecuteCommand(CommandParams[0], 1)
  else
    ExecuteCommand('all', 1);

  MainRoutine;

  if Options.DoWritePackage then
  begin
    RootPackage.SaveToFile(PjamasFile);
  end;

  if Options.DoEchoOptions then
  begin
    for Opt in CompilerOptions do
    begin
      if Length(Opt) > 0 then
        write(Opt, ' ');
    end;
  end;

  if Options.DoEchoUnits then
  begin
    for UnitDir in AllUnitDirs do
    begin
      write('-Fu', UnitDir, ' ');
    end;
    for UnitDir in AllIncludeDirs do
    begin
      write('-Fi', UnitDir, ' ');
    end;
  end;

  if Options.DoEchoMainUnit then
  begin
    write(RootPackage.MainUnit);
  end;

  writeln('');

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