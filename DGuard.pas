// *************************************************************************** }
//
// Delphi Guard
//
// Copyright (c) 2020-2025 Daniele Teti
//
// https://github.com/danieleteti/delphiguard
//
// ***************************************************************************
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
// ***************************************************************************


unit DGuard;

{
  Unified RAII Resource Management for Delphi

  Two usage patterns:
  1. Guard Pattern - Individual object lifetime management
     var stream := Using.Guard(TFileStream.Create('file.txt', fmOpenRead));

  2. ResourceManager Pattern - Group lifetime management
     var Resources := NewResourceScope();
     var stream := Resources.Manager.Add(TFileStream.Create('file.txt', fmOpenRead));

}

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections
  {$IFDEF MSWINDOWS}, Winapi.Windows{$ENDIF};

type
  EGuard = class(Exception)

  end;


  // ============================================================================
  // GUARD PATTERN - Individual Object Lifetime Management
  // ============================================================================

  // Individual resource wrapper
  IGuard<T: class> = interface
    function GetResource: T;
    property Resource: T read GetResource;
    function ExtractFromGuard: T;
  end;

  // Factory for individual guards
  Using = record
  private
    type
      // Implementation Wrapper RAII
      TGuard<T: class> = class(TInterfacedObject, IGuard<T>)
      private
        fResource: T;
        fExtracted: Boolean;
      public
        constructor Create(const AResource: T);
        destructor Destroy; override;
        function GetResource: T;
        function ExtractFromGuard: T;
      end;
  public
    class function Guard<T: class>(AResource: T): IGuard<T>; static;
  end;

  // ============================================================================
  // RESOURCE MANAGER PATTERN - Group Lifetime Management
  // ============================================================================

  // Cross-platform debug callback
  TDebugProc = reference to procedure(const AMessage: string);

  // Forward declarations
  TResourceManager = class;

  // Interface to manage scope/lifetime of ResourceManager
  IResourceScope = interface
    ['{B8F45A21-8B4C-4F3D-9E2A-1234567890AB}']
    function GetManager: TResourceManager;
    function GetResourceCount: Integer;
    function GetResourceInfo: string;

    property Manager: TResourceManager read GetManager;
    property ResourceCount: Integer read GetResourceCount;
  end;

  // Information about a managed resource
  TResourceInfo = record
    Resource: TObject;
    Name: string;
    ClassName: string;
    CreatedAt: TDateTime;
  end;

  // ResourceManager as normal class with generic methods
  TResourceManager = class
  private
    FResources: TList<TResourceInfo>;
    FDebugMode: Boolean;
    FDebugProc: TDebugProc;

    procedure LogDebug(const AMessage: string);
    procedure ValidateResource(AResource: TObject);
  public
    constructor Create(ADebugMode: Boolean = False);
    destructor Destroy; override;

    // Generic methods - now possible because it's a class!
    function Add<T: class>(AResource: T): T;
    function AddNamed<T: class>(const AName: string; AResource: T): T;
    procedure Remove(AResource: TObject);
    procedure Clear;

    // Information
    function GetResourceCount: Integer;
    function GetResourceInfo: string;

    // Debug configuration
    procedure SetDebugProc(const ADebugProc: TDebugProc);

    // Properties
    property ResourceCount: Integer read GetResourceCount;
    property DebugMode: Boolean read FDebugMode write FDebugMode;
  end;

  // Resource Scope implementation
  TResourceScope = class(TInterfacedObject, IResourceScope)
  private
    FManager: TResourceManager;
  public
    constructor Create(ADebugMode: Boolean = False);
    destructor Destroy; override;

    // IResourceScope implementation
    function GetManager: TResourceManager;
    function GetResourceCount: Integer;
    function GetResourceInfo: string;
  end;

// ============================================================================
// FACTORY FUNCTIONS
// ============================================================================

// ResourceManager Pattern factories
function NewResourceScope(ADebugMode: Boolean = False): IResourceScope;
function NewDebugResourceScope(): IResourceScope;
function NewResourceScopeWithDebug(const ADebugProc: TDebugProc): IResourceScope;

// Default debug procedures for different platforms
function DefaultConsoleDebug: TDebugProc;

{$IFDEF MSWINDOWS}
function DefaultWindowsDebug: TDebugProc;
{$ENDIF}

implementation

// ============================================================================
// GUARD PATTERN IMPLEMENTATION
// ============================================================================

constructor Using.TGuard<T>.Create(const AResource: T);
begin
  inherited Create;
  fResource := AResource;
  fExtracted := False;
end;

destructor Using.TGuard<T>.Destroy;
begin
  if not fExtracted then
  begin
    fResource.Free; // Automatic Cleanup
  end;
  inherited;
end;

function Using.TGuard<T>.ExtractFromGuard: T;
begin
  if fExtracted then
  begin
    raise EGuard.Create('Resource already extracted');
  end;
  fExtracted := True;
  Result := fResource;
  fResource := nil;
end;

function Using.TGuard<T>.GetResource: T;
begin
  if fExtracted then
  begin
    raise EGuard.Create('Resource has been extracted, cannot be accessed from guard anymore');
  end;
  Result := fResource;
end;

class function Using.Guard<T>(AResource: T): IGuard<T>;
begin
  Result := TGuard<T>.Create(AResource);
end;

// ============================================================================
// RESOURCE MANAGER PATTERN IMPLEMENTATION
// ============================================================================

// Factory Functions
function NewResourceScope(ADebugMode: Boolean = False): IResourceScope;
begin
  Result := TResourceScope.Create(ADebugMode);
end;

function NewDebugResourceScope(): IResourceScope;
begin
  Result := TResourceScope.Create(True);
end;

function NewResourceScopeWithDebug(const ADebugProc: TDebugProc): IResourceScope;
var
  Scope: TResourceScope;
begin
  Scope := TResourceScope.Create(True);
  Scope.GetManager.SetDebugProc(ADebugProc);
  Result := Scope;
end;

// Default Debug Procedures
function DefaultConsoleDebug: TDebugProc;
begin
  Result := procedure(const AMessage: string)
    begin
      WriteLn('[ResourceManager] ', AMessage);
    end;
end;

{$IFDEF MSWINDOWS}
function DefaultWindowsDebug: TDebugProc;
begin
  Result := procedure(const AMessage: string)
    begin
      // Use Windows-specific debug output when available
      Winapi.Windows.OutputDebugStringA(PAnsiChar(AnsiString('[ResourceManager] ' + AMessage)));
    end;
end;
{$ENDIF}

// TResourceManager Implementation
constructor TResourceManager.Create(ADebugMode: Boolean = False);
begin
  inherited Create;
  FResources := TList<TResourceInfo>.Create;
  FDebugMode := ADebugMode;
  if FDebugMode then
  begin
    // Set default debug procedure (cross-platform console output)
    FDebugProc := DefaultConsoleDebug();
  end;
  LogDebug('ResourceManager created');
end;

destructor TResourceManager.Destroy;
begin
  LogDebug(Format('ResourceManager destroying with %d resources', [FResources.Count]));

  try
    Clear;
  except
    on E: Exception do
    begin
      // Don't re-raise exceptions in destructor
      if FDebugMode then
        LogDebug('ResourceManager.Destroy error: ' + E.Message);
    end;
  end;

  FResources.Free;

  LogDebug('ResourceManager destroyed');
  inherited;
end;

function TResourceManager.Add<T>(AResource: T): T;
begin
  Result := AddNamed<T>('', AResource);
end;

function TResourceManager.AddNamed<T>(const AName: string; AResource: T): T;
var
  Info: TResourceInfo;
begin
  ValidateResource(AResource);

  Info.Resource := AResource;
  Info.Name := AName;
  Info.ClassName := AResource.ClassName;
  Info.CreatedAt := Now;

  FResources.Add(Info);

  LogDebug(Format('Added resource: %s (%s) [%s]',
                 [Info.ClassName, Info.Name, DateTimeToStr(Info.CreatedAt)]));

  Result := AResource;
end;

procedure TResourceManager.Remove(AResource: TObject);
var
  i: Integer;
begin
  ValidateResource(AResource);

  for i := FResources.Count - 1 downto 0 do
  begin
    if FResources[i].Resource = AResource then
    begin
      LogDebug(Format('Removed resource: %s (%s)',
                     [FResources[i].ClassName, FResources[i].Name]));
      FResources.Delete(i);
      Exit;
    end;
  end;

  LogDebug('Warning: Attempted to remove resource not in manager');
end;

procedure TResourceManager.Clear;
var
  i: Integer;
  Info: TResourceInfo;
begin
  LogDebug(Format('Clearing %d resources', [FResources.Count]));

  // Free in reverse order (LIFO) to handle dependencies
  for i := FResources.Count - 1 downto 0 do
  begin
    Info := FResources[i];
    try
      LogDebug(Format('Freeing resource: %s (%s)', [Info.ClassName, Info.Name]));
      Info.Resource.Free;
    except
      on E: Exception do
      begin
        LogDebug(Format('Error freeing resource %s: %s', [Info.ClassName, E.Message]));
        // Continue with others even if one fails
      end;
    end;
  end;

  FResources.Clear;
  LogDebug('All resources cleared');
end;

function TResourceManager.GetResourceCount: Integer;
begin
  Result := FResources.Count;
end;

function TResourceManager.GetResourceInfo: string;
var
  i: Integer;
  Info: TResourceInfo;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add(Format('ResourceManager Info - Total Resources: %d', [FResources.Count]));
    SL.Add('----------------------------------------');

    for i := 0 to FResources.Count - 1 do
    begin
      Info := FResources[i];
      SL.Add(Format('%d. %s', [i + 1, Info.ClassName]));
      if Info.Name <> '' then
        SL.Add(Format('   Name: %s', [Info.Name]));
      SL.Add(Format('   Created: %s', [DateTimeToStr(Info.CreatedAt)]));
      SL.Add('');
    end;

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

procedure TResourceManager.LogDebug(const AMessage: string);
begin
  if FDebugMode and Assigned(FDebugProc) then
  begin
    try
      FDebugProc(AMessage);
    except
      // Ignore debug output errors to prevent cascade failures
    end;
  end;
end;

procedure TResourceManager.SetDebugProc(const ADebugProc: TDebugProc);
begin
  FDebugProc := ADebugProc;
end;

procedure TResourceManager.ValidateResource(AResource: TObject);
begin
  if not Assigned(AResource) then
    raise EArgumentNilException.Create('Resource cannot be nil');
end;

// TResourceScope Implementation
constructor TResourceScope.Create(ADebugMode: Boolean = False);
begin
  inherited Create;
  FManager := TResourceManager.Create(ADebugMode);
end;

destructor TResourceScope.Destroy;
begin
  // Automatic cleanup of manager
  FManager.Free;
  inherited;
end;

function TResourceScope.GetManager: TResourceManager;
begin
  Result := FManager;
end;

function TResourceScope.GetResourceCount: Integer;
begin
  Result := FManager.ResourceCount;
end;

function TResourceScope.GetResourceInfo: string;
begin
  Result := FManager.GetResourceInfo;
end;

end.
