unit KeyInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections;

type
  TKeyInput = class
  private
    FPressed: specialize TDictionary<Integer, Boolean>;
    FReleased: specialize TDictionary<Integer, Boolean>;
    FHeld: specialize TDictionary<Integer, Boolean>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure InitKeys;
    procedure ClearKeys;
    procedure KeyUpEvent(Scancode: Integer);
    procedure KeyDownEvent(Scancode: Integer);
    function KeyPressed(Scancode: Integer): Boolean;
    function KeyReleased(Scancode: Integer): Boolean;
    function KeyHeld(Scancode: Integer): Boolean;
    procedure QuitKeys;
  end;

implementation

constructor TKeyInput.Create;
begin
  FPressed := specialize TDictionary<Integer, Boolean>.Create;
  FReleased := specialize TDictionary<Integer, Boolean>.Create;
  FHeld := specialize TDictionary<Integer, Boolean>.Create;
end;

destructor TKeyInput.Destroy;
begin
  FPressed.Free;
  FReleased.Free;
  FHeld.Free;
  inherited Destroy;
end;

procedure TKeyInput.InitKeys;
begin
  FPressed.Clear;
  FReleased.Clear;
  FHeld.Clear;
end;

procedure TKeyInput.ClearKeys;
begin
  FPressed.Clear;
  FReleased.Clear;
end;

procedure TKeyInput.KeyUpEvent(Scancode: Integer);
begin
  FReleased.AddOrSetValue(Scancode, True);
  FHeld.AddOrSetValue(Scancode, False);
end;

procedure TKeyInput.KeyDownEvent(Scancode: Integer);
begin
  FPressed.AddOrSetValue(Scancode, True);
  FHeld.AddOrSetValue(Scancode, True);
end;

function TKeyInput.KeyPressed(Scancode: Integer): Boolean;
begin
  Result := FPressed.ContainsKey(Scancode) and FPressed[Scancode];
end;

function TKeyInput.KeyReleased(Scancode: Integer): Boolean;
begin
  Result := FReleased.ContainsKey(Scancode) and FReleased[Scancode];
end;

function TKeyInput.KeyHeld(Scancode: Integer): Boolean;
begin
  Result := FHeld.ContainsKey(Scancode) and FHeld[Scancode];
end;

procedure TKeyInput.QuitKeys;
begin
  FPressed.Clear;
  FReleased.Clear;
  FHeld.Clear;
end;

end.

