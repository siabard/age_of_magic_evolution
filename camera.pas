unit camera;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, game_types, tilemap;

type
  TDirection = (dirNone, dirUp, dirDown, dirLeft, dirRight);

  { TCamera }

  TCamera = class
  private
    FName: string;
    FX, FY: integer;
    FTargetX, FTargetY: integer;
    FMaxX, FMaxY: integer;
    FW, FH: integer;
  public
    constructor Create(AName: string; AX, AY, AW, AH: integer);
    function GetRect: RRect;
    procedure Follow(PosX, PosY: integer; VDir, HDir: TDirection);
    procedure Update(DT: real);
    property Name: string read FName write FName;
    property X: integer read FX write FX;
    property Y: integer read FY write FY;
    property TargetX: integer read FTargetX write FTargetX;
    property TargetY: integer read FTargetY write FTargetY;
    property MaxX: integer read FMaxX write FMaxX;
    property MaxY: integer read FMaxY write FMaxY;
    property W: integer read FW write FW;
    property H: integer read FH write FH;
    procedure Teleport(Map: RTilemap; Mx: integer; My: integer);
  end;

implementation

constructor TCamera.Create(AName: string; AX, AY, AW, AH: integer);
begin
  FName := AName;
  FX := AX;
  FY := AY;
  FTargetX := AX;
  FTargetY := AY;
  FW := AW;
  FH := AH;
  { 기본 카메라가 표시하는 전체 영역은 기본 가로, 세로의 두 배 }
  FMaxX := FW * 2;
  FMaxY := FH * 2;
end;

function TCamera.GetRect: RRect;
begin
  Result.RX := FX;
  Result.RY := FY;
  Result.RW := FX + FW;
  Result.RH := FY + FH;
end;

{
    Camera는 대상 X, y위치를 가로, 세로를 보는 방향에 맞추어 움직인다.

}
procedure TCamera.Follow(PosX, PosY: integer; VDir, HDir: TDirection);
begin

  {
  WriteLn('Camera check ');
  WriteLn(Format(' For X : %d, Y : %d ', [PosX, PosY]));
  WriteLn(VDir, HDir);
  WriteLn(Format(' Camera WH W : %d, H : %d ', [FW, FH]));
  }
  case VDir of
    dirUp: FTargetY := Min(FMaxY - FH, Max(0, PosY - Round(0.6 * FH)));
    dirDown: FTargetY := Max(0, Min(FMaxY - FH, PosY - Round(0.4 * FH)));
  end;

  case HDir of
    dirLeft: FTargetX := Min(FMaxX - FW, Max(0, PosX - Round(0.6 * FW)));
    dirRight: FTargetX := Max(0, Min(FMaxX - FW, PosX - Round(0.4 * FW)));
  end;
  {
  WriteLn(Format(' CAMERA X : %d, Y : %d ', [FX, FY]));
  WriteLn(Format(' TARGET X : %d, Y : %d ', [FTargetX, FTargetY]));
  }
end;

procedure TCamera.Update(DT: real);
var
  DeltaX, DeltaY: integer;
  FDX, FDY: real;
begin
  DeltaX := FX - FTargetX;
  DeltaY := FY - FTargetY;

  if (FX <> FTargetX) or (FY <> FTargetY) then
  begin
    {
    WriteLn('Dt : ', DT);
    WriteLn('DELTAX: ', DeltaX * DT);
    WriteLn('DELTAY: ', DeltaY * DT);
    }

    FDX := DeltaX * DT;
    FDY := DeltaY * DT;

    if Abs(FDX) < 1.0 then
      FDX := 1.0 * Sign(FDX);

    if Abs(FDY) < 1.0 then
      FDY := 1.0 * Sign(FDY);



    FX := Round(FX - FDX);
    FY := Round(FY - FDY);

    if Abs(FX - FTargetX) < 1 then
      FX := FTargetX;

    if Abs(FY - FTargetY) < 1 then
      FY := FTargetY;
  end;
end;

procedure TCamera.Teleport(Map: RTilemap; Mx: integer; My: integer);
var
  Target: RVec2;
begin
  Target := GetTilePos(Map, Mx, My);
  Self.X := Max(0, Target.RX - Floor(Self.FW / 2));
  Self.Y := Max(0, Target.RY - Floor(Self.FH / 2));
  Self.TargetX := Self.X;
  Self.TargetY := Self.Y;
end;



end.
