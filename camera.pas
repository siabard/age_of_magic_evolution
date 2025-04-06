unit camera;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, game_types;

type
  TDirection = (dirNone, dirUp, dirDown, dirLeft, dirRight);

  TCamera = class
  private
    FName: string;
    FX, FY: Integer;
    FTargetX, FTargetY: Integer;
    FMaxX, FMaxY: Integer;
    FW, FH: Integer;
  public
    constructor Create(AName: string; AX, AY, AW, AH: Integer);
    function GetRect: RRect;
    procedure Follow(PosX, PosY: Integer; VDir, HDir: TDirection);
    procedure Update(DT: Real);
    property Name: string read FName write FName;
    property X: Integer read FX write FX;
    property Y: Integer read FY write FY;
    property TargetX: Integer read FTargetX write FTargetX;
    property TargetY: Integer read FTargetY write FTargetY;
    property MaxX: Integer read FMaxX write FMaxX;
    property MaxY: Integer read FMaxY write FMaxY;
    property W: Integer read FW write FW;
    property H: Integer read FH write FH;
  end;

implementation

constructor TCamera.Create(AName: string; AX, AY, AW, AH: Integer);
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
procedure TCamera.Follow(PosX, PosY: Integer; VDir, HDir: TDirection);
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

procedure TCamera.Update(DT: Real);
var
  DeltaX, DeltaY: Integer;
  FDX, FDY: Real;
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

    IF Abs(FDX) < 1.0 Then
       FDX := 1.0 * Sign(FDX);

    If Abs(FDY) < 1.0 Then
       FDY := 1.0 * Sign(FDY);



    FX := Round(FX - FDX);
    FY := Round(FY - FDY);

    If Abs(FX - FTargetX) < 1 Then
       FX := FTargetX;

    If Abs(FY - FTargetY) < 1 Then
       FY := FTargetY;
  end;
end;

end.

