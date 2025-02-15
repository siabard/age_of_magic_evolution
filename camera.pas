unit camera;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

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
    function GetRect: TRect;
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
  FMaxX := 0;
  FMaxY := 0;
end;

function TCamera.GetRect: TRect;
begin
  Result.Left := FX;
  Result.Top := FY;
  Result.Right := FX + FW;
  Result.Bottom := FY + FH;
end;

procedure TCamera.Follow(PosX, PosY: Integer; VDir, HDir: TDirection);
begin
  case VDir of
    dirUp: FTargetY := Min(FMaxY - FH, Max(0, PosY - Round(0.6 * FH)));
    dirDown: FTargetY := Max(0, Min(FMaxY - FH, PosY - Round(0.4 * FH)));
    else FTargetY := PosY;
  end;

  case HDir of
    dirLeft: FTargetX := Min(FMaxX - FW, Max(0, PosX - Round(0.6 * FW)));
    dirRight: FTargetX := Max(0, Min(FMaxX - FW, PosX - Round(0.4 * FW)));
    else FTargetX := PosX;
  end;
end;

procedure TCamera.Update(DT: Real);
var
  DeltaX, DeltaY: Integer;
begin
  DeltaX := FX - FTargetX;
  DeltaY := FY - FTargetY;

  if (FX <> FTargetX) or (FY <> FTargetY) then
  begin
    FX := Round(FX - (DeltaX * DT));
    FY := Round(FY - (DeltaY * DT));
  end;
end;

end.

