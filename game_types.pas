unit game_types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  RRect = record
    RX: integer;
    RY: integer;
    RW: integer;
    RH: integer;
  end;

  RPos = record
    RX: integer;
    RY: integer;
  end;

  RSize = record
    RW: integer;
    RH: integer;
  end;

  RVec2 = record
    RX: integer;
    RY: integer;
  end;

function RectCenter(ARect: RRect): RVec2;
function RectContainsX(ARect: RRect; BRect: RRect): boolean;
function RectContainsY(ARect: RRect; BRect: RRect): boolean;
function RectContains(ARect: RRect; BRect: RRect): boolean;

function Vec2Delta(V1: RVec2; V2: RVec2): RVec2;
function Vec2Scale(V1: RVec2; n: integer): RVec2;
function Vec2Size(V1: RVec2): real;
function Vec2Normalize(V1: RVec2): RVec2;
function Vec2Plus(V1: RVec2; V2: RVec2): RVec2;

implementation

function RectCenter(ARect: RRect): RVec2;
var
  CenterX: integer;
  CenterY: integer;
begin
  CenterX := ARect.RX + Round(ARect.RW / 2);
  CenterY := ARect.RY + Round(ARect.RH / 2);

  Result.RX := CenterX;
  Result.RY := CenterY;
end;

{ ARect 가 BRect 에 가로로 포함되는지 여부 }
function RectContainsX(ARect: RRect; BRect: RRect): boolean;
var
  ALeft: integer;
  ARight: integer;
  BLeft: integer;
  BRight: integer;
begin
  ALeft := ARect.RX;
  ARight := ARect.RX + ARect.RW - 1;
  BLeft := BRect.RX;
  BRight := BRect.RX + BRect.RW - 1;

  Result := (ALeft >= BLeft) and (ARight <= BRight);

end;

{ Arect가 BRect 에 세로로 포함되는지 여부 }
function RectContainsY(ARect: RRect; BRect: RRect): boolean;
var
  ATop: integer;
  ABottom: integer;
  BTop: integer;
  BBottom: integer;
begin
  ATop := ARect.RY;
  ABottom := ARect.RY + ARect.RH - 1;
  BTop := BRect.RY;
  BBottom := BRect.RY + BRect.RH - 1;

  Result := (ATop >= BTop) and (ABottom <= BBottom);

end;

{ Arect 가 BRect 에 포함되는지 여부 }
function RectContains(ARect: RRect; BRect: RRect): boolean;
begin
  Result := RectContainsX(ARect, BRect) and RectContainsY(ARect, BRect);
end;

function Vec2Delta(V1: RVec2; V2: RVec2): RVec2;
var
  DeltaX: integer;
  DeltaY: integer;
begin
  DeltaX := Abs(V1.RX - V2.RX);
  DeltaY := Abs(V1.RY - V1.RY);

  Result.RX := DeltaX;
  Result.RY := DeltaY;
end;

function Vec2Scale(V1: RVec2; n: integer): RVec2;
begin
  Result.RX := V1.RX * n;
  Result.RY := V1.RY * n;
end;

function Vec2Size(V1: RVec2): real;
begin
  Result := Sqr(V1.RX * V1.RX + V1.RY * V1.RY);
end;

function Vec2Normalize(V1: RVec2): RVec2;
var
  X: integer;
  Y: integer;
  S: real;
begin
  S := Vec2Size(v1);

  if S = 0.0 then
  begin
    Result.RX := 0;
    Result.RY := 0;
  end
  else
  begin
    Result.RX := Round(X / S);
    Result.RY := Round(Y / S);
  end;

end;

function Vec2Plus(V1: RVec2; V2: RVec2): RVec2;
begin
     Result.RX :=  V1.Rx + V2.RX;
     Result.RY := V1.RY + V2.RY;
end;

end.
