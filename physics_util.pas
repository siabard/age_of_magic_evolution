unit physics_util;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, game_types;

function RectCenter(ARect: RRect): RVec2;
function RectContainsX(ARect: RRect; BRect: RRect): boolean;
function RectContainsY(ARect: RRect; BRect: RRect): boolean;
function RectContains(ARect: RRect; BRect: RRect): boolean;

function Vec2Delta(V1: RVec2; V2: RVec2): RVec2;
function Vec2Scale(V1: RVec2; n: integer): RVec2;
function Vec2Size(V1: RVec2): real;
function Vec2Normalize(V1: RVec2): RVec2;
function Vec2Plus(V1: RVec2; V2: RVec2): RVec2;

function OverlapAmount(R1: RRect; R2: RRect): RVec2;

function AABBCollision(R1: RRect; R2: RRect): boolean;

{ RVec2 를 중심으로 가로 w, 세로  h인 사각형}
function BoundRect(pos: RVec2; w: integer; h: integer): RRect;

{ 두 RRect2 가 겹친 범위를 결정한다. }
function CameraClippedRect(ARect: RRect; BRect: RRect): RRect;

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
    Result.RX := Round(V1.RX / S);
    Result.RY := Round(V1.RY / S);
  end;

end;

function Vec2Plus(V1: RVec2; V2: RVec2): RVec2;
begin
  Result.RX := V1.Rx + V2.RX;
  Result.RY := V1.RY + V2.RY;
end;

function OverlapAmount(R1: RRect; R2: RRect): RVec2;
var
  C1: RVec2;
  C2: RVec2;
  Delta: RVec2;
  OX: integer;
  OY: integer;
begin
  C1 := RectCenter(R1);
  C2 := RectCenter(R2);
  Delta := Vec2Delta(C1, C2);
  OX := Trunc(R1.RW / 2) + Trunc(R2.RW / 2) - Delta.RX;
  OY := Trunc(R1.RH / 2) + Trunc(R2.RH / 2) - Delta.RY;

  Result.RX := OX;
  Result.RY := OY;

  if RectContainsX(R1, R2) then
    Result.RX := R1.RW
  else
    Result.RX := R2.RW;

  if RectContainsY(R1, R2) then
    Result.RY := R1.RH
  else
    Result.RY := R2.RH;

end;

function AABBCollision(R1: RRect; R2: RRect): boolean;
begin
  Result := (R1.RX < R2.RX + R2.RW) and (R1.RX + R1.RW > R2.RX) and
    (R1.RY < R2.RY + R2.RH) and (R1.RY + R1.RH > R2.RY);
end;


function BoundRect(pos: RVec2; w: integer; h: integer): RRect;
begin
  Result.RX := pos.RX - (w div 2);
  Result.RY := pos.RX - (h div 2);
  Result.RW := w;
  Result.RH := h;
end;

{ ARect 에서 BRest에 노출을 Mapping 한다고 할 때, 출력할 수 있는
  ARect 의 범위를 구한다.
  ARect는 Entity의 Animation 이 노출될 영역이며,
  BRect는 Camera의 영역이다.
}
function CameraClippedRect(ARect: RRect; BRect: RRect): RRect;
var
  Overlapped: RVec2;
  AX: integer;
  AY: integer;
  BX: integer;
  BY: integer;
begin
  if RectContains(ARect, BRect) then
  begin
    { ARect 가 BRect 에 완전히 포함되었으므로, ARect 전체가 노출되어야한다. }
    Result.RX := 0;
    Result.RY := 0;
    Result.RW := ARect.RW;
    Result.RH := ARect.RH;
  end
  else if RectContains(BRect, ARect) then
  begin
    { BRect 가 ARect 에 포함된 상태이므로, 이 경우는 ARect 에서 BRect 에 해당하는
      영역만 노출되어야한다.
    }
    Result.RX := BRect.RX - ARect.RX;
    Result.RY := BRect.RY - ARect.Ry;
    Result.RW := BRect.RW;
    Result.RH := BRect.RH;
  end
  else
  begin
    { 서로 겹친 경우 겹친만큼만 사용한다.}
    Overlapped := OverlapAmount(ARect, BRect);
    AX := ARect.RX;
    AY := ARect.RY;
    BX := BRect.RX;
    BY := BRect.RY;

    if AX < BX then
      Result.RX := BX - AX
    else
      Result.Rx := 0;

    if AY < BY then
      Result.RY := BY - AY
    else
      Result.RY := 0;

    Result.RW := Overlapped.RX;
    Result.RY := Overlapped.RY;
  end;
end;

end.
