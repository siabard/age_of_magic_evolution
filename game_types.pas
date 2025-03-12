unit game_types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, sdl2;

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

function RectToSdl2Rect(ARect: RRect): TSDL_Rect;

function RectCenter(ARect: RRect): RVec2;

procedure DebugRect(Tag: string; ARect: RRect);

procedure DebugVec2(Tag: string; AVec2: RVec2);

implementation

function RectToSdl2Rect(ARect: RRect): TSDL_Rect;
begin
  Result.x := ARect.RX;
  Result.y := ARect.RY;
  Result.w := ARect.RW;
  Result.h := ARect.RH;
end;

function RectCenter(ARect: RRect): RVec2;
begin
  Result.RX := ARect.RX + (ARect.RW div 2);
  Result.RY := ARect.RX + (ARect.RH div 2);
end;

procedure DebugRect(Tag: string; ARect: RRect);
begin
  WriteLn(Format('%s: %d %d %d %d', [Tag, ARect.RX, ARect.RY, ARect.RW, ARect.RH]));
end;

procedure DebugVec2(Tag: string; AVec2: RVec2);
begin
  WriteLn(Format('%s: %d %d', [Tag, AVec2.RX, AVec2.RY]));
end;

end.
