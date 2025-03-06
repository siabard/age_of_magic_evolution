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

implementation

function RectToSdl2Rect(ARect: RRect): TSDL_Rect;
begin
  Result.x := ARect.RX;
  Result.y := ARect.RY;
  Result.w := ARect.RW;
  Result.h := ARect.RH;
end;

end.
