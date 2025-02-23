unit atlas;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, game_types;

type
  TAtlas = class
  private
    FTextureName: String;
    FRects: specialize TList<RRect>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure MakeAtlas(texture_width: integer; texture_height: integer;
      tile_width: integer; tile_height: integer);
    property Rects: specialize TList<RRect> read FRects write FRects;
    property TextureName: String read FTextureName write FTextureName;
  end;

implementation

constructor TAtlas.Create;
begin
  FRects := specialize TList<RRect>.Create;
end;

destructor TAtlas.Destroy;
begin
  FRects.Free;
end;

procedure TAtlas.MakeAtlas(texture_width: integer; texture_height: integer;
  tile_width: integer; tile_height: integer);
var
  Rows: integer;
  Cols: integer;
  I, J: integer;
  ARect: RRect;
begin
  Rows := texture_height div tile_height;
  Cols := texture_width div tile_width;

  for I := 0 to Rows - 1 do
  begin
    for J := 0 to Cols - 1 do
    begin
      ARect.RX := J * tile_width;
      ARect.RY := I * tile_height;
      ARect.RW := tile_width;
      ARect.RH := tile_height;
      FRects.Add(ARect);
    end;
  end;

end;

end.
