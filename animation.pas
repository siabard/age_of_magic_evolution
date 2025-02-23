unit animation;

{$mode ObjFPC}{$H+}

interface


uses
  SysUtils, Generics.Collections, game_types;

type
  TAnimation = class
  private
    FName: string;
    FTextureName: string;
    FFrameRects: specialize TList<RRect>;
  public
    constructor Create(const AName: string; ATextureName: string;
      AStartFrame: integer; AFrameLength: integer;
      FrameRects: specialize TList<RRect>);
    destructor Destroy; override;
    property Name: string read FName write FName;
    property TextureName: string read FTextureName write FTextureName;
    function GetFrames: specialize TList<RRect>;
    function GetFrameLength: integer;
    procedure PrintAnimationInfo;
    procedure SetFrame(FrameStart: integer; FrameEnd: integer;
      FrameRects: specialize TList<RRect>);
  end;

implementation

uses
  LogUtil;
constructor TAnimation.Create(const AName: string; ATextureName: string;
  AStartFrame: integer; AFrameLength: integer; FrameRects: specialize TList<RRect>);
var
  I, LastFrame: integer;
begin
  FName := AName;
  FTextureName := ATextureName;
  FFrameRects := specialize TList<RRect>.Create;

  LastFrame := AStartFrame + AFrameLength - 1;
  for I := AStartFrame to LastFrame do
  begin
    WriteLn(' Frame ', I);
    FFrameRects.Add(FrameRects[I]);
  end;
end;

destructor TAnimation.Destroy;
begin
  LogDebug('TAnimation.Destroy');
  FFrameRects.Free;
  inherited Destroy;
end;


function TAnimation.GetFrames: specialize TList<RRect>;
begin
  Result := FFrameRects;
end;

function TAnimation.GetFrameLength: integer;
begin
  Result := FFrameRects.Count;
end;

procedure TAnimation.PrintAnimationInfo;
var
  Frame: RRect;
begin
  WriteLn('Animation Name: ', FName);
  WriteLn('Texture Name: ', FTextureName);
  Write('Frames: ');
  for Frame in FFrameRects do
    Write(Frame.RX, Frame.RY, Frame.RW, Frame.RH);
  WriteLn;
end;

procedure TAnimation.SetFrame(FrameStart: integer; FrameEnd: integer;
  FrameRects: specialize TList<RRect>);
var
  I: integer;
begin
  FFrameRects.Clear;
  for I := FrameStart to FrameEnd do
  begin
    FFrameRects.Add(FrameRects[I]);
  end;
end;

end.
