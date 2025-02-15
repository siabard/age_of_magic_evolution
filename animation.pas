unit animation;

{$mode ObjFPC}{$H+}

interface


uses
  SysUtils, Generics.Collections;

type
  TAnimation = class
  private
    FName: string;
    FTextureName: string;
    FFrames: specialize TList<integer>;
    FFrameLength: integer;
  public
    constructor Create(const AName, ATextureName: string;
      AStartFrame, AFrameLength: integer);
    destructor Destroy; override;
    property Name: string read FName write FName;
    property TextureName: string read FTextureName write FTextureName;
    function GetFrames: specialize TList<integer>;
    function GetFrameLength: integer;
    procedure PrintAnimationInfo;
    procedure SetFrame(FrameStart: integer; FrameEnd: integer);
  end;

implementation



constructor TAnimation.Create(const AName, ATextureName: string;
  AStartFrame, AFrameLength: integer);
var
  I, LastFrame: integer;
begin
  FName := AName;
  FTextureName := ATextureName;
  FFrameLength := AFrameLength;
  FFrames := specialize TList<integer>.Create;

  LastFrame := AStartFrame + AFrameLength - 1;
  for I := AStartFrame to LastFrame - 1 do
    FFrames.Add(I);
end;

destructor TAnimation.Destroy;
begin
  FFrames.Free;
  inherited Destroy;
end;


function TAnimation.GetFrames: specialize TList<integer>;
begin
  Result := FFrames;
end;

function TAnimation.GetFrameLength: integer;
begin
  Result := FFrameLength;
end;

procedure TAnimation.PrintAnimationInfo;
var
  Frame: integer;
begin
  WriteLn('Animation Name: ', FName);
  WriteLn('Texture Name: ', FTextureName);
  Write('Frames: ');
  for Frame in FFrames do
    Write(Frame, ' ');
  WriteLn;
  WriteLn('Frame Length: ', FFrameLength);
end;

procedure TAnimation.SetFrame(FrameStart: integer; FrameEnd: integer);
var
  I: integer;
begin
  FFrameLength :=
    FrameEnd - FrameStart + 1;
  FFrames.Clear;
  for I := FrameStart to FrameEnd do
  begin
    FFrames.Add(I);
  end;
end;

end.
