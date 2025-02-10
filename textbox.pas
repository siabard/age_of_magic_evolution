unit textbox;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, sdl2, hangul;

type

  TTextBox = class
  private
    BX: integer;
    BY: integer;
    BW: integer;
    BH: integer;
    ABoxTexture: PSDL_Texture;
    AKorFontTexture: PSDL_Texture;
    AEngFontTexture: PSDL_Texture;
    procedure SetBoxTexture(texture: PSDL_Texture);
    procedure SetKorFontTexture(texture: PSDL_Texture);
    procedure SetEngFontTexture(texture: PSDL_Texture);
  public
    constructor Create(x: integer; y: integer; w: integer; h: integer);
    destructor Destroy; override;
    property boxTexture: PSDL_Texture read ABoxTexture write SetBoxTexture;
    property korFontTexture: PSDL_Texture read AKorFontTexture write SetKorFontTexture;
    property engFontTexture: PSDL_Texture read AEngFontTexture write SetEngFontTexture;
    procedure DrawAsciiCharacter(renderer: PSDL_Renderer; ox: integer;
      oy: integer; AsciiWord: word);
    procedure DrawHangulCharacter(renderer: PSDL_Renderer; ox: integer;
      oy: integer; AsciiWord: word);
    procedure DrawString(renderer: PSDL_Renderer; px: integer;
      py: integer; Src: string);

  end;

implementation


constructor TTextBox.Create(x: integer; y: integer; w: integer; h: integer);
begin
  BX := x;
  BY := y;
  BW := w;
  BH := h;
end;

destructor TTextBox.Destroy;
begin
  // Do Nothing
end;

procedure TTextBox.SetBoxTexture(texture: PSDL_Texture);
begin
  Self.ABoxTexture := texture;

end;

procedure TTextBox.SetKorFontTexture(texture: PSDL_Texture);
begin
  Self.AKorFontTexture := texture;
end;

procedure TTextBox.SetEngFontTexture(texture: PSDL_Texture);
begin
  Self.AEngFontTexture := texture;
end;

procedure TTextBox.DrawAsciiCharacter(renderer: PSDL_Renderer; ox: integer;
  oy: integer; AsciiWord: word);
var
  col: integer;
  row: integer;
  CharRect: TSDL_Rect;
  DestRect: TSDL_Rect;
begin
  // Ascii Texture 에서 위치를 찾는다.
  // 각 폰트 크기는 가로 8, 세로 16픽셀이다.
  row := integer(AsciiWord div 16);
  col := (AsciiWord - (row * 16));
  // 해당 글자를 지정한 x, y 위치에 Render Copy 한다.
  CharRect.x := col * 8;
  CharRect.y := row * 16;
  CharRect.w := 8;
  CharRect.h := 16;

  DestRect.x := ox;
  DestRect.y := oy;
  DestRect.w := 8;
  DestRect.h := 16;

  SDL_RenderCopy(renderer, AEngFontTexture, @CharRect, @DestRect);
end;

procedure TTextBox.DrawString(renderer: PSDL_Renderer; px: integer;
  py: integer; Src: string);
var
  WordArray: TUTF16Array;
  I: integer;
  Tx: integer;
  Ty: integer;
begin
  WordArray := utf8_to_ucs2(Src);

  Tx := BX + px; // TextBox의 X 좌표에 Padding 을 더해 시작
  Ty := BY + py; // TextBox의 Y 좌표에 Padding 을 더해 시작
  for I := Low(WordArray) to High(WordArray) do
  begin

    if get_language(WordArray[I]) = ascii then
    begin
      Self.DrawAsciiCharacter(renderer, Tx, Ty, WordArray[I]);
      Tx := Tx + 8;
    end
    else if get_language(WordArray[I]) = korean then
    begin
      Self.DrawHangulCharacter(renderer, Tx, Ty, WordArray[I]);
      Tx := Tx + 16;
    end;

  end;
end;

procedure TTextBox.DrawHangulCharacter(renderer: PSDL_Renderer;
  ox: integer; oy: integer; AsciiWord: word);
var
  AJaso: TJaso;
  ABul: TBul;
  CharRect: TSDL_Rect;
  DestRect: TSDL_Rect;
  fx: integer;
  fy: integer;
begin
  // 대상 워드의 자소와 벌을 가져온다.
  AJaso := buildJaso(AsciiWord);
  ABul := buildBul(Ajaso);

  // 기본적으로 한글은 가로 16, 세로 16 픽셀이다.
  // 초성/중성/종성에 대한 각각의 위치를 구한다.
  // 각위치는 각각의 값이 0이 아니라면 : 자소값 * 16, 벌값 * 16 이다.

  DestRect.x := ox;
  DestRect.y := oy;
  DestRect.w := 16;
  DestRect.h := 16;

  CharRect.w := 16;
  CharRect.h := 16;

  if (AJaso.cho > 0) and (ABul.cho > 0) then
  begin
    fx := (AJaso.cho - 1) * 16;
    fy := (ABul.cho - 1) * 16;
    CharRect.x := fx;
    CharRect.y := fy;

    SDL_RenderCopy(renderer, AKorFontTexture, @CharRect, @DestRect);
  end;

  if (AJaso.middle > 0) and (ABul.middle > 0) then
  begin
    fx := (AJaso.middle - 1) * 16;
    fy := (ABul.middle - 1) * 16 + 8 * 16;
    CharRect.x := fx;
    CharRect.y := fy;

    SDL_RenderCopy(renderer, AKorFontTexture, @CharRect, @DestRect);
  end;

  if (AJaso.jong > 0) and (ABul.jong > 0) then
  begin
    fx := AJaso.jong * 16;
    fy := (ABul.jong - 1) * 16 + 12 * 16;
    CharRect.x := fx;
    CharRect.y := fy;

    SDL_RenderCopy(renderer, AKorFontTexture, @CharRect, @DestRect);
  end;
end;

end.
