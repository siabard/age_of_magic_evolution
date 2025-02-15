unit textbox;

{$mode ObjFPC}{$H+}

interface


uses
  Classes, SysUtils, sdl2, hangul, LogUtil;

type

  TTextBox = class
  private
    BX: integer;
    BY: integer;
    BW: integer;
    BH: integer;
    PX: integer;
    PY: integer;
    ABoxTexture: PSDL_Texture;
    AKorFontTexture: PSDL_Texture;
    AEngFontTexture: PSDL_Texture;
    FText: string;
    procedure SetBoxTexture(texture: PSDL_Texture);
    procedure SetKorFontTexture(texture: PSDL_Texture);
    procedure SetEngFontTexture(texture: PSDL_Texture);
    procedure SetText(RText: string);
  public
    constructor Create(x: integer; y: integer; w: integer; h: integer;
      padding_x: integer; padding_y: integer);
    destructor Destroy; override;
    property boxTexture: PSDL_Texture read ABoxTexture write SetBoxTexture;
    property korFontTexture: PSDL_Texture read AKorFontTexture write SetKorFontTexture;
    property engFontTexture: PSDL_Texture read AEngFontTexture write SetEngFontTexture;
    property Text: string read FText write SetText;
    procedure DrawAsciiCharacter(renderer: PSDL_Renderer; Tx: integer;
      Ty: integer; AsciiWord: word);
    procedure DrawHangulCharacter(renderer: PSDL_Renderer; Tx: integer;
      Ty: integer; AsciiWord: word);
    procedure DrawString(renderer: PSDL_Renderer; Src: string);
    procedure Draw(renderer: PSDL_Renderer);
    procedure DrawPanel(renderer: PSDL_Renderer);


  end;

const
  ASCII_FONT_HEIGHT: integer = 16;
  ASCII_FONT_WIDTH: integer = 8;
  HANGUL_FONT_HEIGHT: integer = 16;
  HANGUL_FONT_WIDTH: integer = 16;
  ASCII_FONT_COLS: integer = 16;
  PANEL_PART_SIZE: integer = 3;

implementation


constructor TTextBox.Create(x: integer; y: integer; w: integer; h: integer;
  padding_x: integer; padding_y: integer);
begin
  BX := x;
  BY := y;
  BW := w;
  BH := h;
  PX := padding_x;
  PY := padding_y;
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

procedure TTextBox.DrawAsciiCharacter(renderer: PSDL_Renderer; Tx: integer;
  Ty: integer; AsciiWord: word);
var
  col: integer;
  row: integer;
  CharRect: TSDL_Rect;
  DestRect: TSDL_Rect;
begin
  // Ascii Texture 에서 위치를 찾는다.
  // 각 폰트 크기는 가로 8, 세로 16픽셀이다.
  row := integer(AsciiWord div ASCII_FONT_COLS);
  col := AsciiWord - (row * ASCII_FONT_COLS);
  // 해당 글자를 지정한 x, y 위치에 Render Copy 한다.
  CharRect.x := col * ASCII_FONT_WIDTH;
  CharRect.y := row * ASCII_FONT_HEIGHT;
  CharRect.w := ASCII_FONT_WIDTH;
  CharRect.h := ASCII_FONT_HEIGHT;

  DestRect.x := Tx;
  DestRect.y := Ty;
  DestRect.w := ASCII_FONT_WIDTH;
  DestRect.h := ASCII_FONT_HEIGHT;

  SDL_RenderCopy(renderer, AEngFontTexture, @CharRect, @DestRect);
end;

procedure TTextBox.DrawString(renderer: PSDL_Renderer; Src: string);
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

    if Ty <= (Self.BY + Self.BH - 2 * PANEL_PART_SIZE) then
    begin
      if get_language(WordArray[I]) = ascii then
      begin
        Self.DrawAsciiCharacter(renderer, Tx, Ty, WordArray[I]);
        Tx := Tx + ASCII_FONT_WIDTH;
      end
      else if get_language(WordArray[I]) = korean then
      begin
        Self.DrawHangulCharacter(renderer, Tx, Ty, WordArray[I]);
        Tx := Tx + HANGUL_FONT_WIDTH;
      end;
      if Tx > (Self.BX + Self.BW - 2 * PANEL_PART_SIZE) then
      begin
        Tx := Self.Bx + Self.Px;
        Ty := Ty + ASCII_FONT_HEIGHT;
      end;
    end;
  end;

end;

procedure TTextBox.Draw(renderer: PSDL_Renderer);
begin
  DrawString(renderer, FText);
end;

procedure TTextBox.DrawHangulCharacter(renderer: PSDL_Renderer;
  Tx: integer; Ty: integer; AsciiWord: word);
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

  DestRect.x := Tx;
  DestRect.y := Ty;
  DestRect.w := HANGUL_FONT_WIDTH;
  DestRect.h := HANGUL_FONT_HEIGHT;

  CharRect.w := HANGUL_FONT_WIDTH;
  CharRect.h := HANGUL_FONT_HEIGHT;

  if (AJaso.cho > 0) and (ABul.cho > 0) then
  begin
    fx := (AJaso.cho - 1) * HANGUL_FONT_WIDTH;
    fy := (ABul.cho - 1) * HANGUL_FONT_HEIGHT;
    CharRect.x := fx;
    CharRect.y := fy;

    SDL_RenderCopy(renderer, AKorFontTexture, @CharRect, @DestRect);
  end;

  if (AJaso.middle > 0) and (ABul.middle > 0) then
  begin
    // 중성의 세로는 초성 8벌이 끝난 뒤부터 시작한다.
    fx := (AJaso.middle - 1) * HANGUL_FONT_WIDTH;
    fy := (ABul.middle - 1) * HANGUL_FONT_HEIGHT + 8 * HANGUL_FONT_HEIGHT;
    CharRect.x := fx;
    CharRect.y := fy;

    SDL_RenderCopy(renderer, AKorFontTexture, @CharRect, @DestRect);
  end;

  if (AJaso.jong > 0) and (ABul.jong > 0) then
  begin
    // 종성의 세로는 초성 8벌, 중성 4벌이 시작된 후부터 시작한다.
    // 종성의 첫번째(1)은 비어있다.
    fx := AJaso.jong * HANGUL_FONT_WIDTH;
    fy := (ABul.jong - 1) * HANGUL_FONT_HEIGHT + 12 * HANGUL_FONT_HEIGHT;
    CharRect.x := fx;
    CharRect.y := fy;

    SDL_RenderCopy(renderer, AKorFontTexture, @CharRect, @DestRect);
  end;
end;

procedure TTextBox.SetText(RText: string);
begin
  FText := RText;
end;

procedure TTextBox.DrawPanel(renderer: PSDL_Renderer);
var
  CharRect: TSDL_Rect;
  DestRect: TSDL_Rect;
  PanelWidth: integer;
  PanelHeight: integer;
begin
  // 모서리를 제외한 너비와 넓이를 구한다.
  PanelWidth := Self.BW - PANEL_PART_SIZE * 2;
  PanelHeight := Self.BH - PANEL_PART_SIZE * 2;

  if (PanelWidth > 0) and (PanelHeight > 0) then
  begin
    // 왼쪽 위 모서리
    DestRect.x := Self.BX;
    DestRect.y := Self.BY;
    DestRect.w := PANEL_PART_SIZE;
    DestRect.h := PANEL_PART_SIZE;

    CharRect.x := 0;
    CharRect.y := 0;
    CharRect.w := PANEL_PART_SIZE;
    CharRect.h := PANEL_PART_SIZE;
    SDL_RenderCopy(renderer, ABoxTexture, @CharRect, @DestRect);

    // 윗변
    DestRect.x := Self.BX + PANEL_PART_SIZE;
    DestRect.y := Self.BY;
    DestRect.w := PanelWidth;
    DestRect.h := PANEL_PART_SIZE;

    CharRect.x := PANEL_PART_SIZE;
    CharRect.y := 0;
    CharRect.w := PANEL_PART_SIZE;
    CharRect.h := PANEL_PART_SIZE;
    SDL_RenderCopy(renderer, ABoxTexture, @CharRect, @DestRect);

    // 오른쪽 위 모서리
    DestRect.x := Self.BX + PANEL_PART_SIZE + PanelWidth;
    DestRect.y := Self.BY;
    DestRect.w := PANEL_PART_SIZE;
    DestRect.h := PANEL_PART_SIZE;

    CharRect.x := PANEL_PART_SIZE * 2;
    CharRect.y := 0;
    CharRect.w := PANEL_PART_SIZE;
    CharRect.h := PANEL_PART_SIZE;
    SDL_RenderCopy(renderer, ABoxTexture, @CharRect, @DestRect);

    // 왼쪽 변
    DestRect.x := Self.BX;
    DestRect.y := Self.BY + PANEL_PART_SIZE;
    DestRect.w := PANEL_PART_SIZE;
    DestRect.h := PanelHeight;

    CharRect.x := 0;
    CharRect.y := PANEL_PART_SIZE;
    CharRect.w := PANEL_PART_SIZE;
    CharRect.h := PANEL_PART_SIZE;
    SDL_RenderCopy(renderer, ABoxTexture, @CharRect, @DestRect);

    // 가운데
    DestRect.x := Self.BX + PANEL_PART_SIZE;
    DestRect.y := Self.BY + PANEL_PART_SIZE;
    DestRect.w := PanelWidth;
    DestRect.h := PanelHeight;

    CharRect.x := PANEL_PART_SIZE;
    CharRect.y := PANEL_PART_SIZE;
    CharRect.w := PANEL_PART_SIZE;
    CharRect.h := PANEL_PART_SIZE;
    SDL_RenderCopy(renderer, ABoxTexture, @CharRect, @DestRect);

    // 오른쪽 변
    DestRect.x := Self.BX + PANEL_PART_SIZE + PanelWidth;
    DestRect.y := Self.BY + PANEL_PART_SIZE;
    DestRect.w := PANEL_PART_SIZE;
    DestRect.h := PanelHeight;

    CharRect.x := PANEL_PART_SIZE * 2;
    CharRect.y := PANEL_PART_SIZE;
    CharRect.w := PANEL_PART_SIZE;
    CharRect.h := PANEL_PART_SIZE;
    SDL_RenderCopy(renderer, ABoxTexture, @CharRect, @DestRect);

    // 왼쪽 아래 모서리
    DestRect.x := Self.BX;
    DestRect.y := Self.BY + PANEL_PART_SIZE + PanelHeight;
    DestRect.w := PANEL_PART_SIZE;
    DestRect.h := PANEL_PART_SIZE;

    CharRect.x := 0;
    CharRect.y := PANEL_PART_SIZE * 2;
    CharRect.w := PANEL_PART_SIZE;
    CharRect.h := PANEL_PART_SIZE;
    SDL_RenderCopy(renderer, ABoxTexture, @CharRect, @DestRect);

    // 가운데
    DestRect.x := Self.BX + PANEL_PART_SIZE;
    DestRect.y := Self.BY + PANEL_PART_SIZE + PanelHeight;
    DestRect.w := PanelWidth;
    DestRect.h := PANEL_PART_SIZE;

    CharRect.x := PANEL_PART_SIZE;
    CharRect.y := PANEL_PART_SIZE * 2;
    CharRect.w := PANEL_PART_SIZE;
    CharRect.h := PANEL_PART_SIZE;
    SDL_RenderCopy(renderer, ABoxTexture, @CharRect, @DestRect);

    // 오른쪽 변
    DestRect.x := Self.BX + PANEL_PART_SIZE + PanelWidth;
    DestRect.y := Self.BY + PANEL_PART_SIZE + PanelHeight;
    DestRect.w := PANEL_PART_SIZE;
    DestRect.h := PANEL_PART_SIZE;

    CharRect.x := PANEL_PART_SIZE * 2;
    CharRect.y := PANEL_PART_SIZE * 2;
    CharRect.w := PANEL_PART_SIZE;
    CharRect.h := PANEL_PART_SIZE;
    SDL_RenderCopy(renderer, ABoxTexture, @CharRect, @DestRect);
  end;
end;

end.
