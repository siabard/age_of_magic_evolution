unit engine;

{$mode ObjFPC}{$H+}{$J-}

interface


uses
  Classes, SysUtils, Sdl2, asset_manager, entity, Components, LogUtil,
  textbox, Generics.Collections;

type
  TEngine = class
  private
    AWindow: PSDL_Window;
    ARenderer: PSDL_Renderer;
    Running: boolean;
    AAssetManager: TAssetManager;
    ATextBox: TTextBox;
    FEntities: specialize TList<TEntity>;
    AddedEntities: specialize TList<TEntity>;

  public
    constructor Create;
    destructor Destroy; override;
    procedure GameInit(ConfigPath: PChar);
    procedure GameLoop;
    procedure GameUpdate();
    procedure GameRender();
    procedure DebugEntities();
  end;


implementation

constructor TEngine.Create;
begin
  {---------------------------------------------------------------------------}
  { Window / Renderer 를 초기화하기                                           }
  {---------------------------------------------------------------------------}
  AWindow := SDL_CreateWindow('Age of Magic Engine', 0, 0, 640, 480,
    SDL_WINDOW_SHOWN);

  if AWindow = nil then
  begin
    LogDebug('ERROR::MAIN::Failed to Init Window');
    Exit;
  end;

  ARenderer := SDL_CreateRenderer(AWindow, -1, SDL_RENDERER_ACCELERATED or
    SDL_RENDERER_PRESENTVSYNC);
  if ARenderer = nil then
  begin
    LogDebug('ERROR::MAIN::Failed to Init Renderer');
    Exit;
  end;

  AAssetManager := TAssetManager.Create(ARenderer);
  ATextBox := TTextBox.Create(0, 0, 192, 32, 4, 4);
  Running := True;
  FEntities := specialize TList<TEntity>.Create;
  AddedEntities := specialize TList<TEntity>.Create;
end;


destructor TEngine.Destroy;
var
  AEntity: TEntity;
begin

  {---------------------------------------------------------------------------}
  { Gabbage Collection                                                        }
  {---------------------------------------------------------------------------}

  for AEntity in FEntities do
  begin
    LogDebug('Remove Entity');
    AEntity.Free;
  end;
  FEntities.Free;

  AAssetManager.Free;

  if Assigned(ARenderer) then
  begin
    LogDebug('Destroy Renderer');
    SDL_DestroyRenderer(ARenderer);
  end;

  if Assigned(AWindow) then
  begin
    LogDebug('Destroy Window');
    SDL_DestroyWindow(AWindow);
  end;

end;

{------------------------------------------------------------------------------}
{ 환경설정 파일 읽어 들이기                                                    }
{------------------------------------------------------------------------------}
procedure TEngine.GameInit(ConfigPath: PChar);
var
  configFile: TextFile;
  config: string;
  Fields: TStringList;
  AEntity: TEntity;
  ACompPosition: TPositionComponent;
begin
  Fields := TStringList.Create;

  AssignFile(configFile, ConfigPath);

  try
    Reset(configFile);
    try
      while not EOF(configFile) do
      begin
        ReadLn(configFile, config);

        // Config를 Split 한다.
        Fields.DelimitedText := config;

        if Fields[0] = 'texture' then
        begin
          // Texture Loading
          AAssetManager.LoadTexture(Fields[1], pansichar(Fields[2]));
        end;
      end;

    except
      on E: EInOutError do
      begin

        LogDebug('Failed Open File');

      end;
    end;

    // 텍스트박스 폰트 설정
    ATextBox.korFontTexture := AAssetManager.GetTexture('hangul');
    ATextBox.engFontTexture := AAssetManager.GetTexture('ascii');
    ATextBox.boxTexture := AAssetManager.GetTexture('panel');

    // 새로운 Entity 추가하기
    AEntity := TEntity.Create;
    ACompPosition := TPositionComponent.Create('position');
    ACompPosition.X := 60;
    ACompPosition.Y := 80;
    AEntity.position := ACompPosition;
    AddedEntities.Add(AEntity);

  finally
    CloseFile(configFile);
  end;

end;

procedure TEngine.GameLoop;
var
  sdlEvents: PSDL_Event;
  AEntity: TEntity;
  CompPosition: TPositionComponent;
begin

  {---------------------------------------------------------------------------}
  { 이벤트 루프 +  화면 출력                                                  }
  {---------------------------------------------------------------------------}
  LogDebug('Show Screen');
  while Running = True do
  begin

    new(sdlEvents);

    // Event Loop
    while SDL_PollEvent(sdlEvents) = 1 do
    begin

      case sdlEvents^.type_ of
        SDL_QUITEV: Running := False;
        SDL_KEYDOWN: begin
          case sdlEvents^.key.keysym.sym of
            SDLK_ESCAPE: Running := False;
            SDLK_i: begin
              // 임시로 새로운 엔터티를 넣기
              AEntity := TEntity.Create;
              CompPosition := TPositionComponent.Create('position');
              CompPosition.X := 11;
              CompPosition.Y := 12;
              AEntity.position := CompPosition;
              AddedEntities.Add(AEntity);
            end;

          end;
        end;
      end;
    end;

    GameUpdate();
    GameRender();


    SDL_Delay(20);

  end;
  Dispose(sdlEvents);
end;


procedure TEngine.GameUpdate();
var
  AEntity: TEntity;
begin
  // 추가된 항목을 모두 더한다.
  for AEntity in AddedEntities do
  begin
    FEntities.Add(AEntity);
  end;

  AddedEntities.Clear;

end;

procedure TEngine.GameRender();
var
  itemTexture: PSDL_Texture;
begin

  SDL_RenderClear(ARenderer);

  itemTexture := AAssetManager.GetTexture('items');
  if itemTexture <> nil then
    SDL_RenderCopy(ARenderer, itemTexture, nil, nil);

  ATextBox.DrawPanel(ARenderer);
  ATextBox.DrawString(ARenderer,
    'ABCDEFG ijkl 123 가각단댕. 세상은 더 이상 커질 수 없을 정도로 커진다. 텍스트의 크기도 마찬가지. 점점 커진다.');
  SDL_RenderPresent(ARenderer);

  DebugEntities;
end;

procedure TEngine.DebugEntities();
var
  AEntity: TEntity;
begin
  for AEntity in FEntities do
  begin
    WriteLn(' X : ', AEntity.position.X, ' , Y : ', AEntity.position.Y);
  end;
end;

end.
