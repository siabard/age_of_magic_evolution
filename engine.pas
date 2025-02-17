unit engine;

{$mode ObjFPC}{$H+}{$J-}

interface


uses
  Classes, SysUtils, Sdl2, asset_manager, entity, component, LogUtil,
  textbox, Generics.Collections, scene, KeyInput, scene_map;

type
  TEngine = class
  private
    AWindow: PSDL_Window;
    ARenderer: PSDL_Renderer;
    Running: boolean;
    AAssetManager: TAssetManager;
    AScenes: specialize TList<TScene>;
    AKeyInput: TKeyInput;

  public
    constructor Create;
    destructor Destroy; override;
    procedure GameInit(ConfigPath: pchar);
    procedure GameLoop;
    procedure GameUpdate(dt: real);
    procedure GameRender();
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

  AKeyInput := TKeyInput.Create;

  { Scene 리스트를 생성 }
  AScenes := specialize TList<TScene>.Create;
  Running := True;
end;


destructor TEngine.Destroy;
var
  tmpScene: TScene;
begin
  {---------------------------------------------------------------------------}
  { Remove All Scenes                                                         }
  {---------------------------------------------------------------------------}
  for tmpScene in AScenes do
  begin
    tmpScene.Free;
  end;
  AScenes.Free;

  {---------------------------------------------------------------------------}
  { Gabbage Collection                                                        }
  {---------------------------------------------------------------------------}

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

  FreeAndNil(AKeyInput);

end;

{------------------------------------------------------------------------------}
{ 환경설정 파일 읽어 들이기                                                    }
{------------------------------------------------------------------------------}
procedure TEngine.GameInit(ConfigPath: pchar);
var
  configFile: TextFile;
  config: string;
  Fields: TStringList;
  AScene: TScene;
begin
  Fields := TStringList.Create;
  Fields.Delimiter := #9; // 탭 문자
  Fields.StrictDelimiter := False; // 여러 구분자 허용

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

    AScene := TSceneMap.Create(AAssetManager, ARenderer, AKeyInput);
    AScenes.Add(AScene);
  finally
    CloseFile(configFile);
  end;

end;

procedure TEngine.GameLoop;
var
  sdlEvents: PSDL_Event;
  CurrentTime: uint32;
  LastTime: uint32;
  DeltaTime: uint32;
  dt: real;
begin

  {---------------------------------------------------------------------------}
  { 이벤트 루프 +  화면 출력                                                  }
  {---------------------------------------------------------------------------}
  LogDebug('Show Screen');

  LastTime := SDL_GetTicks;

  AKeyInput.InitKeys;

  new(sdlEvents);
  while Running = True do
  begin

    // Event Loop
    while SDL_PollEvent(sdlEvents) = 1 do
    begin

      case sdlEvents^.type_ of
        SDL_QUITEV: Running := False;
        SDL_KEYDOWN: begin
          case sdlEvents^.key.keysym.sym of
            SDLK_ESCAPE: Running := False;
            else
              AKeyInput.KeyDownEvent(sdlEvents^.key.keysym.sym);
          end;
        end;
        SDL_KEYUP: begin
          AKeyInput.KeyDownEvent(sdlEvents^.key.keysym.sym);
        end;
      end;
    end;

    // 프레임 딜레이
    CurrentTime := SDL_GetTicks;
    DeltaTime := CurrentTime - LastTime;
    LastTime := CurrentTime;
    dt := DeltaTime / 1000.0;

    GameUpdate(dt);

    GameRender();

    if DeltaTime < 16 then
    begin
      SDL_Delay(16 - DeltaTime);
    end;

  end;
  Dispose(sdlEvents);
end;


procedure TEngine.GameUpdate(dt: real);
var
  AScene: TScene;
begin
  for AScene in AScenes do
  begin
    if AScene is TSceneMap then
      TSceneMap(AScene).SceneUpdate(dt)
    else
      Ascene.SceneUpdate(dt);

  end;

end;

procedure TEngine.GameRender();
var
  AScene: TScene;
begin
  for AScene in AScenes do
  begin
    if AScene is TSceneMap then
      TSceneMap(AScene).SceneRender
    else
      AScene.SceneRender;

  end;
end;


end.
