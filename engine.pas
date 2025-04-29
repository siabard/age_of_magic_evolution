unit engine;

{$mode ObjFPC}{$H+}{$J-}

interface


uses
  Classes, SysUtils, Sdl2, asset_manager, entity, component, LogUtil,
  textbox, Generics.Collections, scene, KeyInput, atlas,
  animation, game_types, story_engine;

type

  { TEngine }

  TSceneDictionary = specialize TDictionary<string, TScene>;

  TEngine = class
  private
    AWindow: PSDL_Window;
    ARenderer: PSDL_Renderer;
    Running: boolean;
    AAssetManager: TAssetManager;
    FScenes: TSceneDictionary;
    FCurrentSceneName: string;
    AKeyInput: TKeyInput;

  public
    constructor Create;
    destructor Destroy; override;
    procedure GameInit(ConfigPath: PChar);
    procedure GameLoop;
    procedure GameUpdate(dt: real);
    procedure GameRender();
  end;


implementation

uses StrUtils, scene_map, scene_shooter;

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
  FScenes := TSceneDictionary.Create;
  Running := True;
end;


destructor TEngine.Destroy;
var
  tmpScene: TScene;
begin
  {---------------------------------------------------------------------------}
  { Remove All Scenes                                                         }
  {---------------------------------------------------------------------------}
  for tmpScene in FScenes.Values do
  begin
    tmpScene.Free;
  end;
  FScenes.Free;

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
procedure TEngine.GameInit(ConfigPath: PChar);
var
  configFile: TextFile;
  config: string;
  AScene: TScene;
  Fields: TStringList;
  tile_width: integer;
  tile_height: integer;
  code: integer;
  itemsAsset: TAtlas;
  I: integer;
  AnimationName: string;
  AtlasName: string;
  AAnimation: TAnimation;
  AAtlas: TAtlas;
  FrameStart: integer;
  FrameSize: integer;
  SwordAnimation: TAnimation;
  SwordFrame: specialize TList<RRect>;
begin
  Fields := TStringList.Create;
  Fields.Delimiter := #9; // 탭 문자
  Fields.StrictDelimiter := False; // 여러 구분자 허용

  LogDebug(Format('TEngine.GameInit:: %s - request open', [ConfigPath]));
  AssignFile(configFile, ConfigPath);

  try
    Reset(configFile);
    try
      while not EOF(configFile) do
      begin
        ReadLn(configFile, config);

        if IsEmptyStr(config, []) then
          continue;

        // Config를 Split 한다.
        Fields.DelimitedText := config;

        case Fields[0] of
          'texture': begin
            // Texture Loading
            AAssetManager.LoadTexture(Fields[1], pansichar(Fields[2]));
          end;
          'atlas': begin
            // atlas making
            // atlas [atlas name] [texture name] [tile-width] [tile-height]
            Val(Fields[3], tile_width, code);
            Val(Fields[4], tile_height, code);
            AAssetManager.AddAtlas(Fields[1], Fields[2], tile_width, tile_height);
          end;
          'animation': begin
            { 주어진 animation 이름에 해당하는 Frame의 Rect를 얻는다.
              해당하는 animation의 Frame이 들어가있는 Rect의 List는
              Atlas에 있다.
            }
            // animation [animation name] [atlas name] [start frame] [frame size]
            // animation sword items 1 2
            AnimationName := Fields[1];
            AtlasName := Fields[2];
            Val(Fields[3], FrameStart, Code);
            Val(Fields[4], FrameSize, Code);
            AAtlas := AAssetManager.GetAtlas(AtlasName);
            AAnimation := TAnimation.Create(AnimationName, AAtlas.TextureName,
              FrameStart, FrameSize, AAtlas.Rects);
            AAssetManager.AddAnimation(AnimationName, AAnimation);
          end;
          'scene': begin
            { Scene 생성 처리 }
            if Fields[1] = 'scene_map' then
              AScene := TSceneMap.Create(AAssetManager, ARenderer, AKeyInput)
            else if Fields[1] = 'scene_shooter' then
              AScene := TSceneShooter.Create(AAssetManager, ARenderer, AKeyInput)
            else
              AScene := TScene.Create(AAssetManager, ARenderer, AKeyInput);

            AScene.SceneInit(Fields[3]);

            { Scene 의 리소스 생성 (초기 엔터티등) }
            FScenes.Add(Fields[2], AScene);
          end;
          'current_scene': begin
            FCurrentSceneName := Fields[1];
          end;
        end;
      end;

    except
      on E: EInOutError do
      begin

        LogDebug('Failed Open File');

      end;
    end;

    // 디버그 Atlas
    {
    itemsAsset := AAssetManager.GetAtlas('items');
    for I := 0 to itemsAsset.Rects.Count - 1 do
    begin
      Write(' RX: ', itemsAsset.Rects[i].RX);
      Write(' RY: ', itemsAsset.Rects[i].RY);
      Write(' RW: ', itemsAsset.Rects[i].RW);
      Write(' RH: ', itemsAsset.Rects[i].RH);
      WriteLn();
    end;
    }
    // 디버그 Animation
    {
    SwordAnimation := AAssetManager.GetAnimation('sword');
    SwordFrame := SwordAnimation.GetFrames;
    WriteLn(' sword animation ');
    WriteLn(' texture name : ', SwordAnimation.TextureName);

    WriteLn( ' Frame Size ' , SwordFrame.Count);
    for I := 0 to SwordFrame.Count - 1 do
    begin
      Write(' X: ', SwordFrame[I].RX);
      Write(' Y: ', SwordFrame[I].RY);
      Write(' W: ', SwordFrame[I].RW);
      Write(' H: ', SwordFrame[I].RH);
      WriteLn();

    end;
    }

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
  AScene: TScene;
begin

  {---------------------------------------------------------------------------}
  { 이벤트 루프 +  화면 출력                                                  }
  {---------------------------------------------------------------------------}
  LogDebug('Show Screen');

  LastTime := SDL_GetTicks;

  AKeyInput.InitKeys;

  new(sdlEvents);

  FScenes.TryGetValue(FCurrentSceneName, AScene);
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
            begin
              AKeyInput.KeyDownEvent(sdlEvents^.key.keysym.sym);
              AScene.DoAction(sdlEvents^.key.keysym.sym, action_start);
            end;
          end;
        end;
        SDL_KEYUP: begin
          AKeyInput.KeyDownEvent(sdlEvents^.key.keysym.sym);
          AScene.DoAction(sdlEvents^.key.keysym.sym, action_stop);

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
  if FScenes.TryGetValue(FCurrentSceneName, AScene) then
    if AScene is TSceneShooter then
      TSceneShooter(AScene).SceneUpdate(dt)
    else if AScene is TSceneMap then
      TSceneMap(AScene).SceneUpdate(dt)
    else
      Ascene.SceneUpdate(dt);

end;

procedure TEngine.GameRender();
var
  AScene: TScene;
begin
  if FScenes.TryGetValue(FCurrentSceneName, AScene) then
    if AScene is TSceneShooter then
      TSceneShooter(AScene).SceneRender
    else if AScene is TSceneMap then
      TSceneMap(AScene).SceneRender
    else
      AScene.SceneRender;

end;


end.
