unit scene_shooter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, asset_manager, sdl2, KeyInput, scene_map;

type
  { Scene Shooter }

  { TSceneShooter }

  TSceneShooter = class(TSceneMap)
  public
    constructor Create(AM: TAssetManager; AR: PSDL_Renderer; AK: TKeyInput);
    destructor Destroy; override;
    procedure SceneUpdate(dt: real);
    procedure SceneRender;
  end;

implementation

{ TSceneShooter }

constructor TSceneShooter.Create(AM: TAssetManager; AR: PSDL_Renderer;
  AK: TKeyInput);
begin
  inherited;
end;

destructor TSceneShooter.Destroy;
begin
  inherited Destroy;
end;

procedure TSceneShooter.SceneUpdate(dt: real);
var
  FPS: integer;
begin
  inherited;

  if dt = 0 then
    FPS := 0
  else
    FPS := 1000 div Trunc(dt * 1000);
  FTextBox.Text := Format('DT : %8d%sFPS: %8d', [Trunc(dt * 1000), sLineBreak, FPS]);


end;

procedure TSceneShooter.SceneRender;
begin
  SDL_RenderClear(FRenderer);

  Self.RenderSystem;

  { 기타 정보창은 모든 렌더링 이후에 진행 }
  FTextBox.DrawPanel(FRenderer);
  FTextBox.Draw(FRenderer);


  SDL_RenderPresent(FRenderer);
end;

end.
