unit scene_map;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, asset_manager, sdl2, textbox, scene, KeyInput;

type


  TSceneMap = class(TScene)
  protected
    ATextBox: TTextBox;
  public
    constructor Create(AM: TAssetManager; AR: PSDL_Renderer; AK: TKeyInput);
    destructor Destroy; override;
    procedure SceneUpdate(dt: real);
    procedure SceneRender;

  end;

implementation

constructor TSceneMap.Create(AM: TAssetManager; AR: PSDL_Renderer; AK: TKeyInput);
begin
  inherited;

  ASceneType := map_scene;
  ATextBox := TTextBox.Create(0, 0, 192, 32, 4, 4);

  ATextBox.boxTexture := AAssetManager.GetTexture('panel');
  // 텍스트박스 폰트 설정
  ATextBox.korFontTexture := AAssetManager.GetTexture('hangul');
  ATextBox.engFontTexture := AAssetManager.GetTexture('ascii');

end;

destructor TSceneMap.Destroy;
begin
  ATextBox.Free;

  inherited;
end;

procedure TSceneMap.SceneUpdate(dt: real);
begin

  inherited;
  ATextBox.Text := Format('%4d', [Trunc(dt * 1000)]);

end;

procedure TSceneMap.SceneRender();
var
  itemTexture: PSDL_Texture;
begin
  SDL_RenderClear(ARenderer);

  itemTexture := AAssetManager.GetTexture('items');
  if itemTexture <> nil then
    SDL_RenderCopy(ARenderer, itemTexture, nil, nil);

  ATextBox.DrawPanel(ARenderer);

  ATextBox.Draw(ARenderer);
  SDL_RenderPresent(ARenderer);

end;

end.
